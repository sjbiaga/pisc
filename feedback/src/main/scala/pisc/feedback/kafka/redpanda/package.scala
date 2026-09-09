package pisc
package feedback
package kafka

import scala.scalajs.js

import cats.effect.IO

import io.circe.Codec
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.{ Headers, MediaType, Method, Request, Uri }
import org.http4s.headers.{ Accept, `Content-Type` }
import org.http4s.client.Client

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*


package object redpanda:

  private val mediaType = new MediaType("application", "vnd.kafka.json.v2+json")
  private val headers = Headers(Accept(mediaType), `Content-Type`(mediaType))

  case class Key(label: String) derives Codec.AsObject
  case class Value(pid: Long,
                   number: Long, clock: Double, started: Long, ended: Long,
                   agent: String, name: String, polarity: Option[Boolean],
                   key: String, guard: Boolean, label: String,
                   rate: String, delay: Double, duration: Option[Double],
                   dir_cap: Option[String], from: Option[String], to: Option[String],
                   snapshot: Option[String]) derives Codec.AsObject

  case class JsonKafkaRecord(topic: String, key: Key, value: Value, partition: Int, offset: Long) extends AbstractKafkaRecord[Key, Value] derives Codec.AsObject

  object JsonKafkaRecord:

    given Reusability[JsonKafkaRecord] = Reusability.by_==


  case class Redpanda(proxyUrl: String,
                      topic: String,
                      offset: Long,
                      maxBytes: Int,
                      timeout: Int,
                      groupId: String = "feedback-json-group",
                      instanceName: String = s"feedback-json-instance-${System.nanoTime}")

  case class Props(item_id: String,
                   isBioAmbients: Boolean,
                   pid: Long,
                   redpanda: Redpanda)
                  (using val httpClient: Client[IO])

  object Props:

    given Reusability[Props] = Reusability.by(_.item_id)


  private val Componentʹ = ScalaFnComponent.withHooks[Props]
    .useState(List.empty[JsonKafkaRecord])

    .useEffectWithDepsBy { (p, _) => p.item_id }
                         { (p, records) => _ =>

      val createUrl = Uri.unsafeFromString(s"${p.redpanda.proxyUrl}/consumers/${p.redpanda.groupId}")
      val createBody = CreateConsumerConfig(p.redpanda.instanceName, "json", "earliest")

      val createRequest = Request[IO](Method.POST, createUrl)
        .withHeaders(headers)
        .withEntity(createBody)

      for
        response <- p.httpClient.expect[CreateConsumerResponse](createRequest)
        base_uri  = p.redpanda.proxyUrl + response.base_uri.substring(7+response.base_uri.stripPrefix("http://").indexOf("/"))
        subUrl    = Uri.unsafeFromString(s"$base_uri/subscription")
        subBody   = SubscriptionPayload(List(p.redpanda.topic))
        subReq    = Request[IO](Method.POST, subUrl).withHeaders(headers).withEntity(subBody)
        _        <- p.httpClient.successful(subReq)
        recUrl    = Uri.unsafeFromString(s"$base_uri/records?offset=${p.redpanda.offset}&max_bytes=${p.redpanda.maxBytes}&timeout=${p.redpanda.timeout}")
        pollReq   = Request[IO](Method.GET, recUrl).withHeaders(headers)
        newRec   <- p.httpClient.expect[List[JsonKafkaRecord]](pollReq)
        offUrl    = Uri.unsafeFromString(s"$base_uri/offsets")
        offReq    = Request[IO](Method.POST, offUrl).withHeaders(headers)
        _        <- p.httpClient.successful(offReq)
        _        <- records.modState(_ ::: newRec.filter { it => if p.pid == -1 then true else it.value.pid == p.pid }).to[IO]
      yield
        ()
    }

    .renderWithReuse { (p, records) =>

      <.div(
        <.p(s"""Redpanda REST Proxy ['${p.redpanda.topic}' topic] #${records.value.size} records (${(if records.value.isEmpty then "current" else "last") + " offset = " + records.value.lastOption.fold(p.redpanda.offset)(_.offset)})"""),

        if records.value.nonEmpty
        then
          <.div(
            ^.padding := "10px",
            ^.border := "1px solid #ccc",
            <.table(
              ^.className := "table-auto", // Optional CSS classes
              <.thead(
                <.tr(
                  <.th("PID"),
                  <.th("Number"),
                  <.th("Clock"),
                  <.th("Started"),
                  <.th("Ended"),
                  <.th("Agent"),
                  <.th("Name"),
                  <.th("Polarity"),
                  <.th("Key"),
                  <.th("Guard"),
                  <.th("Label"),
                  <.th("Rate"),
                  <.th("Delay"),
                  <.th("Duration"),
                  <.th("Direction").when(p.isBioAmbients),
                  <.th("Capability").when(p.isBioAmbients),
                  <.th("From").when(p.isBioAmbients),
                  <.th("To").when(p.isBioAmbients),
                  <.th("Snapshot").when(p.isBioAmbients)
                )
              ),
              <.tbody(
                records.value.map { case JsonKafkaRecord(_, _, rec, _, _) =>
                  val key = s"""${rec.pid}-${rec.number}${rec.polarity.fold("")("-" + _.toString)}"""
                  <.tr(^.key := key,
                       <.td(rec.pid),
                       <.td(rec.number),
                       <.td(rec.clock),
                       <.td(new js.Date(rec.started.toDouble).toISOString()),
                       <.td(new js.Date(rec.ended.toDouble).toISOString()),
                       <.td(rec.agent),
                       <.td(rec.name),
                       <.td(rec.polarity.fold("")(_.toString)),
                       <.td(rec.key),
                       <.td(rec.guard.toString),
                       <.td(rec.label),
                       <.td(rec.rate),
                       <.td(rec.delay),
                       <.td(rec.duration.getOrElse(Double.NaN).toString),
                       <.td(rec.dir_cap match { case it @ Some("local" | "s2s" | "p2c" | "c2p") => it case _ => None }: Option[String]).when(p.isBioAmbients),
                       <.td(rec.dir_cap match { case it @ Some("enter" | "accept" | "exit" | "expel" | "merge+" | "merge-") => it case _ => None }: Option[String]).when(p.isBioAmbients),
                       <.td(rec.from).when(p.isBioAmbients),
                       <.td(rec.to).when(p.isBioAmbients),
                       <.td(rec.snapshot.map(Download(key + ".xml", _, "text/xml"))).when(p.isBioAmbients)
                  )
                }.toTagMod
              )
            )
          )
        else
          <.div
      )
    }

  val Component = React.memo(Componentʹ)
