package basc
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
import japgolly.scalajs.react.util.EffectCatsEffect.*
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
                   dir_cap: String, from: String, to: String,
                   snapshot: Option[String]) derives Codec.AsObject

  case class JsonKafkaRecord(topic: String, key: Key, value: Value, partition: Int, offset: Long) extends AbstractKafkaRecord[Key, Value] derives Codec.AsObject

  case class Props(topic: String,
                   offset: Long,
                   maxBytes: Int,
                   timeout: Int,
                   proxyUrl: String = "/redpanda-proxy",
                   groupId: String = "feedback-json-group",
                   instanceName: String = s"feedback-json-instance-${System.nanoTime}")

  def Component(using httpClient: Client[IO]) = ScalaFnComponent.withHooks[Props]
    .useState(List.empty[JsonKafkaRecord])

    .useEffectOnMountBy { (p, records) =>
      val createUrl = Uri.unsafeFromString(s"${p.proxyUrl}/consumers/${p.groupId}")
      val createBody = CreateConsumerConfig(p.instanceName, "json", "earliest")

      val createRequest = Request[IO](Method.POST, createUrl)
        .withHeaders(headers)
        .withEntity(createBody)

      for
        response <- httpClient.expect[CreateConsumerResponse](createRequest)
        base_uri  = p.proxyUrl + response.base_uri.substring(7+response.base_uri.stripPrefix("http://").indexOf("/"))
        subUrl    = Uri.unsafeFromString(s"$base_uri/subscription")
        subBody   = SubscriptionPayload(List(p.topic))
        subReq    = Request[IO](Method.POST, subUrl).withHeaders(headers).withEntity(subBody)
        _        <- httpClient.successful(subReq)
        recUrl    = Uri.unsafeFromString(s"$base_uri/records?offset=${p.offset}&max_bytes=${p.maxBytes}&timeout=${p.timeout}")
        pollReq   = Request[IO](Method.GET, recUrl).withHeaders(headers)
        newRec   <- httpClient.expect[List[JsonKafkaRecord]](pollReq)
        offUrl    = Uri.unsafeFromString(s"$base_uri/offsets")
        offReq    = Request[IO](Method.POST, offUrl).withHeaders(headers)
        _        <- httpClient.successful(offReq)
        _        <- records.modState(_ ::: newRec).to[IO]
      yield
        ()
    }

    .render { (p, records) =>
      <.div(
        <.p(s"""Redpanda REST Proxy ['${p.topic}' topic] #${records.value.size} records (${(if records.value.isEmpty then "current" else "last") + " offset = " + records.value.lastOption.fold(p.offset)(_.offset)})"""),

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
                  <.th("Direction"),
                  <.th("Capability"),
                  <.th("From"),
                  <.th("To"),
                  <.th("Snapshot")
                )
              ),
              <.tbody(
                records.value.map { case JsonKafkaRecord(_, _, rec, _, _) =>
                  <.tr(^.key := s"""${rec.number.toString}-${rec.polarity.fold("")(_.toString)}""",
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
                       <.td(rec.duration.getOrElse(Double.NaN)),
                       <.td(rec.dir_cap match { case it @ ("local" | "s2s" | "p2c" | "c2p") => it case _ => "" }),
                       <.td(rec.dir_cap match { case it @ ("enter" | "accept" | "exit" | "expel" | "merge+" | "merge-") => it case _ => "" }),
                       <.td(rec.from),
                       <.td(rec.to),
                       <.td("")
                  )
                }.toTagMod
              )
            )
        )
        else
          VdomArray.empty()
      )
    }
