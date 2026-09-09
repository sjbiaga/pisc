package basc
package feedback

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

import cats.instances.list.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*

import cats.effect.{ IO, Ref }

import io.circe.Codec
import io.circe.parser.*

import monocle.{ Focus, Lens }

import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.*
import japgolly.scalajs.react.ReactMonocle.*
import japgolly.scalajs.react.vdom.html_<^.*


package object amazonsqs:

  @js.native
  @JSImport("@aws-sdk/client-sqs", "SQSClient")
  class SQSClient(config: js.Object) extends js.Object

  @js.native
  @JSImport("@aws-sdk/client-sqs", "ReceiveMessageCommand")
  class ReceiveMessageCommand(params: js.Object) extends js.Object

  @js.native
  @JSImport("@aws-sdk/client-sqs", "DeleteMessageCommand")
  class DeleteMessageCommand(params: js.Object) extends js.Object

  @js.native
  trait SQSMessage extends js.Object {
    val MessageId: js.UndefOr[String] = js.native
    val ReceiptHandle: js.UndefOr[String] = js.native
    val Body: js.UndefOr[String] = js.native
  }

  @js.native
  trait ReceiveMessageResponse extends js.Object {
    val Messages: js.UndefOr[js.Array[SQSMessage]] = js.native
  }


  case class Message(pid: Long,
                     number: Long, clock: Double, started: Long, ended: Long,
                     agent: String, name: String, polarity: Option[Boolean],
                     key: String, guard: Boolean, label: String,
                     rate: String, delay: Double, duration: Option[Double],
                     dir_cap: Option[String], from: Option[String], to: Option[String],
                     snapshot: Option[String]) derives Codec.AsObject


  case class Item(message: Message,
                  receiptHandle: String,
                  id: Int,
                  delete: Boolean = true,
                  removed: Boolean = false)

  object Item:

    given Reusability[Item] = Reusability.by_==
    given Reusability[List[Item]] = Reusability.by_==

  case class AmazonSQSReceiver(queueUrl: String,
                               region: String,
                               accessKey: String,
                               secretKey: String,
                               sessionToken: String,
                               limit: Int,
                               timeout: Int):

    private val sqsClient: SQSClient =
      new SQSClient(
        js.Dynamic.literal(
          //disableHostPrefix = true,
          region = region,
          endpoint = queueUrl.substring(0, queueUrl.indexOf("queue")),
          useQueueUrlAsEndpoint = true,
          credentials = js.Dynamic.literal(
            accessKeyId = accessKey,
            secretAccessKey = secretKey,
            sessionToken = sessionToken
          )
        )
      )

    val fetchMessages: IO[ReceiveMessageResponse] =
      IO.fromPromise {
        IO {
          val params = js.Dynamic.literal(
            QueueUrl = queueUrl,
            MaxNumberOfMessages = 10 min limit,
            WaitTimeSeconds = timeout
          )
          val command = new ReceiveMessageCommand(params)
          sqsClient.asInstanceOf[js.Dynamic].send(command).asInstanceOf[js.Promise[ReceiveMessageResponse]]
        }
      }

    def deleteMessage(id: Int, receiptHandle: String): IO[Int] =
      IO.fromPromise {
        IO {
          val params = js.Dynamic.literal(QueueUrl = queueUrl, ReceiptHandle = receiptHandle)
          val command = new DeleteMessageCommand(params)
          sqsClient.asInstanceOf[js.Dynamic].send(command).asInstanceOf[js.Promise[js.Object]]
        }
      }.as(id).handleError(_ => -1)


  case class Props(item_id: String,
                   isBioAmbients: Boolean,
                   pid: Long,
                   receiver: AmazonSQSReceiver)

  object Props:

    given Reusability[Props] = Reusability.by(_.item_id)


  private val Componentʹ = ScalaFnComponent.withHooks[Props]
    .useStateSnapshotWithReuse(List.empty[Item])

    .useEffectWithDepsBy { (p, _) => p.item_id }
                         { (p, items) => _ =>
      def loop(buffer: Ref[IO, Vector[Item]]): IO[Unit] =
        p.receiver.fetchMessages.flatMap { response =>
          buffer.modify { v =>
            val vʹ = response
              .Messages
              .toOption
              .map(_
                     .toVector
                     .flatMap { m => m.Body.toOption zip m.ReceiptHandle.toOption }
                     .flatMap { (b, h) => parse(b).toOption.map(_ -> h) }
                     .flatMap { (j, h) => j.as[Message].toOption.map(_ -> h) }
                     .filter { (m, h) => if p.pid == -1 then true else m.pid == p.pid }
                     .zipWithIndex
                     .map { case ((m, h), i) => Item(m, h, v.length + i) }
              )
              .getOrElse(Vector.empty)
            val vʹʹ = v ++ vʹ
            (vʹʹ, vʹ.nonEmpty && vʹʹ.length < p.receiver.limit)
          }.ifM(loop(buffer), buffer.get.flatMap(v => items.modState(_ ::: v.toList).to[IO]))
        }
      IO.ref(Vector.empty[Item]).flatMap(loop)
    }

    .renderWithReuse { (p, items) =>

      <.div(
        <.p(s"""AmazonSQS AWS-SDK ['${p.receiver.queueUrl}' queue URL] #${items.value.size} items""",

          <.button(
            ^.marginLeft := "15px",
            ^.disabled   := items.value.forall(!_.delete),
            ^.onClick   --> items.value
              .filter(_.delete)
              .parTraverse { item => p.receiver.deleteMessage(item.id, item.receiptHandle) }
              .flatMap { ids =>
                items.modState {
                  _.map { item =>
                    if ids.contains(item.id)
                    then
                      item.copy(delete = false, removed = true)
                    else
                      item
                  }
                }.to[IO]
              },
            "Delete"
          )
        ),

        if items.value.nonEmpty
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
                  <.th("Snapshot").when(p.isBioAmbients),
                  <.th(
                    <.input(
                      ^.id        := "delete-all-checkbox",
                      ^.`type`    := "checkbox",
                      ^.checked   := items.value.filterNot(_.removed).forall(_.delete),
                      ^.onChange ==> { (e: ReactEventFromInput) =>
                        items.modState {
                          _.map { item =>
                            if item.removed
                            then
                              item
                            else
                              item.copy(delete = e.target.checked)
                          }
                        }
                      }
                    ),
                    "Delete"
                  )
                )
              ),
              <.tbody(
                items.value.map { it =>
                  val lens = Lens[List[Item], Item](_.find(_.id == it.id).get) { i =>
                    _.map { iʹ => if i.id == iʹ.id then iʹ else i }
                  }

                  val item = StateSnapshot
                    .withReuse
                    .zoomL(lens)
                    .prepare(items.toModStateFn)
                    .apply(items.value)

                  val delete = StateSnapshot
                    .withReuse
                    .zoomL(Focus[Item](_.delete))
                    .prepare(item.toModStateFn)
                    .apply(item.value)

                  val removed = StateSnapshot
                    .withReuse
                    .zoomL(Focus[Item](_.removed))
                    .prepare(item.toModStateFn)
                    .apply(item.value)

                  val msg = it.message
                  val key = s"""${msg.pid}-${msg.number}${msg.polarity.fold("")("-" + _.toString)}"""

                  <.tr(^.key := key,
                       ^.style := {
                         if removed.value
                         then
                           js.Dictionary("opacity" -> "0")
                         else
                           js.Dictionary.empty
                       },
                       <.td(msg.pid),
                       <.td(msg.number),
                       <.td(msg.clock),
                       <.td(new js.Date(msg.started.toDouble).toISOString()),
                       <.td(new js.Date(msg.ended.toDouble).toISOString()),
                       <.td(msg.agent),
                       <.td(msg.name),
                       <.td(msg.polarity.fold("")(_.toString)),
                       <.td(msg.key),
                       <.td(msg.guard.toString),
                       <.td(msg.label),
                       <.td(msg.rate),
                       <.td(msg.delay),
                       <.td(msg.duration.getOrElse(Double.NaN).toString),
                       <.td(msg.dir_cap match { case it @ Some("local" | "s2s" | "p2c" | "c2p") => it case _ => None }: Option[String]).when(p.isBioAmbients),
                       <.td(msg.dir_cap match { case it @ Some("enter" | "accept" | "exit" | "expel" | "merge+" | "merge-") => it case _ => None }: Option[String]).when(p.isBioAmbients),
                       <.td(msg.from).when(p.isBioAmbients),
                       <.td(msg.to).when(p.isBioAmbients),
                       <.td(msg.snapshot.map(Download(key + ".xml", _, "text/xml"))).when(p.isBioAmbients),
                       <.td(
                         <.input(
                           ^.id        := s"delete-${it.id}-checkbox",
                           ^.`type`    := "checkbox",
                           ^.checked   := delete.value,
                           ^.disabled  := removed.value,
                           ^.onChange ==> { (e: ReactEventFromInput) => delete.setState(e.target.checked) }
                         )
                       )
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
