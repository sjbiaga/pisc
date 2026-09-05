package basc
package feedback

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

import cats.instances.list.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*

import cats.effect.IO

import io.circe.Codec
import io.circe.parser.*

import monocle.Focus

import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.*
import japgolly.scalajs.react.ReactMonocle.*
import japgolly.scalajs.react.util.EffectCatsEffect.*
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
                     rate: String, delay: Double, duration: Option[Double]) derives Codec.AsObject


  case class Item(message: Message,
                  receiptHandle: String,
                  id: Int,
                  delete: Boolean = true,
                  removed: Boolean = false)

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
          //endpoint = queueUrl.substring(0, queueUrl.indexOf("queue")),
          useQueueUrlAsEndpoint = true,
          credentials = js.Dynamic.literal(
            accessKeyId = accessKey,
            secretAccessKey = secretKey,
            sessionToken = sessionToken
          )
        )
      )

    private val fetchMessages: IO[ReceiveMessageResponse] =
      IO.fromPromise {
        IO {
          val params = js.Dynamic.literal(
            QueueUrl = queueUrl,
            MaxNumberOfMessages = limit,
            WaitTimeSeconds = timeout
          )
          val command = new ReceiveMessageCommand(params)
          sqsClient.asInstanceOf[js.Dynamic].send(command).asInstanceOf[js.Promise[ReceiveMessageResponse]]
        }
      }

    private def deleteMessage(id: Int, receiptHandle: String): IO[Int] =
      IO.fromPromise {
        IO {
          val params = js.Dynamic.literal(QueueUrl = queueUrl, ReceiptHandle = receiptHandle)
          val command = new DeleteMessageCommand(params)
          sqsClient.asInstanceOf[js.Dynamic].send(command).asInstanceOf[js.Promise[js.Object]]
        }
      }.as(id).handleError(_ => -1)

    val Component = ScalaFnComponent.withHooks[Unit]
      .useStateSnapshot(List.empty[Item])

      .useEffectOnMountBy { (_, items) =>
        fetchMessages.flatMap { response =>
          items.modState { list =>
            list ::: response
              .Messages
              .toOption
              .map(_
                     .toList
                     .flatMap { m => m.Body.toOption zip m.ReceiptHandle.toOption }
                     .flatMap { (b, h) => parse(b).toOption.map(_ -> h) }
                     .flatMap { (j, h) => j.as[Message].toOption.map(_ -> h) }
                     .zipWithIndex
                     .map { case ((m, h), i) => Item(m, h, list.length + i) }
              )
              .getOrElse(Nil)
          }.to[IO]
        }
      }

      .render { (_, items) =>
        <.div(
          <.p(s"""AmazonSQS AWS-SDK ['$queueUrl' queue URL] #${items.value.size} items""",

            <.button(
              ^.marginLeft := "15px",
              ^.disabled   := items.value.forall(!_.delete),
              ^.onClick   --> items.value
                .filter(_.delete)
                .parTraverse { item => deleteMessage(item.id, item.receiptHandle) }
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
            ),
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
                  items.value.map { item =>
                    val lens: StateSnapshot[Item] = items.zoomState(_(item.id)) { i => l =>
                      l.take(i.id) ::: i :: l.drop(i.id + 1)
                    }

                    val delete = lens.zoomStateL(Focus[Item](_.delete))
                    val removed = lens.zoomStateL(Focus[Item](_.removed))

                    val msg = item.message

                    <.tr(^.key := s"""${msg.number.toString}-${msg.polarity.fold("")(_.toString)}""",
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
                         <.td(msg.duration.getOrElse(Double.NaN)),
                         <.td(
                           <.input(
                             ^.id        := s"delete-${item.id}-checkbox",
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
            VdomArray.empty()
        )

      }
