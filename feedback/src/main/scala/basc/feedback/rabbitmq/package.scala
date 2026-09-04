package basc
package feedback

import scala.scalajs.js

import cats.effect.IO

import fs2.Stream
import fs2.concurrent.SignallingRef

import io.circe.Codec
import io.circe.parser.*

import org.http4s.Uri
import org.http4s.dom.WebSocketClient
import org.http4s.client.websocket.{ WSFrame, WSRequest }

import org.scalajs.dom.WebSocket

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*


package object rabbitmq:

  case class Message(pid: Long,
                     number: Long, clock: Double, started: Long, ended: Long,
                     agent: String, name: String, polarity: Option[Boolean],
                     key: String, guard: Boolean, label: String,
                     rate: String, delay: Double, duration: Option[Double],
                     dir_cap: String, from: String, to: String,
                     snapshot: Option[String]) derives Codec.AsObject

  object RabbitMQStomp:

    private def stompFrame(command: String, headers: List[(String, String)]): String =
      val headerStr = headers.map { case (k, v) => s"$k:$v" }.mkString("\n")
      s"$command\n$headerStr\n\n\u0000"

    def apply(uri: Uri, queue: String, username: String, password: String, subscriberId: String): Stream[IO, Message] =
      Stream.resource(WebSocketClient[IO].connectHighLevel(WSRequest(uri))).flatMap { connection =>
        val connectPayload = stompFrame("CONNECT", List(
          "accept-version" -> "1.1,1.2",
          "login"          -> username,
          "passcode"       -> password
        ))

        val subscribePayload = stompFrame("SUBSCRIBE", List(
          "id"          -> subscriberId,
          "destination" -> s"/queue/$queue",
          "ack"         -> "auto"
        ))

        val sendConnect = Stream.eval(connection.sendText(connectPayload))

        val processFrames = connection.receiveStream.flatMap {
          case WSFrame.Text(text, _) =>
            if text.startsWith("CONNECTED")
            then
              Stream.eval(connection.sendText(subscribePayload)).drain
            else if text.startsWith("MESSAGE")
            then
              text
              .split("\n\n")
              .lastOption
              .map(_.replace("\u0000", ""))
              .flatMap(parse(_).toOption)
              .flatMap(_.as[Message].toOption)
              .fold(Stream.empty)(Stream.emit)
            else
              Stream.empty
          case _ =>
            Stream.empty
        }

        processFrames.concurrently(sendConnect)
      }

  case class Props(queue: String,
                   signal: SignallingRef[IO, Boolean],
                   username: String,
                   password: String,
                   url: String,
                   subscriberId: String = "feedback-subscriber")

  val Component = ScalaFnComponent.withHooks[Props]
    .useState(Vector.empty[Message])

    .useEffectBy { (p, messages) =>
      RabbitMQStomp(Uri.unsafeFromString(p.url), p.queue, p.username, p.password, p.subscriberId)
        .evalMap { msg => messages.modState(_ :+ msg).to[IO] }
        .interruptWhen(p.signal)
        .compile
        .drain
    }

    .render { (p, messages) =>
      <.div(
        <.p(s"""RabbitMQ Web-STOMP ['${p.queue}' queue] #${messages.value.size} messages"""),

        if messages.value.nonEmpty
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
                messages.value.map { msg =>
                  <.tr(^.key := s"""${msg.number.toString}-${msg.polarity.fold("")(_.toString)}""",
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
                       <.td(msg.dir_cap match { case it @ ("local" | "s2s" | "p2c" | "c2p") => it case _ => "" }),
                       <.td(msg.dir_cap match { case it @ ("enter" | "accept" | "exit" | "expel" | "merge+" | "merge-") => it case _ => "" }),
                       <.td(msg.from),
                       <.td(msg.to),
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
