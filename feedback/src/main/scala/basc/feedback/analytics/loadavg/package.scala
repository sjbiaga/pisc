package basc
package feedback
package analytics

import cats.effect.IO

import fs2.Stream
import fs2.concurrent.SignallingRef

import io.circe.Codec
import io.circe.parser.*

import org.http4s.Uri
import org.http4s.dom.WebSocketClient
import org.http4s.client.websocket.{ WSFrame, WSRequest }

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*


package object loadavg:

  case class LoadAvg(timestamp: Long,
                     oneMinuteLoad: Double,
                     tenMinutesLoad: Double,
                     fifteenMinutesLoad: Double,
                     clock: Double
  ) derives Codec.AsObject

  object LoadAvg:

    given Reusability[LoadAvg] = Reusability.by_==

    def apply(uri: Uri): Stream[IO, LoadAvg] =
      Stream.resource(WebSocketClient[IO].connectHighLevel(WSRequest(uri))).flatMap(
        _.receiveStream.flatMap {
          case WSFrame.Text(jsonText, _) =>
            parse(jsonText).toOption
              .flatMap(_.as[LoadAvg].toOption)
              .fold(Stream.empty)(Stream.emit)
          case _                         =>
            Stream.empty
        }
      )

    def apply(milliseconds: Long): Option[String] =
      val seconds = milliseconds / 1000
      val minutes = seconds / 60
      val hours = minutes / 60
      val days = hours / 24

      if seconds > 0
      then
        if minutes > 0
        then
          if hours > 0
          then
            if days > 0
            then
              Some(s"$days DAYS ${hours%24}:${minutes%60}:${seconds%60}")
            else
              Some(s"${hours%24}:${minutes%60}:${seconds%60}")
          else
            Some(s"${minutes%60}:${seconds%60}")
        else
          Some(s"${seconds%60}")
      else
        None


  case class Props(item_id: String,
                   url: String)
                  (val signal: SignallingRef[IO, Boolean])

  object Props:

    given Reusability[Props] = Reusability.by(_.item_id)


  private val Componentʹ = ScalaFnComponent.withHooks[Props]
    .useState(false)
    .useState(LoadAvg(0, .0, .0, .0, .0) -> 0L)

    .useEffectWithDepsBy { (p, _, _) => p.item_id }
                         { (p, _, avg) => _ =>
                           LoadAvg(Uri.unsafeFromString(p.url))
                             .evalMap { laʹ =>
                               avg.modState {
                                 case (la, _) if la.clock <= laʹ.clock =>
                                   laʹ -> laʹ.timestamp
                                 case (la, ts) =>
                                   la.copy(timestamp = laʹ.timestamp) -> ts
                               }.to[IO]
                             }
                             .interruptWhen(p.signal)
                             .compile
                             .drain
                         }

    .renderWithReuse { (p, tooltip, avg) =>

      <.div(

        <.span(
          ^.marginLeft := "8px",
          String.format("%.2f", avg.value._1.oneMinuteLoad)),

        <.span(
          ^.marginLeft := "8px",
          String.format("%.2f", avg.value._1.tenMinutesLoad)),

        <.span(
          ^.marginLeft := "8px",
          String.format("%.2f", avg.value._1.fifteenMinutesLoad)),

        <.div(
          ^.marginLeft := "8px",
          ^.position.relative,
          ^.display.inlineBlock,
          ^.onMouseOver --> tooltip.setState(true),
          ^.onMouseLeave --> tooltip.setState(false),

          <.span(s"@ ${avg.value._1.clock}"),

          if tooltip.value
          then
            <.div(
              ^.position.absolute,
              ^.bottom          := "100%",
              ^.left            := "50%",
              ^.transform       := "translateX(-25%)",
              ^.backgroundColor := "black",
              ^.color           := "white",
              ^.padding         := "5px",
              ^.borderRadius    := "4px",
              ^.whiteSpace.nowrap,
              ^.zIndex          := "100",
              LoadAvg(avg.value._1.timestamp - avg.value._2).fold("LIVE")("IDLE " + _)
            )
          else
            <.div(^.display.inlineBlock, ^.position.absolute)
        ),

      )

    }

  val Component = React.memo(Componentʹ)
