package pisc
package feedback
package analytics

import scala.scalajs.js

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

  case class LoadAvg1msBurst(timestamp: Long,
                             clock: Double,
                             averages: Map[String, LoadAvg]
  ) derives Codec.AsObject

  object LoadAvg1msBurst:

    given Reusability[LoadAvg1msBurst] = Reusability.by_==

    def apply(uri: Uri): Stream[IO, LoadAvg1msBurst] =
      Stream.resource(WebSocketClient[IO].connectHighLevel(WSRequest(uri))).flatMap(
        _.receiveStream.flatMap {
          case WSFrame.Text(jsonText, _) =>
            parse(jsonText).toOption
              .flatMap(_.as[LoadAvg1msBurst].toOption)
              .fold(Stream.empty)(Stream.emit)
          case _                         =>
            Stream.empty
        }
      )


  case class Props(item_id: String,
                   url: String)
                  (val signal: SignallingRef[IO, Boolean])

  object Props:

    given Reusability[Props] = Reusability.by(_.item_id)


  case class Data(label: String,
                  var disabled: Boolean,
                  oneMinuteLoad: Double,
                  tenMinutesLoad: Double,
                  fifteenMinutesLoad: Double)

  case class State(timestamp: Long = 0L,
                   idle: Long = 0L,
                   clock: Double = .0,
                   averages: Map[Int, Data] = Map.empty,
                   selected: Int = -1)

  object State:

    given Reusability[State] = Reusability.by_==


  private val Componentʹ = ScalaFnComponent.withHooks[Props]
    .useState(false)
    .useState(State())

    .useEffectWithDepsBy { (p, _, _) => p.item_id }
                         { (p, _, state) => _ =>
                           LoadAvg1msBurst(Uri.unsafeFromString(p.url))
                             .evalMap { burstʹ =>
                               state.modState {
                                 case State(_, _, _, _, -1) =>
                                   val asʹ = burstʹ.averages.zipWithIndex.map { case ((label, la), i) =>
                                     i -> Data(label, false, la.oneMinuteLoad, la.tenMinutesLoad, la.fifteenMinutesLoad)
                                   }.toMap
                                   State(burstʹ.timestamp, 0L, burstʹ.clock, asʹ, 0)
                                 case State(_, _, clock, as, i) if clock <= burstʹ.clock =>
                                   var j = as.size - 1
                                   val asʹ = burstʹ.averages.map {
                                     case (label, la) =>
                                       as.find { case (_, Data(`label`, _, _, _, _)) => true case _ => false } match
                                         case Some((j, _)) =>
                                           j -> Data(label, false, la.oneMinuteLoad, la.tenMinutesLoad, la.fifteenMinutesLoad)
                                         case _ =>
                                           j += 1
                                           j -> Data(label, false, la.oneMinuteLoad, la.tenMinutesLoad, la.fifteenMinutesLoad)
                                   }.toMap
                                   as.foreach(_._2.disabled = true)
                                   State(burstʹ.timestamp, 0L, burstʹ.clock, as ++ asʹ, i)
                                 case it: State =>
                                   it.copy(idle = burstʹ.timestamp - it.timestamp)
                               }.to[IO]
                             }
                             .interruptWhen(p.signal)
                             .compile
                             .drain
                         }

    .renderWithReuse { (p, tooltip, state) =>
      val State(_, idle, clock, as, i) = state.value
      val Data(label, disabled, oneMinuteLoad, tenMinutesLoad, fifteenMinutesLoad) =
        if as.nonEmpty
        then
          as(i)
        else
          Data("-", false, .0, .0, .0)

      val opacity =
        if disabled
        then
          ^.style := js.Dictionary("opacity" -> "0.5")
        else
          ^.style := js.Dictionary.empty

      <.div(

        <.span(
          ^.marginLeft := "8px",
          opacity,
          String.format("%.2f", oneMinuteLoad)),

        <.span(
          ^.marginLeft := "8px",
          opacity,
          String.format("%.2f", tenMinutesLoad)),

        <.span(
          ^.marginLeft := "8px",
          opacity,
          String.format("%.2f", fifteenMinutesLoad)),

        <.div(
          ^.marginLeft := "8px",
          ^.position.relative,
          ^.display.inlineBlock,
          ^.onMouseOver --> tooltip.setState(true),
          ^.onMouseLeave --> tooltip.setState(false),

          <.span(s"@ $clock"),

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
              time(idle).fold("LIVE")("IDLE " + _)
            )
          else
            <.div(^.display.inlineBlock, ^.position.absolute)
        ),

        <.label(
          ^.marginLeft := "15px",
          ^.htmlFor    := "label-select", "Label: "),

        <.select(
          ^.id        := "label-select",
          ^.value     := i,
          ^.onChange ==> { (e: ReactEventFromInput) => state.modState(_.copy(selected = e.target.value.toInt)) },

          as.map { case (j, Data(label, _, _, _, _)) => <.option(^.value := j, label) }.toTagMod
        )

      )

    }

  val Component = React.memo(Componentʹ)
