package basc
package feedback
package analytics

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

import cats.effect.IO

import fs2.Stream
import fs2.concurrent.SignallingRef

import io.circe.{ Codec, Decoder }
import io.circe.parser.*

import org.http4s.Uri
import org.http4s.dom.WebSocketClient
import org.http4s.client.websocket.{ WSFrame, WSRequest }

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*


package object velocityreport:

  @JSImport("#/basc/feedback/analytics/velocityreport/styles.css", JSImport.Namespace)
  @js.native
  object Stylesheet extends js.Object

  private val _ = Stylesheet

  sealed trait Payload

  case class WindowVelocity(name: String,
                            label: String,
                            clock: Double,
                            deltaDepth: Long,
                            deltaClock: Double,
                            structuralVelocity: Double, // ΔDepth / ΔClock
                            eventDensity: Long
  ) extends Payload derives Codec.AsObject

  case class ClockAttribution(name: String,
                              label: String,
                              clockStep: Double,
                              microscopicThreshold: Double,
                              isMicroscopicStep: Boolean
  ) extends Payload derives Codec.AsObject

  case class StructuralAlert(name: String,
                             clock: Double,
                             observedVelocity: Double,
                             runningMean: Double,
                             runningStdDev: Double,
                             zScore: Double
  ) extends Payload derives Codec.AsObject


  enum PayloadType:
    case CLOCK_ATTRIBUTION, STRUCTURAL_ALERT, WINDOW_VELOCITY

  object Payload:

    import PayloadType.*

    given Reusability[Payload] = Reusability.by_==

    given Decoder[Payload] = Decoder.instance { cursor =>
      for
        _tpe         <- cursor.downField("type").as[String]
        tpe           = PayloadType.valueOf(_tpe)
        payloadCursor = cursor.downField("payload")
        payload      <- tpe match
          case STRUCTURAL_ALERT  => payloadCursor.as[StructuralAlert]
          case CLOCK_ATTRIBUTION => payloadCursor.as[ClockAttribution]
          case WINDOW_VELOCITY   => payloadCursor.as[WindowVelocity]
      yield
        payload
    }

    def apply(uri: Uri): Stream[IO, Payload] =
      Stream.resource(WebSocketClient[IO].connectHighLevel(WSRequest(uri))).flatMap(
        _.receiveStream.flatMap {
          case WSFrame.Text(jsonText, _) =>
            parse(jsonText).toOption
              .flatMap(_.as[Payload].toOption)
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


  case class State(alerts: List[StructuralAlert] = Nil,
                   attributions: Map[String, List[ClockAttribution]] = Map.empty, // Grouped by channel
                   velocities: List[WindowVelocity] = Nil)

  object State:

    given Reusability[State] = Reusability.by_==


  // Sub-Component 1: Panel for Live Cascading Alerts
  val AlertPanel = ScalaFnComponent[List[StructuralAlert]] { alerts =>
    <.div(^.className := "panel alert-panel",
      <.h3("🛑 Active Structural Alerts"),
      <.ul(
        alerts.take(5).map { alert =>
          <.li(^.key := alert.name + alert.observedVelocity, ^.className := "alert-item-critical",
            <.strong(s"[Channel: ${alert.name}] "),
            <.span(s"Z-Score: ${String.format("%.2f", alert.zScore)}")
          )
        }.toTagMod
      )
    )
  }

  // Sub-Component 2: Upstream Clock Step Attribution Panel (Bar Matrix Mockup)
  val AttributionPanel = ScalaFnComponent[Map[String, List[ClockAttribution]]] { attributions =>
    <.div(^.className := "panel attribution-panel",
      <.h3("📊 Stochastic Clock Step Attribution (ΔClock Drops)"),
      <.div(^.className := "bar-matrix",
        attributions.flatMap { (name, attrs) =>
          attrs.map { attr =>
            val barWidth = math.min(100.0, math.max(5.0, 1.0 / (attr.clockStep + attr.microscopicThreshold))) // Visual weight for tiny steps
            val colorClass = if attr.isMicroscopicStep then "bar-orange" else "bar-blue"
            <.div(^.key := attr.name + attr.label, ^.className := "matrix-row",
              <.span(^.className := "label-text", s"${attr.name} (${attr.label}):"),
              <.div(^.className := "bar-container",
                <.div(^.className := s"bar $colorClass", ^.style := js.Dynamic.literal(width = s"${barWidth}%")),
                <.span(^.className := "value-text", s"Δt: ${String.format("%.6f", attr.clockStep)}")
              )
            )
          }
        }.toTagMod
      )
    )
  }

  // Sub-Component 3: Structural Velocity Trend Panel
  val VelocityPanel = ScalaFnComponent[List[WindowVelocity]] { velocities =>
    <.div(^.className := "panel velocity-panel",
      <.h3("📈 Structural Velocity Trend Over Time (ΔDepth / ΔClock)"),
      <.table(^.className := "velocity-table",
        <.thead(
          <.tr(<.th("Channel"), <.th("Label"), <.th("ΔDepth"), <.th("ΔClock"), <.th("Velocity"))
        ),
        <.tbody(
          velocities.take(10).map { vel =>
            <.tr(^.key := vel.name + vel.label + vel.clock,
              <.td(vel.name),
              <.td(vel.label),
              <.td(vel.deltaDepth.toString),
              <.td(String.format("%.4f", vel.deltaClock)),
              <.td(^.className := "bold-velocity", String.format("%.2f", vel.structuralVelocity))
            )
          }.toTagMod
        )
      )
    )
  }

  private val Componentʹ = ScalaFnComponent.withHooks[Props]
    .useState(State())

    .useEffectWithDepsBy { (p, _) => p.item_id }
                         { (p, state) => _ =>
                           Payload(Uri.unsafeFromString(p.url))
                             .evalMap {
                               case a: StructuralAlert =>
                                 state.modState { it => it.copy(alerts = (a :: it.alerts).take(50)) }.to[IO]
                               case c: ClockAttribution =>
                                 val k = c.name + c.label
                                 state.modState { it => it.copy(attributions = it.attributions + (k -> (c :: it.attributions.getOrElse(k, Nil)).take(5))) }.to[IO]
                               case v: WindowVelocity =>
                                 state.modState { it => it.copy(velocities = (v :: it.velocities).take(20)) }.to[IO]
                             }
                             .interruptWhen(p.signal)
                             .compile
                             .drain
                         }

    .renderWithReuse { (p, state) =>

      <.div(^.className := "dashboard-container",
        <.h1("⚡ Online BioAmbients Telemetry Diagnostics"),
        AlertPanel(state.value.alerts),
        <.div(^.className := "dashboard-grid",
          AttributionPanel(state.value.attributions),
          VelocityPanel(state.value.velocities)
        )
      )

    }

  val Component = React.memo(Componentʹ)
