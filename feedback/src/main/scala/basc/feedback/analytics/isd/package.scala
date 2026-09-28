package basc
package feedback
package analytics

import scala.scalajs.js

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


package object isd:

  case class ESSWindowDiagnostics(label: String,
                            windowStart: Long,
                            windowEnd: Long,
                            sampleCount: Long,            // Total trace sub-trees in window (M)
                            effectiveSampleSize: Double,  // Computed ESS
                            efficiencyRatio: Double,      // ESS / M (0.0 to 1.0)
                            maxLogWeight: Double,         // Tracks weight extremes
                            weightedMeanDuration: Double  // Self-Normalized Importance Sampling latency
  ) derives Codec.AsObject


  enum PayloadType:
    case ESS_DIAGNOSTICS, DEGENERACY_ALERT

  object ESSWindowDiagnostics:

    import PayloadType.*

    given Reusability[ESSWindowDiagnostics] = Reusability.by_==

    given Decoder[(PayloadType, ESSWindowDiagnostics)] = Decoder.instance { cursor =>
      for
        _tpe         <- cursor.downField("type").as[String]
        tpe           = PayloadType.valueOf(_tpe)
        payloadCursor = cursor.downField("payload")
        payload      <- payloadCursor.as[ESSWindowDiagnostics]
      yield
        tpe -> payload
    }

    def apply(uri: Uri): Stream[IO, (PayloadType, ESSWindowDiagnostics)] =
      Stream.resource(WebSocketClient[IO].connectHighLevel(WSRequest(uri))).flatMap(
        _.receiveStream.flatMap {
          case WSFrame.Text(jsonText, _) =>
            parse(jsonText).toOption
              .flatMap(_.as[(PayloadType, ESSWindowDiagnostics)].toOption)
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


  case class State(alerts: Map[String, List[ESSWindowDiagnostics]] = Map.empty,
                   diagnostics: Map[String, List[ESSWindowDiagnostics]] = Map.empty)

  object State:

    given Reusability[State] = Reusability.by_==


  private val Componentʹ = ScalaFnComponent.withHooks[Props]
    .useState("")
    .useState(State())

    .useEffectWithDepsBy { (p, label, _) => (p.item_id, label.value) }
                         { (p, _, state) => _ =>
                           ESSWindowDiagnostics(Uri.unsafeFromString(p.url))
                             .evalMap {
                               case (PayloadType.DEGENERACY_ALERT, a: ESSWindowDiagnostics) =>
                                 state.modState { it => it.copy(alerts = it.alerts + (a.label -> (a :: it.alerts.getOrElse(a.label, Nil)).take(5))) }.to[IO]
                               case (PayloadType.ESS_DIAGNOSTICS, d: ESSWindowDiagnostics) =>
                                 state.modState { it => it.copy(diagnostics = it.diagnostics + (d.label -> (d :: it.diagnostics.getOrElse(d.label, Nil)).take(50))) }.to[IO]
                             }
                             .interruptWhen(p.signal)
                             .compile
                             .drain
                         }

    .renderWithReuse { (p, label, state) =>

      val ks = (state.value.alerts.keySet ++ state.value.diagnostics.keySet) + ""

      // --- Helper Card Renderer ---
      def metricCard(title: String, value: String, isWarning: Boolean) =
        <.div(
          ^.className := s"p-4 rounded-lg bg-gray-800 border ${if (isWarning) "border-red-500 bg-red-950/20" else "border-gray-700"}",
          <.p(^.className := "text-xs font-medium text-gray-400 uppercase", title),
          <.p(^.className := s"text-2xl font-bold mt-1 ${if (isWarning) "text-red-400" else "text-white"}", value)
        )

      // --- Lightweight Scalable Vector Graphics (SVG) Trend Line Chart ---
      def renderSvgTrendChart(history: List[ESSWindowDiagnostics]) =
        if history.size < 2
        then
          <.div(^.className := "h-40 flex items-center justify-center text-gray-500", "Collecting trend points...")
        else
          val width = 800.0
          val height = 160.0
          val padding = 20.0

          val pointsCount = history.length
          val stepX = (width - (padding * 2)) / (pointsCount - 1)

          // Map efficiency ratio (0.0 to 1.0) to SVG Y coordinates
          val svgPoints = history.zipWithIndex.map { case (d, i) =>
            val x = padding + (i * stepX)
            val y = height - padding - (d.efficiencyRatio * (height - (padding * 2)))
            s"$x,$y"
          }.mkString(" ")

          import japgolly.scalajs.react.vdom.svg_<^.*
          import japgolly.scalajs.react.vdom.html_<^.{ ^ => ^^ }

          <.svg(
            ^^.className := "w-full h-44 bg-gray-900 rounded",
            ^.viewBox := s"0 0 $width $height",

            // Threshold Line at 10% Efficiency (0.10)
            {
              val thresholdY = height - padding - (0.10 * (height - (padding * 2)))
              <.line(
                ^.x1 := padding, ^.y1 := thresholdY,
                ^.x2 := width - padding, ^.y2 := thresholdY,
                ^.stroke := "#ef4444", ^.strokeDasharray := "4", ^.strokeWidth := "1.5"
              )
            },

            // Trend Polyline
            <.polyline(
              ^.fill := "none",
              ^.stroke := "#3b82f6",
              ^.strokeWidth := "2.5",
              ^.points := svgPoints
            )
          )

      <.div(
        ^.className := "p-6 max-w-7xl mx-auto space-y-6 font-sans bg-gray-900 text-gray-100 min-h-screen",

        <.label(
          ^.htmlFor    := "label-select", "Label: "),

        <.select(
          ^.id        := "label-select",
          ^.value     := label.value,
          ^.onChange ==> { (e: ReactEventFromInput) => label.setState(e.target.value) },

          ks.map { case label => <.option(^.value := label, label) }.toTagMod
        ),

        // --- Header Status Bar ---
        <.div(
          ^.className := "flex justify-between items-center border-b border-gray-800 pb-4",
          <.h1(^.className := "text-2xl font-bold text-white", "Importance Sampling Diagnostics"),
        ),

        // --- Key Metric Cards ---
        state.value.diagnostics.get(label.value).map { case diag :: _ =>
          <.div(
            ^.className := "grid grid-cols-1 md:grid-cols-4 gap-4",
            metricCard("Efficiency Ratio", f"${diag.efficiencyRatio * 100}%.1f%%", diag.efficiencyRatio < 0.15),
            metricCard("Effective Sample Size", f"${diag.effectiveSampleSize}%.1f", false),
            metricCard("Sample Count", s"${diag.sampleCount}", false),
            metricCard("Max Log Weight", f"${diag.maxLogWeight}%.2f", diag.maxLogWeight > 3.0)
          )
        }.getOrElse(<.div(^.className := "text-gray-500", "Awaiting stream data...")),

        // --- Live Efficiency & ESS Sparkline / SVG Chart ---
        <.div(
          ^.className := "bg-gray-800 p-4 rounded-lg shadow border border-gray-700",
          <.h2(^.className := "text-lg font-semibold mb-2 text-gray-200", "Live Sampling Efficiency Trend"),
          renderSvgTrendChart(state.value.diagnostics.get(label.value).map(_.take(50)).getOrElse(Nil))
        ),

        // --- Degenerate Sampling Alerts Section ---
        <.div(
          ^.className := "bg-gray-800 p-4 rounded-lg shadow border border-gray-700",
          <.h2(^.className := "text-lg font-semibold mb-3 text-red-400 flex items-center space-x-2",
            <.span("⚠ Degenerate Sampling Alerts")
          ),
          if (state.value.alerts.get(label.value).isEmpty)
            <.p(^.className := "text-gray-500 text-sm", "No degeneracy alerts detected. Target parameters well matched.")
          else
            <.div(
              ^.className := "space-y-2 max-h-60 overflow-y-auto",
              state.value.alerts.get(label.value).map { alerts =>
                alerts.take(5).map { alert =>
                  <.div(
                    ^.className := "bg-red-950/50 border border-red-800/80 p-3 rounded text-sm text-red-200 flex justify-between items-start",
                    <.div(
                      <.p(^.className := "font-semibold", f"CRITICAL: Importance Sampling Efficiency dropped to ${alert.efficiencyRatio * 100}%.2f%% (ESS: ${alert.effectiveSampleSize}%.1f / M: ${alert.sampleCount}). Targets are diverging."),
                      <.p(^.className := "text-xs text-red-400 mt-1",
                        s"Window: ${alert.windowStart} - ${alert.windowEnd} | Max Log Weight: ${alert.maxLogWeight}"
                      )
                    ),
                    <.span(^.className := "text-xs text-gray-400 whitespace-nowrap",
                      new js.Date(alert.windowEnd.toDouble).toLocaleTimeString()
                    )
                  )
                }.toTagMod
              }.getOrElse(<.div)
            )
        )
      )

    }

  val Component = React.memo(Componentʹ)
