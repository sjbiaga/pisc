package basc
package feedback
package analytics

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import org.scalajs.dom.{ HTMLCanvasElement, URL }

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


package object sweepline:

  @js.native
  @JSGlobal("Chart")
  class Chart(context: HTMLCanvasElement, config: js.Object) extends js.Object:
    def update(): Unit = js.native
    def data: js.Dynamic = js.native


  case class SweepLine(timestamp: Long,
                       label: String,
                       clock: Double,
                       histogram: Map[Int, Double]
  ) derives Codec.AsObject


  case class SweepLine1msBurst(timestamp: Long,
                               clock: Double,
                               histograms: Map[String, SweepLine]
  ) derives Codec.AsObject

  object SweepLine1msBurst:

    given Reusability[SweepLine1msBurst] = Reusability.by_==

    def apply(uri: Uri): Stream[IO, SweepLine1msBurst] =
      Stream.resource(WebSocketClient[IO].connectHighLevel(WSRequest(uri))).flatMap(
        _.receiveStream.flatMap {
          case WSFrame.Text(jsonText, _) =>
            parse(jsonText).toOption
              .flatMap(_.as[SweepLine1msBurst].toOption)
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
                  counts: js.Array[Int],
                  percentages: js.Array[Double])

  case class State(timestamp: Long = 0L,
                   idle: Long = 0L,
                   clock: Double = .0,
                   histograms: Map[Int, Data] = Map.empty,
                   selected: Int = -1)

  object State:

    given Reusability[State] = Reusability.by_==


  private val Componentʹ = ScalaFnComponent.withHooks[Props]
    .useState(false)
    .useState(State())

    .useRefToVdom[HTMLCanvasElement]
    .useRef[Chart](null)

    .useEffectWithDepsBy { (p, _, _, _, _) => p.item_id }
                         { (p, _, state, _, _) => _ =>
                           SweepLine1msBurst(Uri.unsafeFromString(p.url))
                             .evalMap { burstʹ =>
                               state.modState {
                                 case State(_, _, _, _, -1) =>
                                   val hsʹ = burstʹ.histograms.zipWithIndex.map { case ((label, sl), i) =>
                                     val his = sl.histogram.toList.sortBy(_._1)
                                     val counts = js.Array(his.map(_._1)*)
                                     val percents = js.Array(his.map(_._2)*)
                                     i -> Data(label, false, counts, percents)
                                   }.toMap
                                   State(burstʹ.timestamp, 0L, burstʹ.clock, hsʹ, 0)
                                 case State(_, _, clock, hs, i) if clock <= burstʹ.clock =>
                                   var j = hs.size - 1
                                   val hsʹ = burstʹ.histograms.map {
                                     case (label, sl) =>
                                       val his = sl.histogram.toList.sortBy(_._1)
                                       val counts = js.Array(his.map(_._1)*)
                                       val percents = js.Array(his.map(_._2)*)
                                       hs.find { case (_, Data(`label`, _, _, _)) => true case _ => false } match
                                         case Some((j, _)) =>
                                           j -> Data(label, false, counts, percents)
                                         case _ =>
                                           j += 1
                                           j -> Data(label, false, counts, percents)
                                   }
                                   hs.foreach(_._2.disabled = true)
                                   State(burstʹ.timestamp, 0L, burstʹ.clock, hs ++ hsʹ, i)
                                 case it: State =>
                                   it.copy(idle = burstʹ.timestamp - it.timestamp)
                               }.to[IO]
                             }
                             .interruptWhen(p.signal)
                             .compile
                             .drain
                         }

    .useEffectBy { (_, _, state, canvasRef, chartRef) =>
      val State(_, _, clock, hs, i) = state.value

      canvasRef.foreach { currentCanvas =>
        if hs.nonEmpty
        then
          if chartRef.value eq null
          then
            // Initialize Chart.js configuration if it doesn't exist yet
            val chartConfig = js.Dynamic.literal(
              `type` = "bar", // Horizontal representation chart setup
              data = js.Dynamic.literal(
                labels = hs(i).counts,
                datasets = js.Array(
                  js.Dynamic.literal(
                    label = hs(i).label,
                    data = hs(i).percentages,
                    backgroundColor = if hs(i).disabled then "#808080" else "#4caf50",
                    borderWidth = 1
                  )
                )
              ),
              options = js.Dynamic.literal(
                indexAxis = "y", // Flip coordinates to make it a horizontal bar histogram
                responsive = true,
                scales = js.Dynamic.literal(
                  x = js.Dynamic.literal(
                    min = 0,
                    max = 100, // Explicitly lock percentage parameters to 100% bounds
                    title = js.Dynamic.literal(display = true, text = "Percentage %")
                  )
                )
              )
            )

            // Instantiate the Chart instance directly on the mutable reference pointer
            chartRef.value_=(Chart(currentCanvas, chartConfig))
          else
            // If the instance already exists, modify the properties and trigger a visual update
            val activeChart = chartRef.value
            activeChart.data.labels = hs(i).counts
            activeChart.data.datasets.asInstanceOf[js.Array[js.Dynamic]](0).data = hs(i).percentages
            activeChart.data.datasets.asInstanceOf[js.Array[js.Dynamic]](0).label = hs(i).label
            activeChart.data.datasets.asInstanceOf[js.Array[js.Dynamic]](0).backgroundColor = if hs(i).disabled then "#808080" else "#4caf50"
            activeChart.update()
      }
    }

    .renderWithReuse { (p, tooltip, state, canvasRef, _) =>
      val State(_, idle, clock, hs, i) = state.value

      <.div(

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

          hs.map { case (j, Data(label, _, _, _)) => <.option(^.value := j, label) }.toTagMod
        ),

        if hs.nonEmpty
        then
          <.div(
            ^.style := js.Dictionary("width" -> "100%", "padding" -> "20px"),
            <.canvas.withRef(canvasRef)
          )
        else
          <.div

      )

    }

  val Component = React.memo(Componentʹ)
