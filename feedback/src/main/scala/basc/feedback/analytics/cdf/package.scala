package basc
package feedback
package analytics

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.JSConverters.*
import org.scalajs.dom.{ HTMLCanvasElement, URL }

import cats.effect.IO

import org.http4s.Uri
import org.http4s.dom.FetchClientBuilder

import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*


package object cdf:

  @js.native
  @JSGlobal("Chart")
  class Chart(context: HTMLCanvasElement, config: js.Object) extends js.Object:
    def destroy(): Unit = js.native

  // Prometheus Range Query Types
  @js.native
  trait PromRangeResponse extends js.Object {
    val status: String = js.native
    val data: PromRangeData = js.native
  }

  @js.native
  trait PromRangeData extends js.Object {
    val result: js.Array[PromRangeResultElement] = js.native
  }

  @js.native
  trait PromRangeResultElement extends js.Object {
    val metric: js.Dictionary[String] = js.native
    val values: js.Array[js.Array[js.Any]] = js.native // Array of [unix_timestamp, value_string]
  }

  // Internal representation for data series mapping
  case class TimeDataPoint(timestampMs: Long, value: Double)

  object TimeDataPoint:

    given Reusability[TimeDataPoint] = Reusability.by_==


  case class Props(item_id: String,
                   url: String,
                   topic: String,
                   uuid: String)

  object Props:

    given Reusability[Props] = Reusability.by(_.item_id)


  val baseQuery = "flink_taskmanager_job_task_operator_bioambients_flink4traces_cdf_topic_profile_uuid_label"

  def query(percentile: Int, filter: String): String =
    s"""${baseQuery}_${if percentile == 50 then "p50_Median_ms" else s"p${percentile}_ms"}{$filter}"""

  private def fetchHistory(baseUrl: String, topic: String, uuid: String,
                           profile: String, percentile: Int,
                           range: Int, step: Int): IO[Map[String, List[TimeDataPoint]]] =
    val now = System.currentTimeMillis / 1000
    val since = now - range
    FetchClientBuilder[IO].resource.use { client =>
      val filter = s"""topic="$topic""""
                 + s""",profile="${profile}_probability""""
                 + s""",uuid="$uuid""""

      val targetUri = Uri.unsafeFromString(s"$baseUrl/api/v1/query_range")
        .withQueryParam("query", query(percentile, filter))
        .withQueryParam("start", since.toString)
        .withQueryParam("end", now.toString)
        .withQueryParam("step", s"${step}s")

      client.get(targetUri) { response =>
        if response.status.isSuccess
        then
          response.as[String].map { json =>
            val promResponse = js.JSON.parse(json).asInstanceOf[PromRangeResponse]
            if promResponse.status == "success"
            then
              promResponse.data.result.toList.map { result =>
                val label = result.metric.get("label").getOrElse("")
                label -> result.values.toList.flatMap { element =>
                  scala.util.Try {
                    val ts = (element(0).asInstanceOf[Double] * 1000).toLong
                    val value = element(1).toString.toDouble
                    TimeDataPoint(ts, value)
                  }.toOption.toSeq
                }
              }.toMap
            else
              Map.empty
          }
        else
          IO.pure(Map.empty)
      }
    }


  case class State(high: Map[String, List[TimeDataPoint]] = Map(),
                   low: Map[String, List[TimeDataPoint]] = Map(),
                   percentile: Int = 90)

  object State:

    given Reusability[State] = Reusability.by_==


  private val Componentʹ = ScalaFnComponent.withHooks[Props]
    .useState(90) // 50 90 95 99
    .useState(1800) // seconds
    .useState(15) // data resolution granularity steps
    .useState("") // label
    .useState(0) // reload

    .useState(State())

    .useRefToVdom[HTMLCanvasElement]
    .useRef[Chart](null)

    .useEffectWithDepsBy { (p, _, _, _, _, reload, _, _, _) => (p.item_id, reload.value) }
                         { (p, percentile, range, step, _, _, state, _, _) => _ =>
      for
        high <- fetchHistory(p.url, p.topic, p.uuid, "high", percentile.value, range.value, step.value)
        low  <- fetchHistory(p.url, p.topic, p.uuid, "low", percentile.value, range.value, step.value)
        _    <- state.setState(State(high, low, percentile.value)).to[IO]
      yield
        ()
    }

    .useEffectWithDepsBy { (p, _, _, _, label, reload, _, _, _) => (p.item_id, label.value, reload.value) }
                         { (p, _, range, step, label, _, state, canvasRef, chartRef) => _ =>
      canvasRef.foreach { currentCanvas =>
        val State(high, low, percentile) = state.value
        val (highData, lowData) = high.getOrElse(label.value, Nil) -> low.getOrElse(label.value, Nil)

        val highDatasetPoints = highData.map(p => js.Dynamic.literal(x = p.timestampMs.toDouble, y = p.value)).toJSArray
        val lowDatasetPoints  = lowData.map(p => js.Dynamic.literal(x = p.timestampMs.toDouble, y = p.value)).toJSArray

        val configurationSchema = js.Dynamic.literal(
          `type` = "line",
          data = js.Dynamic.literal(
            datasets = js.Array(
              js.Dynamic.literal(
                label = s"High Probability p$percentile Delay",
                data = highDatasetPoints,
                borderColor = "#3e95cd",
                fill = false,
                tension = 0.1,
                clip = true
              ),
              js.Dynamic.literal(
                label = s"Low Probability (Rare) p$percentile Delay",
                data = lowDatasetPoints,
                borderColor = "#c45850",
                fill = false,
                tension = 0.1,
                clip = true
              )
            )
          ),
          options = js.Dynamic.literal(
            responsive = true,
            scales = js.Dynamic.literal(
              x = js.Dynamic.literal(
                `type` = "time", // Requires adding the chartjs-adapter-date-fns module to web environment configs
                time = js.Dynamic.literal(
                  unit = "second",
                  displayFormats = js.Dynamic.literal(second = "HH:mm:ss") // Formats human-readable x-axis labels
                )
              ),
              y = js.Dynamic.literal(
                title = js.Dynamic.literal(display = true, text = "Latency Duration (ms)"),
                min = 0,
              )
            )
          )
        )

        if chartRef.value ne null
        then
          chartRef.value.destroy()
        chartRef.value_=(Chart(currentCanvas, configurationSchema))
      }
    }

    .renderWithReuse { (p, percentile, range, step, label, reload, state, canvasRef, _) =>

      val ks = (state.value.high.keySet ++ state.value.low.keySet) + ""

      <.div(

        <.label(
          ^.marginLeft := "8px",
          ^.htmlFor    := "percentile-select", "Percentile: "),

        <.select(
          ^.id        := "percentile-select",
          ^.value     := percentile.value,
          ^.onChange ==> { (e: ReactEventFromInput) => percentile.setState(e.target.value.toInt) },

          <.option(^.value := 50, "50%"),
          <.option(^.value := 90, "90%"),
          <.option(^.value := 95, "95%"),
          <.option(^.value := 99, "99%")
        ),

        <.label(
          ^.marginLeft := "15px",
          ^.htmlFor    := "range-number", "Range: "),

        <.input(
          ^.id         := "range-number",
          ^.`type`     := "number",
          ^.value      := range.value,
          ^.size       := range.value.toString.length,
          ^.onChange  ==> { (e: ReactEventFromInput) => range.setState(e.target.valueAsNumber.toInt) },
        ),

        <.label(
          ^.marginLeft := "15px",
          ^.htmlFor    := "step-number", "Step: "),

        <.input(
          ^.id         := "step-number",
          ^.`type`     := "number",
          ^.value      := step.value,
          ^.size       := step.value.toString.length,
          ^.onChange  ==> { (e: ReactEventFromInput) => step.setState(e.target.valueAsNumber.toInt) },
        ),

        <.label(
          ^.marginLeft := "15px",
          ^.htmlFor    := "label-select", "Label: "),

        <.select(
          ^.id        := "label-select",
          ^.value     := label.value,
          ^.onChange ==> { (e: ReactEventFromInput) => label.setState(e.target.value) },

          ks.map { case label => <.option(^.value := label, label) }.toTagMod
        ),

        <.button(
          ^.marginLeft := "15px",
          ^.onClick   --> reload.modState(_ + 1),
          "🔄"
        ),

        <.div(
          ^.marginLeft := "8px",
          ^.className := "chart-wrapper-frame",
          <.div(^.style := js.Dictionary("width" -> "100%", "height" -> "600px").asInstanceOf[js.Object],
            <.canvas.withRef(canvasRef)
          )
        )

      )

    }

  val Component = React.memo(Componentʹ)
