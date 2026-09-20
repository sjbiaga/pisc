package basc
package flink4traces
package velocityreport

import java.time.Duration

import org.apache.flink.api.common.eventtime.WatermarkStrategy
import org.apache.flink.streaming.api.datastream.DataStream
import org.apache.flink.api.common.eventtime.SerializableTimestampAssigner

import org.apache.flink.streaming.api.functions.windowing.ProcessWindowFunction
import org.apache.flink.streaming.api.windowing.assigners.SlidingEventTimeWindows
import org.apache.flink.streaming.api.windowing.windows.TimeWindow
import org.apache.flink.util.Collector

import functions.{ CausalDepthTrackerFunction, ClockStepAttributor, StatisticalVelocityAlerter, VelocityAggregator }
import functions.VelocityAggregator.WindowDepthAccumulator


object VelocityReportPipeline:

  class VelocityWindowProcessor extends ProcessWindowFunction[WindowDepthAccumulator, WindowVelocityReport, String, TimeWindow]:

    override def process(key: String,
                         context: ProcessWindowFunction[WindowDepthAccumulator, WindowVelocityReport, String, TimeWindow]#Context,
                         elements: java.lang.Iterable[WindowDepthAccumulator],
                         out: Collector[WindowVelocityReport]): Unit =
      val i = key.indexOf(' ')
      val j = i + 1 + key.substring(i + 1).indexOf(' ')

      val (pid, name, label) = (key.substring(0, i).toLong, key.substring(i + 1, j), key.substring(j + 1))

      val acc = elements.iterator.next

      val deltaDepth = if acc.maxDepth >= acc.minDepth && acc.totalEvents > 0 then acc.maxDepth - acc.minDepth else 0L
      val deltaClock = if acc.maxClock >= acc.minClock && acc.totalEvents > 0 then acc.maxClock - acc.minClock else .0

      // Structural Velocity: How many execution layers are added per unit of stochastic time
      val velocity = try deltaDepth.toDouble / deltaClock catch _ => Double.PositiveInfinity

      out.collect:
        WindowVelocityReport(name,
                             pid,
                             label,
                             context.window.getEnd.toDouble / 1000,
                             deltaDepth,
                             deltaClock,
                             velocity,
                             acc.totalEvents)


  def apply(tracesStream: DataStream[Traces],
            windowDuration: Long,
            windowInterval: Long,
            keepPast: Double,
            purgeThreshold: Int,
            microscopicThreshold: Double,
            zScoreThreshold: Double,
            topic: String,
            port: Int): Unit =

    val watermarkStrategy = WatermarkStrategy
      .forMonotonousTimestamps[Traces]
      .withTimestampAssigner((traces, _) => (traces.clock * 1000).toLong)

    val timestampedStream: DataStream[Traces] = tracesStream
      .assignTimestampsAndWatermarks(watermarkStrategy)

    val depthTraceStream: DataStream[DepthTrace] = timestampedStream
      .keyBy(_.pid)
      .process(new CausalDepthTrackerFunction(keepPast, purgeThreshold))

    val velocityReportStream: DataStream[WindowVelocityReport] = depthTraceStream
      .keyBy { it => s"${it.pid} ${it.name} ${it.label}" }
      .window(SlidingEventTimeWindows.of(Duration.ofMillis(windowDuration), Duration.ofMillis(windowInterval)))
      .aggregate(new VelocityAggregator(), new VelocityWindowProcessor())

    val attributionReportStream: DataStream[AttributionReport] = depthTraceStream
      .keyBy(_.name)
      .process(new ClockStepAttributor(microscopicThreshold))

    val alertStream: DataStream[StructuralVelocityAlert] = velocityReportStream
      .keyBy(_.name)
      .process(new StatisticalVelocityAlerter(zScoreThreshold))

    val attributionReportStreamʹ = attributionReportStream.map(it => MixedVelocityReport(Some(it), None, None))
    val velocityReportStreamʹ = velocityReportStream.map(it => MixedVelocityReport(None, Some(it), None))
    val alertStreamʹ = alertStream.map(it => MixedVelocityReport(None, None, Some(it)))

    val mixedVelocityReportStream: DataStream[MixedVelocityReport] = alertStreamʹ
      .union(attributionReportStreamʹ)
      .union(velocityReportStreamʹ)

    val wsBroadcastSink: WebSocketSink = WebSocketSink(port, s"traces-velocityreport-$topic")

    mixedVelocityReportStream.sinkTo(wsBroadcastSink)
