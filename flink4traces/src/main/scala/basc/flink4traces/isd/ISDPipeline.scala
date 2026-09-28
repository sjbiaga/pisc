package basc
package flink4traces
package isd

import java.time.Duration

import org.apache.flink.api.common.eventtime.WatermarkStrategy

import org.apache.flink.streaming.api.datastream.DataStream

import org.apache.flink.streaming.api.windowing.assigners.SlidingEventTimeWindows

import functions.{ CausalSubTreeWeightAggregator, ESSWindowProcessFunction }
import websocket.WebSocketSink


object ISDPipeline:

  def apply(tracesStream: DataStream[Traces],
            windowDuration: Long,
            windowInterval: Long,
            allowedLateness: Long,
            weightBlowoutThreshold: Double,
            port: Int,
            topic: String): Unit =

    val watermarkStrategyTraces = WatermarkStrategy
      .forMonotonousTimestamps[Traces]
      .withTimestampAssigner((traces, _) => (traces.clock * 1000).toLong)

    val timestampedStream: DataStream[Traces] = tracesStream
      .assignTimestampsAndWatermarks(watermarkStrategyTraces)

    val summaryStream = timestampedStream
      .keyBy(_.keyBy)
      .process(new CausalSubTreeWeightAggregator(allowedLateness, weightBlowoutThreshold))

    val rcaStream = summaryStream
      .getSideOutput(StreamTags.rcaTag)

    val watermarkStrategySummary = WatermarkStrategy
      .forMonotonousTimestamps[CausalSubTreeSummary]
      .withTimestampAssigner((summary, _) => summary.windowEndTime)

    val essDiagnosticsStream = summaryStream
      .assignTimestampsAndWatermarks(watermarkStrategySummary)
      .keyBy(_.hid)
      .window(SlidingEventTimeWindows.of(Duration.ofMillis(windowDuration), Duration.ofMillis(windowInterval)))
      .process(new ESSWindowProcessFunction())

    // Filter / Alerting Stream for Degenerate Sampling Efficiency
    val essAlertStream = essDiagnosticsStream.filter(_.effectiveSampleSize < .1)

    val essDiagnosticsStreamʹ = essDiagnosticsStream.map(it => ESSMixedDiagnostics(Left(it)))
    val essAlertStreamʹ = essAlertStream.map(it => ESSMixedDiagnostics(Right(it)))

    val mixedESSDiagnosticStream = essDiagnosticsStreamʹ.union(essAlertStreamʹ)

    val wsBroadcastSink: WebSocketSink[ESSMixedDiagnostics] =
      WebSocketSink(port, s"traces-isd-$topic")

    mixedESSDiagnosticStream.sinkTo(wsBroadcastSink)
