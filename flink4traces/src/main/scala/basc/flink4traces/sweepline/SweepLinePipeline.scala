package basc
package flink4traces
package sweepline

import org.apache.flink.api.common.eventtime.WatermarkStrategy
import org.apache.flink.streaming.api.datastream.DataStream
import org.apache.flink.api.common.eventtime.SerializableTimestampAssigner

import functions.{ StatefulSweepLineFunction, SweepLine1msBurstFunction }
import util.ExactTimestampWindowAssigner


object SweepLinePipeline:

  def apply(tracesStream: DataStream[Traces], window: Long, topic: String, port: Int): Unit =

    val watermarkStrategy = WatermarkStrategy
      .forMonotonousTimestamps[Traces]
      .withTimestampAssigner((traces, _) => traces.started)

    val timestampedStream: DataStream[Traces] = tracesStream
      .assignTimestampsAndWatermarks(watermarkStrategy)

    val sweepLineStream: DataStream[SweepLine1msBurst] = timestampedStream
      .keyBy(_.keyBy)
      .process(new StatefulSweepLineFunction(window * 1000))
      .windowAll(ExactTimestampWindowAssigner)
      .process(new SweepLine1msBurstFunction())

    val wsBroadcastSink: WebSocketSink = WebSocketSink(port, s"traces-sweepline-$topic")

    sweepLineStream.sinkTo(wsBroadcastSink)
