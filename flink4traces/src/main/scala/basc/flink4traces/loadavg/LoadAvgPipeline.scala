package basc
package flink4traces
package loadavg

import org.apache.flink.api.common.eventtime.WatermarkStrategy
import org.apache.flink.streaming.api.datastream.DataStream
import org.apache.flink.api.common.eventtime.SerializableTimestampAssigner

import functions.{ StatefulLoadAvgFunction, LoadAvg1msBurstFunction }
import util.ExactTimestampWindowAssigner


object LoadAvgPipeline:

  def apply(tracesStream: DataStream[Traces], topic: String, port: Int): Unit =

    val watermarkStrategy = WatermarkStrategy
      .forMonotonousTimestamps[Traces]
      .withTimestampAssigner((traces, _) => traces.started)

    val timestampedStream: DataStream[Traces] = tracesStream
      .assignTimestampsAndWatermarks(watermarkStrategy)

    val loadAvgStream: DataStream[LoadAvg1msBurst] = timestampedStream
      .keyBy { traces => traces.agent + "-" + traces.label }
      .process(new StatefulLoadAvgFunction())
      .windowAll(ExactTimestampWindowAssigner)
      .process(LoadAvg1msBurstFunction)

    val wsBroadcastSink: WebSocketSink = WebSocketSink(port, s"traces-loadavg-$topic")

    loadAvgStream.sinkTo(wsBroadcastSink)
