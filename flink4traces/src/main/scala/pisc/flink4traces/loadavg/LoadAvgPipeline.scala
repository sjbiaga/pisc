package pisc
package flink4traces
package loadavg

import org.apache.flink.api.common.eventtime.WatermarkStrategy
import org.apache.flink.streaming.api.datastream.DataStream
import org.apache.flink.api.common.eventtime.SerializableTimestampAssigner

import functions.{ StatefulLoadAvgFunction, LoadAvg1msBurstFunction }
import util.ExactTimestampWindowAssigner
import websocket.WebSocketSink


object LoadAvgPipeline:

  def apply(tracesStream: DataStream[Traces], topic: String, port: Int): Unit =

    val watermarkStrategy = WatermarkStrategy
      .forMonotonousTimestamps[Traces]
      .withTimestampAssigner((traces, _) => traces.started)

    val timestampedStream: DataStream[Traces] = tracesStream
      .assignTimestampsAndWatermarks(watermarkStrategy)

    val loadAvgStream: DataStream[LoadAvg1msBurst] = timestampedStream
      .keyBy(_.keyBy)
      .process(new StatefulLoadAvgFunction())
      .windowAll(ExactTimestampWindowAssigner)
      .process(new LoadAvg1msBurstFunction())

    val wsBroadcastSink: WebSocketSink[LoadAvg1msBurst] =
      WebSocketSink(port,
                    s"traces-loadavg-$topic",
                    { element => {
                        case uuid if element.perUUIDLoadAvg1msBurst.containsKey(uuid) =>
                          Some(element.perUUIDLoadAvg1msBurst.get(uuid).toJson)
                        case _ =>
                          None
                      }
                    })

    loadAvgStream.sinkTo(wsBroadcastSink)
