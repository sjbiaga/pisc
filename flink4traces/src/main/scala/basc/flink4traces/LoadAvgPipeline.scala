package basc
package flink4traces

import java.time.Duration

import java.util.{ HashMap => Map }

import org.apache.flink.api.common.eventtime.WatermarkStrategy
import org.apache.flink.util.Collector
import org.apache.flink.streaming.api.datastream.{ DataStream, DataStreamSink }
import org.apache.flink.streaming.api.functions.windowing.ProcessWindowFunction
import org.apache.flink.streaming.api.windowing.assigners.TumblingEventTimeWindows
import org.apache.flink.streaming.api.windowing.windows.TimeWindow

import functions.{ IntervalDiscretizer, EwmaLoadCalculator, EwmaLoadResult }
import websocket.WebSocketSink


object LoadAvgPipeline:

  object CustomWindowProcessor extends ProcessWindowFunction[EwmaLoadResult, LoadAvg, String, TimeWindow]:
    override def process(
      key: String,
      context: ProcessWindowFunction[EwmaLoadResult, LoadAvg, String, TimeWindow]#Context,
      elements: java.lang.Iterable[EwmaLoadResult],
      out: Collector[LoadAvg]
    ): Unit =
      val (perPIDResult, globalResult) = elements.iterator.next
      val windowEndTimestamp = context.window.getEnd
      val perPIDLoadAvg = Map[Long, LoadAvg]()
      perPIDResult.forEach {
        case (pid, (clock, (load1m, load10m, load15m))) =>
          perPIDLoadAvg.put(pid, LoadAvg(windowEndTimestamp, Map(), clock, load1m, load10m, load15m))
      }
      val (clock, (load1m, load10m, load15m)) = globalResult
      out.collect(LoadAvg(windowEndTimestamp, perPIDLoadAvg, clock, load1m, load10m, load15m))


  def apply(tracesStream: DataStream[Traces], topic: String, port: Int): Unit =

    val discreteTicksStream: DataStream[(Long, String, Long, Double)] = tracesStream
      .flatMap(IntervalDiscretizer(5))

    val watermarkStrategy = WatermarkStrategy
      .forMonotonousTimestamps[(Long, String, Long, Double)].withTimestampAssigner((event, _) => event._1)
      // Align partitions so fast partitions don't drift more than 1 second ahead of slow ones
      .withWatermarkAlignment("kafka-alignment-group", Duration.ofSeconds(1), Duration.ofSeconds(5))

    val timestampedStream: DataStream[(Long, String, Long, Double)] = discreteTicksStream
      .assignTimestampsAndWatermarks(watermarkStrategy)

    val loadAvgStream = timestampedStream
      .keyBy(_._2)
      .window(TumblingEventTimeWindows.of(Duration.ofSeconds(5)))
      .aggregate(new EwmaLoadCalculator(5), CustomWindowProcessor)

    val wsBroadcastSink: WebSocketSink = WebSocketSink(port, s"traces-loadavg-$topic")

    loadAvgStream.sinkTo(wsBroadcastSink).setParallelism(1)
