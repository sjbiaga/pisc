package basc
package flink4traces
package cdf

import java.time.Duration

import org.apache.flink.api.common.eventtime.WatermarkStrategy

import org.apache.flink.streaming.api.datastream.DataStream

import org.apache.flink.streaming.api.windowing.assigners.TumblingProcessingTimeWindows

import functions.{ CausalPathTrackerFunction, RollingQuantileAccumulator }


object CDFPipeline:

  def apply(tracesStream: DataStream[Traces],
            windowDuration: Long,
            allowedLateness: Long,
            probabilityThreshold: Double,
            topic: String): Unit =

    val watermarkStrategy = WatermarkStrategy
      .forMonotonousTimestamps[Traces]
      .withTimestampAssigner((traces, _) => (traces.clock * 1000).toLong)

    val timestampedStream: DataStream[Traces] = tracesStream
      .assignTimestampsAndWatermarks(watermarkStrategy)

    val pathTraceStream = timestampedStream
      .keyBy(_.keyBy)
      .process(new CausalPathTrackerFunction(allowedLateness, probabilityThreshold))

    val lowProbPathTraceStream: DataStream[PathTrace] = pathTraceStream
      .getSideOutput(StreamTags.lowProbabilityTag)

    val lowProbPercentilesStream: DataStream[CDFPercentiles] = lowProbPathTraceStream
      .keyBy(_.hid)
      .window(TumblingProcessingTimeWindows.of(Duration.ofMillis(windowDuration)))
      .process(new RollingQuantileAccumulator(topic, "low_probability"))

    val highProbPercentilesStream: DataStream[CDFPercentiles] = pathTraceStream
      .keyBy(_.hid)
      .window(TumblingProcessingTimeWindows.of(Duration.ofMillis(windowDuration)))
      .process(new RollingQuantileAccumulator(topic, "high_probability"))
