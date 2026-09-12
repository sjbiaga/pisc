package basc
package flink4traces
package functions

import org.apache.flink.api.common.functions.FlatMapFunction
import org.apache.flink.util.Collector


class IntervalDiscretizer(val stepSeconds: Long)
    extends FlatMapFunction[Traces, (Long, String, Long, Double)]:

  override def flatMap(value: Traces, out: Collector[(Long, String, Long, Double)]): Unit =
    val startTime = value.started / 1000
    val endTime = value.ended / 1000

    val firstTick = ((startTime + stepSeconds - 1) / stepSeconds) * stepSeconds
    val lastTick = (endTime / stepSeconds) * stepSeconds

    var currentTick = firstTick
    while currentTick <= lastTick
    do
      out.collect((currentTick * 1000, value.label, value.pid, value.clock))
      currentTick += stepSeconds
