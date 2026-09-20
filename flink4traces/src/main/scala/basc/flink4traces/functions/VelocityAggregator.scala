package basc
package flink4traces
package functions

import org.apache.flink.api.common.functions.AggregateFunction

import velocityreport.*
import VelocityAggregator.*


class VelocityAggregator extends AggregateFunction[DepthTrace, WindowDepthAccumulator, WindowDepthAccumulator]:

  override def createAccumulator: WindowDepthAccumulator = WindowDepthAccumulator()

  override def add(value: DepthTrace, acc: WindowDepthAccumulator): WindowDepthAccumulator =
    if value.depth < acc.minDepth then acc.minDepth = value.depth
    if value.depth > acc.maxDepth then acc.maxDepth = value.depth
    if value.clock < acc.minClock then acc.minClock = value.clock
    if value.clock > acc.maxClock then acc.maxClock = value.clock
    acc.totalEvents += 1
    acc

  override def getResult(acc: WindowDepthAccumulator): WindowDepthAccumulator = acc

  override def merge(a: WindowDepthAccumulator, b: WindowDepthAccumulator): WindowDepthAccumulator =
    WindowDepthAccumulator(
      math.min(a.minDepth, b.minDepth),
      math.max(a.maxDepth, b.maxDepth),
      math.min(a.minClock, b.minClock),
      math.max(a.maxClock, b.maxClock),
      a.totalEvents + b.totalEvents
    )


object VelocityAggregator:

  case class WindowDepthAccumulator(var minDepth: Long = Long.MaxValue,
                                    var maxDepth: Long = Long.MinValue,
                                    var minClock: Double = Double.MaxValue,
                                    var maxClock: Double = Double.MinValue,
                                    var totalEvents: Long = 0L)
