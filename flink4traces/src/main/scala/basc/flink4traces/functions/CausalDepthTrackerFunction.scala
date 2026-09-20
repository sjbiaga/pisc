package basc
package flink4traces
package functions

import java.util.{ LinkedList => List }
import java.util.Map.Entry

import org.apache.flink.api.common.state.{ MapState, MapStateDescriptor }
import org.apache.flink.streaming.api.functions.KeyedProcessFunction
import org.apache.flink.util.Collector

import velocityreport.DepthTrace


class CausalDepthTrackerFunction(keepPast: Double, purgeThreshold: Int)
    extends KeyedProcessFunction[Long, Traces, DepthTrace]:

  private var depthState: MapState[Long, (Long, Double)] = null

  override def open(openContext: org.apache.flink.api.common.functions.OpenContext): Unit =
    depthState = getRuntimeContext.getMapState(
      new MapStateDescriptor[Long, (Long, Double)]("causal-depth-store", classOf[Long], classOf[(Long, Double)])
    )

  override def processElement(value: Traces,
                              ctx: KeyedProcessFunction[Long, Traces, DepthTrace]#Context,
                              out: Collector[DepthTrace]): Unit =
    if !depthState.contains(value.number)
    then
      if depthState.isEmpty
      then
        depthState.put(0L, 0L -> Double.NaN)

      var maxParentDepth = 0L
      var latestParentClock = .0

      value.causes.foreach {
        case depId if depthState.contains(depId) =>
          val (parentDepth, parentClock) = depthState.get(depId)
          if parentDepth > maxParentDepth
          then
            maxParentDepth = parentDepth
          if parentClock > latestParentClock
          then
            latestParentClock = parentClock
        case _ =>
          // Parent was either pruned by TTL or bypassed. Fallback to 0.
      }

      val currentDepth = maxParentDepth + 1
      val parentClock = if latestParentClock == .0 then value.clock else latestParentClock

      depthState.put(value.number, currentDepth -> value.clock)
      depthState.put(0L, depthState.get(0L)._1 + 1 -> Double.NaN)

      if depthState.get(0L)._1 > purgeThreshold
      then
        val keysToRemove = List[Long]()
        val cutoff = value.clock - keepPast
        depthState.entries.forEach { entry =>
          val number = entry.getKey
          val (_, clock) = entry.getValue
          if clock < cutoff
          then
            keysToRemove.add(number)
        }
        keysToRemove.forEach(depthState.remove)

      out.collect:
        DepthTrace(value.name,
                   ctx.getCurrentKey,
                   value.agent + '-' + value.label,
                   value.clock,
                   currentDepth,
                   parentClock)
