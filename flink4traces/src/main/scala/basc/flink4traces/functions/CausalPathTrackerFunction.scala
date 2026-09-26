package basc
package flink4traces
package functions

import java.util.{ HashSet => Set }

import org.apache.flink.api.common.state.{ ValueState, ValueStateDescriptor }
import org.apache.flink.streaming.api.functions.KeyedProcessFunction
import org.apache.flink.util.Collector

import cdf.{ PathTrace, StreamTags }
import CausalPathTrackerFunction.*


class CausalPathTrackerFunction(allowedLateness: Long, probabilityThreshold: Double)
    extends KeyedProcessFunction[String, Traces, PathTrace]:

  private var state: ValueState[CausalState] = null

  // Tracks minimum event probability observed along the nested trace tree path
  private var pathProbState: ValueState[java.lang.Double] = null

  override def open(openContext: org.apache.flink.api.common.functions.OpenContext): Unit =
    state = getRuntimeContext.getState(
      new ValueStateDescriptor[CausalState]("causal-grapth-state", classOf[CausalState])
    )
    pathProbState = getRuntimeContext.getState(
      new ValueStateDescriptor[java.lang.Double]("path-prob-state", classOf[java.lang.Double])
    )

  override def processElement(value: Traces,
                              ctx: KeyedProcessFunction[String, Traces, PathTrace]#Context,
                              out: Collector[PathTrace]): Unit =
    if state.value eq null
    then
      state.update(CausalState())

    val currentCausalState = state.value

    if !currentCausalState.observed.contains(value.number)
    then
      currentCausalState.observed.add(value.number)
      currentCausalState.lastEventTimestamp = ctx.timerService.currentProcessingTime

      if value.causes.isEmpty
      then
        currentCausalState.root = value.number
        currentCausalState.rootStartTime = value.clock

      currentCausalState.maxLeafTime = math.max(currentCausalState.maxLeafTime, value.clock)

      state.update(currentCausalState)

      value.plugins.find(_.isInstanceOf[Plugin.probability]) match
        case Some(probPlugin: Plugin.probability) =>
          val currentMinProb = Option(pathProbState.value).getOrElse(java.lang.Double(1.0))
          pathProbState.update(math.min(currentMinProb, probPlugin.probability.doubleValue))
        case _ =>
          pathProbState.update(1)

      ctx.timerService.registerProcessingTimeTimer(currentCausalState.lastEventTimestamp + allowedLateness)

  override def onTimer(timestamp: Long,
                       ctx: KeyedProcessFunction[String, Traces, PathTrace]#OnTimerContext,
                       out: Collector[PathTrace]): Unit =

    val currentCausalState = state.value

    if currentCausalState ne null
    then
      if timestamp >= currentCausalState.lastEventTimestamp + allowedLateness
      then
        if currentCausalState.root != 0L && currentCausalState.maxLeafTime >= currentCausalState.rootStartTime
        then
          val totalDelay = (currentCausalState.maxLeafTime - currentCausalState.rootStartTime) * 1000
          val finalProbability = Option(pathProbState.value).getOrElse(java.lang.Double(1.0))

          val pathTrace = PathTrace(ctx.getCurrentKey,
                                    currentCausalState.root,
                                    totalDelay,
                                    currentCausalState.observed.size,
                                    finalProbability)

          if finalProbability > probabilityThreshold
          then
            out.collect(pathTrace)
          else
            ctx.output(StreamTags.lowProbabilityTag, pathTrace)

        currentCausalState.maxLeafTime = Double.MinValue
        currentCausalState.observed = Set()
        currentCausalState.lastEventTimestamp = 0L
        pathProbState.clear


object CausalPathTrackerFunction:

  case class CausalState(var rootStartTime: Double = Double.MaxValue,
                         var root: Long = 0L,
                         var maxLeafTime: Double = Double.MinValue,
                         var observed: Set[Long] = Set(),
                         var lastEventTimestamp: Long = 0L)
