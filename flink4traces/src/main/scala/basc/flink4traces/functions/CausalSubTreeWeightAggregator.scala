package basc
package flink4traces
package functions

import java.math.BigDecimal
import java.util.{ HashSet => Set, LinkedList }

import org.apache.flink.api.common.state.{ MapState, MapStateDescriptor, ValueState, ValueStateDescriptor }
import org.apache.flink.streaming.api.functions.KeyedProcessFunction
import org.apache.flink.util.Collector

import isd.{ CausalSubTreeSummary, StreamTags }
import isd.rca.{ PathStep, RootCauseReport }
import CausalSubTreeWeightAggregator.*


class CausalSubTreeWeightAggregator(allowedLatenessMs: Long,
                                    weightBlowoutThreshold: Double)
    extends KeyedProcessFunction[String, Traces, CausalSubTreeSummary]:

  @transient private var traceNodeState: MapState[Long, TraceNode] = null

  @transient private var observedState: ValueState[Long] = null

  @transient private var treeAccumulatorState: ValueState[TreeAccumulator] = null

  override def open(openContext: org.apache.flink.api.common.functions.OpenContext): Unit =
    traceNodeState = getRuntimeContext.getMapState(
      new MapStateDescriptor[Long, TraceNode]("traceNodeState", classOf[Long], classOf[TraceNode])
    )

    observedState = getRuntimeContext.getState(
      new ValueStateDescriptor[Long]("observedState", classOf[Long])
    )

    treeAccumulatorState = getRuntimeContext.getState(
      new ValueStateDescriptor[TreeAccumulator]("treeAccumulatorState", classOf[TreeAccumulator])
    )

  override def processElement(value: Traces,
                              ctx: KeyedProcessFunction[String, Traces, CausalSubTreeSummary]#Context,
                              out: Collector[CausalSubTreeSummary]): Unit =
    if observedState.value eq null
    then
      observedState.update(0L)

    value.plugins.find(_.isInstanceOf[Plugin.parents]) -> value.plugins.find(_.isInstanceOf[Plugin.whatIf]) -> value.delay match
      case ((Some(Plugin.parents(parents)), Some(Plugin.whatIf((num, den), diff))), Some(delay)) if observedState.value != value.number =>
        val stepLogWeight = math.log(num.doubleValue) - math.log(den.doubleValue) - diff.multiply(BigDecimal(delay)).doubleValue

        traceNodeState.put(value.number, TraceNode(parents, value.name, value.agent + '-' + value.label, stepLogWeight, Some(delay)))

        val acc = Option(treeAccumulatorState.value).getOrElse(TreeAccumulator(.0, 0, .0, 0L))

        val timestamp = (value.clock * 1000).toLong

        treeAccumulatorState.update:
          TreeAccumulator(acc.runningLogWeight + stepLogWeight,
                          acc.eventCount + 1,
                          acc.totalDuration + delay,
                          timestamp)

        ctx.timerService.registerEventTimeTimer(timestamp + allowedLatenessMs)

      case ((Some(Plugin.parents(parents)), Some(Plugin.whatIf(_, _))), None) if observedState.value != value.number =>
        traceNodeState.put(value.number, TraceNode(parents, null, null, Double.NaN, None))

      case _ =>

    observedState.update(value.number)


  override def onTimer(timestamp: Long,
                       ctx: KeyedProcessFunction[String, Traces, CausalSubTreeSummary]#OnTimerContext,
                       out: Collector[CausalSubTreeSummary]): Unit =
    val acc = treeAccumulatorState.value
    if acc ne null
    then
      out.collect:
        CausalSubTreeSummary(ctx.getCurrentKey,
                             acc.runningLogWeight,
                             acc.eventCount,
                             acc.totalDuration,
                             timestamp)

      if math.abs(acc.runningLogWeight) >= weightBlowoutThreshold
      then
        val parents = Set[Long]()
        var leafNodes = List.empty[Long]

        traceNodeState.values.forEach(_.parents.foreach(parents.add))
        traceNodeState.keys.forEach { number =>
          if !parents.contains(number)
          then
            leafNodes ::= number
        }

        var maxPathWeight = Double.MinValue
        var bestPath = List.empty[PathStep]

        leafNodes.foreach { leaf =>
          var nodes = LinkedList[(Long, Double, List[PathStep])]()

          nodes.addLast((leaf, .0, List.empty[PathStep]))

          while !nodes.isEmpty
          do
            var (number, pathWeight, pathAcc) = nodes.removeFirst
            if traceNodeState.contains(number)
            then
              val TraceNode(parents, name, label, stepLogWeight, delay) = traceNodeState.get(number)
              if delay.isDefined
              then
                pathWeight += stepLogWeight
                pathAcc ::= PathStep(number, name, label, stepLogWeight, delay.get)

              if parents.size < 2
              then
                if pathWeight > maxPathWeight
                then
                  maxPathWeight = pathWeight
                  bestPath = pathAcc

              parents.foreach(nodes.addLast(_, pathWeight, pathAcc))
        }

        if bestPath.nonEmpty
        then
          val contributionRatio = if acc.runningLogWeight != .0 then maxPathWeight / acc.runningLogWeight else .0

          val rcr = RootCauseReport(ctx.getCurrentKey,
                                    acc.runningLogWeight,
                                    maxPathWeight,
                                    bestPath.length,
                                    contributionRatio,
                                    bestPath,
                                    timestamp)

          ctx.output(StreamTags.rcaTag, rcr)


object CausalSubTreeWeightAggregator:

  case class TraceNode(parents: scala.collection.immutable.Set[Long],
                       name: String,
                       label: String,
                       stepLogWeight: Double,
                       delay: Option[Double])

  case class TreeAccumulator(runningLogWeight: Double,
                             eventCount: Int,
                             totalDuration: Double,
                             lastTimestamp: Long)
