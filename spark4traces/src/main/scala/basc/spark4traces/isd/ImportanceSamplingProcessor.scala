package basc
package spark4traces
package isd

import scala.collection.mutable.ListBuffer

import org.apache.spark.sql.streaming.GroupState

import rca.PathStep


case class ImportanceSamplingProcessor(degeneracyThreshold: Double, blowoutThreshold: Double)
    extends Function3[String, Iterator[Traces], GroupState[TraceState], Iterator[CounterfactualWeightResult]] {

  override def apply(hid: String, traces: Iterator[Traces], state: GroupState[TraceState]): Iterator[CounterfactualWeightResult] = {

    var currentState = state.getOption.getOrElse(
      TraceState(hid,
                 cumulativeLogWeight = .0,
                 stepCount = 0L,
                 lastUpdated = 0L,
                 dagNodes = Map.empty,
                 bestPath = Nil,
                 maxPathWeight = Double.MinValue)
    )

    val results = new ListBuffer[CounterfactualWeightResult]()

    traces.foreach { value =>

      value.plugins.get("parents") -> value.plugins.get("whatIf") -> value.delay match {

        case ((Some(Plugin(_, Some(parents), _, _, _)), Some(Plugin(_, _, _, _, Some(((num, den), diff))))), Some(delay))
            if !currentState.observed.contains(value.number) =>

          val stepLogWeight = math.log(num.toDouble) - math.log(den.toDouble) - (diff * BigDecimal(delay)).toDouble

          val totalLogWeight = currentState.cumulativeLogWeight + stepLogWeight
          val updatedCount = currentState.stepCount + 1

          val traceNode = TraceNode(parents,
                                    value.name,
                                    value.agent + '-' + value.name,
                                    stepLogWeight,
                                    Some(delay))

          val dagNodes = currentState.dagNodes + (value.number -> traceNode)

          val isDegeneracyAlert = totalLogWeight < degeneracyThreshold
          val isBlowoutAlert = math.abs(totalLogWeight) > blowoutThreshold

          if (isDegeneracyAlert || isBlowoutAlert) {
            val (maxPathWeight, bestPath) = ImportanceSamplingProcessor(dagNodes)
            if (maxPathWeight > currentState.maxPathWeight)
              currentState = currentState.copy(observed = Some(value.number),
                                               cumulativeLogWeight = totalLogWeight,
                                               stepCount = updatedCount,
                                               lastUpdated = (value.clock * 1000).toLong,
                                               dagNodes = dagNodes,
                                               bestPath = bestPath,
                                               maxPathWeight = maxPathWeight)
          }
          if (currentState.dagNodes ne dagNodes)
            currentState = currentState.copy(observed = Some(value.number),
                                             cumulativeLogWeight = totalLogWeight,
                                             stepCount = updatedCount,
                                             lastUpdated = (value.clock * 1000).toLong,
                                             dagNodes = dagNodes)

          results += CounterfactualWeightResult(hid,
                                                value.number,
                                                totalLogWeight,
                                                math.exp(totalLogWeight),
                                                updatedCount,
                                                new java.sql.Timestamp((value.clock * 1000).toLong),
                                                currentState.bestPath,
                                                isDegeneracyAlert,
                                                isBlowoutAlert)

        case ((Some(Plugin(_, Some(parents), _, _, _)), Some(Plugin(_, _, _, _, Some(_)))), None)
            if !currentState.observed.contains(value.number) =>

          val traceNode = TraceNode(parents,
                                    value.name,
                                    value.agent + '-' + value.name,
                                    Double.NaN,
                                    None)

          currentState = currentState.copy(observed = Some(value.number),
                                           dagNodes = currentState.dagNodes + (value.number -> traceNode))

        case _ =>
          currentState = currentState.copy(observed = Some(value.number))

      }

    }

    results.iterator
  }

}


object ImportanceSamplingProcessor {

  def apply(dagNodes: Map[Long, TraceNode]): (Double, List[PathStep]) = {
    val parents = dagNodes.values.flatMap(_.parents).toSet
    val leafNodes = dagNodes.keys.filterNot(parents.contains(_)).toList

    var maxPathWeight = Double.MinValue
    var bestPath = List.empty[PathStep]

    leafNodes.foreach { leaf =>
      val nodes = ListBuffer.empty[(Long, Double, List[PathStep])]

      nodes.append((leaf, .0, Nil))

      while (nodes.nonEmpty) {
        var (number, pathWeight, pathAcc) = nodes.remove(0)

        if (dagNodes.contains(number)) {
          val TraceNode(parents, name, label, stepLogWeight, delay) = dagNodes(number)

          if (delay.isDefined) {
            pathWeight += stepLogWeight
            pathAcc ::= PathStep(number, name, label, stepLogWeight, delay.get)
          }

          if (parents.size < 2)
            if (pathWeight > maxPathWeight) {
              maxPathWeight = pathWeight
              bestPath = pathAcc
            }

          parents.foreach { node => nodes.append((node, pathWeight, pathAcc)) }
        }
      }
    }

    maxPathWeight -> bestPath
  }

}
