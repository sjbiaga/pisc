package pisc
package beam4traces
package isd

import org.apache.beam.sdk.state.{ StateSpec, StateSpecs, ValueState, MapState }
import org.apache.beam.sdk.transforms.DoFn
import org.apache.beam.sdk.transforms.DoFn.{ ProcessElement, StateId }
import org.apache.beam.sdk.values.KV
import scala.jdk.CollectionConverters._


class StatefulImportanceSamplingFn(degeneracyThreshold: Double, blowoutThreshold: Double)
    extends DoFn[KV[String, Traces], CounterfactualWeightResult] {

  private val now = System.currentTimeMillis

  @StateId("dagNodes")
  private val dagNodesSpec: StateSpec[MapState[java.lang.Long, TraceNode]] = StateSpecs.map()

  @StateId("cumulativeLogWeight")
  private val cumulativeLogWeightSpec: StateSpec[ValueState[java.lang.Double]] = StateSpecs.value()

  @StateId("maxPathWeight")
  private val maxPathWeightSpec: StateSpec[ValueState[java.lang.Double]] = StateSpecs.value()

  @StateId("stepCount")
  private val stepCountSpec: StateSpec[ValueState[java.lang.Long]] = StateSpecs.value()

  @StateId("observed")
  private val observedSpec: StateSpec[ValueState[java.lang.Long]] = StateSpecs.value()

  @ProcessElement
  def processElement(
    c: DoFn[KV[String, Traces], CounterfactualWeightResult]#ProcessContext,
    @StateId("dagNodes") dagNodesState: MapState[java.lang.Long, TraceNode],
    @StateId("cumulativeLogWeight") cumulativeLogWeightState: ValueState[java.lang.Double],
    @StateId("maxPathWeight") maxPathWeightState: ValueState[java.lang.Double],
    @StateId("stepCount") stepCountState: ValueState[java.lang.Long],
    @StateId("observed") observedState: ValueState[java.lang.Long],
  ): Unit = {
    val element = c.element
    val hid = element.getKey
    val value = element.getValue

    val cumulativeLogWeight = Option(cumulativeLogWeightState.read()).map(_.toDouble).getOrElse(.0)
    val maxPathWeight = Option(maxPathWeightState.read()).map(_.toDouble).getOrElse(Double.MinValue)
    val stepCount = Option(stepCountState.read()).map(_.toLong).getOrElse(0L)
    val observed = Option(observedState.read()).map(_.toLong).getOrElse(0L)

    value.plugins.get("parents") -> value.plugins.get("whatIf") -> value.delay match {

      case ((Some(Plugin.parents(parents)), Some(Plugin.whatIf((num, den), diff))), Some(delay))
          if observed != value.number =>

        val stepLogWeight = math.log(num.toDouble) - math.log(den.toDouble) - (diff * BigDecimal(delay)).toDouble

        val totalLogWeight = cumulativeLogWeight + stepLogWeight
        val updatedCount = stepCount + 1

        val traceNode = TraceNode(parents,
                                  value.name,
                                  value.agent + '-' + value.label,
                                  stepLogWeight,
                                  Some(delay))

        dagNodesState.put(value.number, traceNode)

        val isDegeneracyAlert = totalLogWeight < degeneracyThreshold
        val isBlowoutAlert = math.abs(totalLogWeight) > blowoutThreshold

        var output = false

        if (isDegeneracyAlert || isBlowoutAlert) {
          val (maxPathWeightʹ, bestPath) = StatefulImportanceSamplingFn(dagNodesState)
          if (maxPathWeightʹ > maxPathWeight) {
            maxPathWeightState.write(maxPathWeightʹ)
            c.output {
              CounterfactualWeightResult(hid,
                                         value.number,
                                         value.clock,
                                         totalLogWeight,
                                         math.exp(totalLogWeight),
                                         updatedCount,
                                         new java.sql.Timestamp(now + (value.clock * 1000).toLong),
                                         bestPath,
                                         isDegeneracyAlert,
                                         isBlowoutAlert)
            }
            output = true
          }
        }

        if (!output)
          c.output {
            CounterfactualWeightResult(hid,
                                       value.number,
                                       value.clock,
                                       totalLogWeight,
                                       math.exp(totalLogWeight),
                                       updatedCount,
                                       new java.sql.Timestamp(now + (value.clock * 1000).toLong),
                                       Nil,
                                       false,
                                       false)
          }

        cumulativeLogWeightState.write(totalLogWeight)
        stepCountState.write(updatedCount)

      case ((Some(Plugin.parents(parents)), Some(Plugin.whatIf((num, den), diff))), None)
          if observed != value.number =>

        val traceNode = TraceNode(parents,
                                  value.name,
                                  value.agent + '-' + value.label,
                                  Double.NaN,
                                  None)

        dagNodesState.put(value.number, traceNode)

      case _ =>

    }

    observedState.write(value.number)

  }

}


object StatefulImportanceSamplingFn {

  import scala.collection.mutable.ListBuffer

  import rca.PathStep

  def apply(dagNodesState: MapState[java.lang.Long, TraceNode]): (Double, List[PathStep]) = {
    var parents = Set.empty[Long]
    var leafNodes = List.empty[Long]

    dagNodesState.values.read.forEach(parents ++= _.parents)
    dagNodesState.keys.read.forEach { number =>
      if (!parents.contains(number))
        leafNodes ::= number
    }

    var maxPathWeight = Double.MinValue
    var bestPath = List.empty[PathStep]

    leafNodes.foreach { leaf =>
      val nodes = ListBuffer.empty[(Long, Double, List[PathStep])]

      nodes.append((leaf, .0, Nil))

      while (nodes.nonEmpty) {
        var (number, pathWeight, pathAcc) = nodes.remove(0)

        dagNodesState.getOrDefault(number, null).read match {
          case null =>

          case TraceNode(parents, name, label, stepLogWeight, delay) =>

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
