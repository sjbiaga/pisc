package basc
package flink4traces
package functions

import org.apache.flink.api.common.state.{ ValueState, ValueStateDescriptor }
import org.apache.flink.streaming.api.functions.KeyedProcessFunction
import org.apache.flink.util.Collector

import velocityreport.{ WindowVelocityReport, StructuralVelocityAlert }
import StatisticalVelocityAlerter.*


class StatisticalVelocityAlerter(zScoreThreshold: Double)
    extends KeyedProcessFunction[String, WindowVelocityReport, StructuralVelocityAlert]:

  private var statsState: ValueState[WelfordState] = null

  override def open(openContext: org.apache.flink.api.common.functions.OpenContext): Unit =
    statsState = getRuntimeContext.getState(
      new ValueStateDescriptor[WelfordState]("welford-velocity-state", classOf[WelfordState])
    )

  override def processElement(value: WindowVelocityReport,
                              ctx: KeyedProcessFunction[String, WindowVelocityReport, StructuralVelocityAlert]#Context,
                              out: Collector[StructuralVelocityAlert]): Unit =
    if statsState.value eq null
    then
      statsState.update(WelfordState(0L, .0, .0))

    val stats = statsState.value

    val velocity = value.structuralVelocity

    if !java.lang.Double(velocity).isInfinite && stats.count > 5
    then
      val variance = if stats.count > 1 then stats.M2 / (stats.count - 1) else .0
      val stdDev = math.sqrt(variance)

      if stdDev > .0
      then
        val zScore = (velocity - stats.mean) / stdDev

        // If the velocity jumps completely out of bounds, raise a structural alert
        if zScore > zScoreThreshold
        then
          out.collect:
            StructuralVelocityAlert(value.name,
                                    value.pid,
                                    value.clock,
                                    velocity,
                                    stats.mean,
                                    stdDev,
                                    zScore)

    if !java.lang.Double(velocity).isInfinite
    then
      val newCount = stats.count + 1
      val delta = velocity - stats.mean
      val newMean = stats.mean + (delta / newCount)
      val delta2 = velocity - newMean
      val newM2 = stats.M2 + (delta * delta2)

      statsState.update(WelfordState(newCount, newMean, newM2))


object StatisticalVelocityAlerter:

  case class WelfordState(count: Long, mean: Double, M2: Double)
