package basc
package flink4traces
package functions

import org.apache.flink.streaming.api.functions.windowing.ProcessWindowFunction
import org.apache.flink.streaming.api.windowing.windows.TimeWindow
import org.apache.flink.util.Collector

import isd.{ CausalSubTreeSummary, ESSWindowDiagnostics }


class ESSWindowProcessFunction extends ProcessWindowFunction[
  CausalSubTreeSummary,
  ESSWindowDiagnostics,
  String,
  TimeWindow
]:

  override def process(
      key: String,
      context: ProcessWindowFunction[CausalSubTreeSummary, ESSWindowDiagnostics, String, TimeWindow]#Context,
      elements: java.lang.Iterable[CausalSubTreeSummary],
      out: Collector[ESSWindowDiagnostics]
  ): Unit =

    if elements.iterator.hasNext
    then

      var sampleCount = 0L
      var maxLogWeight = Double.MinValue

      elements.forEach { summary =>
        sampleCount += 1
        maxLogWeight = math.max(maxLogWeight, summary.totalLogWeight)
      }

      var sumWStar = .0
      var sumWStarSq = .0
      var sumWStarDuration = .0

      elements.forEach { summary =>
        val wStar = math.exp(summary.totalLogWeight - maxLogWeight)
        sumWStar += wStar
        sumWStarSq += wStar * wStar
        sumWStarDuration += wStar * summary.totalDuration
      }

      // Compute ESS and Efficiency Ratio
      val ess = if sumWStarSq > .0 then sumWStar * sumWStar / sumWStarSq else .0
      val efficiencyRatio = ess / sampleCount

      // Self-Normalized Importance Sampling (SNIS) Mean Latency
      val weightedMeanDuration = if sumWStar > .0 then sumWStarDuration / sumWStar else .0

      out.collect:
        ESSWindowDiagnostics(key,
                             context.window.getStart,
                             context.window.getEnd,
                             sampleCount,
                             ess,
                             efficiencyRatio,
                             maxLogWeight,
                             weightedMeanDuration)
