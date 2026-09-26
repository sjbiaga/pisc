package basc
package flink4traces
package functions

import java.util.concurrent.{ ConcurrentHashMap => Map }

import org.apache.flink.streaming.api.functions.windowing.ProcessWindowFunction
import org.apache.flink.streaming.api.windowing.assigners.TumblingProcessingTimeWindows
import org.apache.flink.streaming.api.windowing.windows.TimeWindow

import org.apache.flink.api.common.functions.OpenContext
import org.apache.flink.metrics.{ Gauge, MetricGroup }
import org.apache.flink.util.Collector

import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

import cdf.{ PathTrace, CDFPercentiles }
import RollingQuantileAccumulator.*


class RollingQuantileAccumulator(topic: String, profile: String)
    extends ProcessWindowFunction[PathTrace, CDFPercentiles, String, TimeWindow]:

  private var baseMetricGroup: MetricGroup = null

  private val p50GaugeCache = Map[(String, String), UpdatableGauge]()
  private val p90GaugeCache = Map[(String, String), UpdatableGauge]()
  private val p95GaugeCache = Map[(String, String), UpdatableGauge]()
  private val p99GaugeCache = Map[(String, String), UpdatableGauge]()

  override def open(openContext: OpenContext): Unit =
    baseMetricGroup = getRuntimeContext
      .getMetricGroup
      .addGroup("bioambients_flink4traces_cdf")
      .addGroup("topic", topic)
      .addGroup("profile", profile)

  override def process(key: String,
                       context: ProcessWindowFunction[PathTrace, CDFPercentiles, String, TimeWindow]#Context,
                       elements: java.lang.Iterable[PathTrace],
                       out: Collector[CDFPercentiles]): Unit =
    val uuid = key.substring(0, key.indexOf('-'))
    val label = key.substring(key.indexOf('-') + 1)

    val stats = new DescriptiveStatistics()
    elements.forEach { element => stats.addValue(element.totalEndToEndDelay) }

    val p50 = stats.getPercentile(50)
    val p90 = stats.getPercentile(90)
    val p95 = stats.getPercentile(95)
    val p99 = stats.getPercentile(99)

    if !p50GaugeCache.containsKey(uuid -> label)
    then
      val metricGroup = baseMetricGroup
        .addGroup("uuid", uuid)
        .addGroup("label", label)

      val p50Gauge = new UpdatableGauge
      p50GaugeCache.put(uuid -> label, p50Gauge)
      metricGroup.gauge[Double, Gauge[Double]]("p50_Median_ms", p50Gauge)

      val p90Gauge = new UpdatableGauge
      p90GaugeCache.put(uuid -> label, p90Gauge)
      metricGroup.gauge[Double, Gauge[Double]]("p90_ms", p90Gauge)

      val p95Gauge = new UpdatableGauge
      p95GaugeCache.put(uuid -> label, p95Gauge)
      metricGroup.gauge[Double, Gauge[Double]]("p95_ms", p95Gauge)

      val p99Gauge = new UpdatableGauge
      p99GaugeCache.put(uuid -> label, p99Gauge)
      metricGroup.gauge[Double, Gauge[Double]]("p99_ms", p99Gauge)

    p50GaugeCache.get(uuid -> label).setValue(p50)
    p90GaugeCache.get(uuid -> label).setValue(p90)
    p95GaugeCache.get(uuid -> label).setValue(p95)
    p99GaugeCache.get(uuid -> label).setValue(p99)

    out.collect:
      CDFPercentiles(key,
                     context.window.getEnd,
                     totalPathCount = stats.getN,
                     p50_Median_ms = p50,
                     p90_ms = p90,
                     p95_ms = p95,
                     p99_ms = p99,
                     maxDelay_ms = stats.getMax)


object RollingQuantileAccumulator:

  class UpdatableGauge extends Gauge[Double]:
    @volatile private var value: Double = 0.0

    def setValue(newValue: Double): Unit =
      this.value = newValue

    override def getValue: Double = this.value
