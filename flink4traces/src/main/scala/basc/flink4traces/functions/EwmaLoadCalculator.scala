package basc
package flink4traces
package functions

import java.util.{ HashMap => Map, HashSet => Set }

import org.apache.flink.api.common.functions.AggregateFunction
import org.apache.flink.streaming.api.windowing.windows.TimeWindow

import EwmaLoadCalculator.*


type EwmaLoadResultʹ = (Double, (Double, Double, Double))
type EwmaLoadResult = (Map[Long, EwmaLoadResultʹ], EwmaLoadResultʹ)


class EwmaLoadCalculator(stepSeconds: Int)
    extends AggregateFunction[(Long, String, Long, Double), EwmaAccumulator, EwmaLoadResult]:

  private def decay(m: Int) =
    // Pre-calculate the exact Linux exponential decay constant (e^(-dt / tau))
    val tau: Double = m * 60.0
    math.exp(-stepSeconds / tau)

  private val decayFactor1m = decay(1)
  private val decayFactor10m = decay(10)
  private val decayFactor15m = decay(15)

  override def createAccumulator(): EwmaAccumulator =
    EwmaAccumulator(.0, .0, .0, 0, Map(), .0)

  override def add(it: (Long, String, Long, Double), acc: EwmaAccumulator): EwmaAccumulator =
    val (_, _, pid, clock) = it

    if !acc.perPidAccumulator.containsKey(pid)
    then
      acc.perPidAccumulator.put(pid, EwmaAccumulator(.0, .0, .0, 0, null, .0))

    acc.perPidAccumulator.get(pid).clock = clock
    acc.perPidAccumulator.get(pid).count += 1

    acc.clock = clock
    acc.count += 1

    acc

  override def getResult(acc: EwmaAccumulator): EwmaLoadResult =
    // We update the acc state directly so the next window builds on it
    val load1m = EwmaLoadCalculator(acc.load1m, acc.count, decayFactor1m)
    val load10m = EwmaLoadCalculator(acc.load10m, acc.count, decayFactor10m)
    val load15m = EwmaLoadCalculator(acc.load15m, acc.count, decayFactor15m)

    acc.load1m = load1m
    acc.load10m = load10m
    acc.load15m = load15m

    val perPidAccumulator = Map[Long, EwmaLoadResultʹ]

    acc.perPidAccumulator.forEach { (pid, acc) =>

      val load1m = EwmaLoadCalculator(acc.load1m, acc.count, decayFactor1m)
      val load10m = EwmaLoadCalculator(acc.load10m, acc.count, decayFactor10m)
      val load15m = EwmaLoadCalculator(acc.load15m, acc.count, decayFactor15m)

      acc.load1m = load1m
      acc.load10m = load10m
      acc.load15m = load15m

      perPidAccumulator.put(pid, (acc.clock -> (load1m, load10m, load15m)))
    }

    perPidAccumulator -> (acc.clock -> (load1m, load10m, load15m))

  override def merge(a: EwmaAccumulator, b: EwmaAccumulator): EwmaAccumulator =
    var perPidAccumulatorMerged: Map[Long, EwmaAccumulator] = null

    if a.perPidAccumulator eq null
    then
      perPidAccumulatorMerged = b.perPidAccumulator
    else if b.perPidAccumulator eq null
    then
      perPidAccumulatorMerged = a.perPidAccumulator
    else
      perPidAccumulatorMerged = Map[Long, EwmaAccumulator]()
      val c = Set(a.perPidAccumulator.keySet)
      c.retainAll(b.perPidAccumulator.keySet)
      c.forEach { pid =>
        perPidAccumulatorMerged.put(pid, merge(a.perPidAccumulator.get(pid), b.perPidAccumulator.get(pid)))
      }
      a.perPidAccumulator.keySet.removeAll(c)
      a.perPidAccumulator.keySet.forEach { pid =>
        perPidAccumulatorMerged.put(pid, a.perPidAccumulator.get(pid))
      }
      b.perPidAccumulator.keySet.removeAll(c)
      b.perPidAccumulator.keySet.forEach { pid =>
        perPidAccumulatorMerged.put(pid, b.perPidAccumulator.get(pid))
      }

    EwmaAccumulator(
      load1m = math.max(a.load1m, b.load1m),
      load10m = math.max(a.load10m, b.load10m),
      load15m = math.max(a.load15m, b.load15m),
      count = a.count + b.count,
      perPidAccumulator = perPidAccumulatorMerged,
      clock = math.max(a.clock, b.clock)
    )


object EwmaLoadCalculator:

  def apply(load: Double, count: Int, decayFactor: Double): Double =
    // Apply the EWMA formula: Load_t = (Load_t-1 * decay) + (Active_t * (1 - decay))
    (load * decayFactor) + (count * (1.0 - decayFactor))

  case class EwmaAccumulator(var load1m: Double,
                             var load10m: Double,
                             var load15m: Double,
                             var count: Int,
                             var perPidAccumulator: Map[Long, EwmaAccumulator],
                             var clock: Double)
