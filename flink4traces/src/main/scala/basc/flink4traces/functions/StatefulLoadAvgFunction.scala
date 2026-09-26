package basc
package flink4traces
package functions

import java.time.Duration

import java.util.{ Collections, HashMap => Map, LinkedList => List }

import org.apache.flink.api.common.state.{ ListState, ListStateDescriptor, ValueState, ValueStateDescriptor }
import org.apache.flink.streaming.api.functions.KeyedProcessFunction
import org.apache.flink.util.Collector

import loadavg.LoadAvg
import StatefulLoadAvgFunction.*


class StatefulLoadAvgFunction extends KeyedProcessFunction[String, Traces, LoadAvg]:

  private def decay(m: Int) =
    // Pre-calculate the exact Linux exponential decay constant (e^(-dt / tau))
    val tau: Double = m * 60.0
    math.exp(-5 / tau)

  private val decayFactor1m = decay(1)
  private val decayFactor10m = decay(10)
  private val decayFactor15m = decay(15)

  private var tracesListState: ListState[Traces] = null
  private var sliceStartState: ValueState[java.lang.Long] = null
  private var loadAvgState: ValueState[LoadAvgState] = null

  override def open(openContext: org.apache.flink.api.common.functions.OpenContext): Unit =
    tracesListState = getRuntimeContext.getListState(
      new ListStateDescriptor[Traces]("traces", classOf[Traces])
    )
    sliceStartState = getRuntimeContext.getState(
      new ValueStateDescriptor[java.lang.Long]("slice-start", classOf[java.lang.Long])
    )
    loadAvgState = getRuntimeContext.getState(
      new ValueStateDescriptor[LoadAvgState]("load-avg", classOf[LoadAvgState], LoadAvgState(.0, .0, .0, Map()))
    )

  override def processElement(value: Traces,
                              ctx: KeyedProcessFunction[String, Traces, LoadAvg]#Context,
                              out: Collector[LoadAvg]): Unit =
    if sliceStartState.value eq null
    then
      // Align slice start cleanly to the configured window duration step (e.g., top of the hour)
      val alignedStart = value.started - (value.started % 5000)
      sliceStartState.update(alignedStart)

      // Register a cleanup/reporting timer for the end of this boundary window
      ctx.timerService.registerEventTimeTimer(alignedStart + 5000)

    // Save the traces into managed operator state
    tracesListState.add(value)

  override def onTimer(timestamp: Long,
                       ctx: KeyedProcessFunction[String, Traces, LoadAvg]#OnTimerContext,
                       out: Collector[LoadAvg]): Unit =
    val currentSliceStart = sliceStartState.value
    val currentSliceEnd = timestamp // The timer triggers exactly at the window edge
    val totalSliceDuration = (currentSliceEnd - currentSliceStart).toDouble

    val perUUIDData = Map[String, (Int, Double)]()
    val crossOverTraces = List[Traces]()

    var count = 0

    // Filter and clip traces to the current slice boundaries
    tracesListState.get.forEach { traces =>
      // Check if the traces overlaps with the current evaluation slice
      if currentSliceStart < traces.ended && traces.started < currentSliceEnd
      then
        count += 1

        {
          val (count, _) = perUUIDData.getOrDefault(traces.uuid, (0, .0))
          perUUIDData.put(traces.uuid, (count + 1, traces.clock))
        }

        // If the traces extends past the current window boundary, save it for the next slice
        if traces.ended > currentSliceEnd
        then
          crossOverTraces.add(traces)
    }

    val perUUIDLoadAvg = Map[String, LoadAvg]()

    perUUIDData.forEach { case (uuid, (countʹ, clock)) =>
      val perUUIDLoadAvgFromState = loadAvgState.value.perUUIDState

      if !perUUIDLoadAvgFromState.containsKey(uuid)
      then
        perUUIDLoadAvgFromState.put(uuid, LoadAvgState(.0, .0, .0, Map()))

      val loadAvgFromState = perUUIDLoadAvgFromState.get(uuid)

      val load1m = StatefulLoadAvgFunction(loadAvgFromState.load1m, countʹ, decayFactor1m)
      val load10m = StatefulLoadAvgFunction(loadAvgFromState.load10m, countʹ, decayFactor10m)
      val load15m = StatefulLoadAvgFunction(loadAvgFromState.load15m, countʹ, decayFactor15m)

      perUUIDLoadAvg.put(uuid, LoadAvg(currentSliceEnd, ctx.getCurrentKey, clock, load1m, load10m, load15m, null))

      perUUIDLoadAvgFromState.put(uuid, loadAvgFromState.copy(load1m = load1m, load10m = load10m, load15m = load15m))
    }

    if tracesListState.get.iterator.hasNext
    then
      val load1m = StatefulLoadAvgFunction(loadAvgState.value.load1m, count, decayFactor1m)
      val load10m = StatefulLoadAvgFunction(loadAvgState.value.load10m, count, decayFactor10m)
      val load15m = StatefulLoadAvgFunction(loadAvgState.value.load15m, count, decayFactor15m)

      out.collect(LoadAvg(currentSliceEnd, ctx.getCurrentKey, .0, load1m, load10m, load15m, perUUIDLoadAvg))

      loadAvgState.update(loadAvgState.value.copy(load1m = load1m, load10m = load10m, load15m = load15m))

    // --- State Rollover Maintenance ---
    tracesListState.clear()
    if !crossOverTraces.isEmpty
    then
      // Re-populate state only with elements that successfully crossed the boundary line
      tracesListState.addAll(crossOverTraces)

    // Shift state tracking forward to the next time block
    sliceStartState.update(currentSliceEnd)
    ctx.timerService.registerEventTimeTimer(currentSliceEnd + 5000)


object StatefulLoadAvgFunction:

  def apply(load: Double, count: Int, decayFactor: Double): Double =
    // Apply the EWMA formula: Load_t = (Load_t-1 * decay) + (Active_t * (1 - decay))
    (load * decayFactor) + (count * (1.0 - decayFactor))

  case class LoadAvgState(load1m: Double,
                          load10m: Double,
                          load15m: Double,
                          perUUIDState: Map[String, LoadAvgState])
