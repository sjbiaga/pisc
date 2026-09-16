package basc
package flink4traces
package functions

import java.time.Duration

import java.util.{ Collections, HashMap => Map, LinkedList => List }

import org.apache.flink.api.common.state.{ ListState, ListStateDescriptor, ValueState, ValueStateDescriptor }
import org.apache.flink.streaming.api.functions.KeyedProcessFunction
import org.apache.flink.util.Collector

import sweepline.SweepLine
import StatefulSweepLineFunction.*


class StatefulSweepLineFunction(windowDurationMs: Long)
  extends KeyedProcessFunction[String, Traces, SweepLine]:

  private var tracesListState: ListState[Traces] = null
  private var sliceStartState: ValueState[java.lang.Long] = null

  override def open(openContext: org.apache.flink.api.common.functions.OpenContext): Unit =
    tracesListState = getRuntimeContext.getListState(
      new ListStateDescriptor[Traces]("traces", classOf[Traces])
    )
    sliceStartState = getRuntimeContext.getState(
      new ValueStateDescriptor[java.lang.Long]("slice-start", classOf[java.lang.Long])
    )

  override def processElement(value: Traces,
                              ctx: KeyedProcessFunction[String, Traces, SweepLine]#Context,
                              out: Collector[SweepLine]): Unit =
    if sliceStartState.value eq null
    then
      // Align slice start cleanly to the configured window duration step (e.g., top of the hour)
      val alignedStart = value.started - (value.started % windowDurationMs)
      sliceStartState.update(alignedStart)

      // Register a cleanup/reporting timer for the end of this boundary window
      ctx.timerService.registerEventTimeTimer(alignedStart + windowDurationMs)

    // Save the traces into managed operator state
    tracesListState.add(value)

  override def onTimer(timestamp: Long,
                       ctx: KeyedProcessFunction[String, Traces, SweepLine]#OnTimerContext,
                       out: Collector[SweepLine]): Unit =
    val currentSliceStart = sliceStartState.value
    val currentSliceEnd = timestamp // The timer triggers exactly at the window edge
    val totalSliceDuration = (currentSliceEnd - currentSliceStart).toDouble

    val events = java.util.ArrayList[TimeEvent]()
    val perPIDEvents = Map[Long, java.util.ArrayList[TimeEvent]]()
    val crossOverTraces = List[Traces]()

    // Filter and clip traces to the current slice boundaries
    tracesListState.get.forEach { traces =>
      // Check if the traces overlaps with the current evaluation slice
      if currentSliceStart < traces.ended && traces.started < currentSliceEnd
      then
        def add(events: java.util.ArrayList[TimeEvent], clock: Double): Unit =
          events.add(TimeEvent(math.max(traces.started, currentSliceStart), 1, clock))
          events.add(TimeEvent(math.min(traces.ended, currentSliceEnd), -1, clock))

        add(events, .0)

        if !perPIDEvents.containsKey(traces.pid)
        then
          perPIDEvents.put(traces.pid, java.util.ArrayList[TimeEvent]())
        add(perPIDEvents.get(traces.pid), traces.clock)

        // If the traces extends past the current window boundary, save it for the next slice
        if traces.ended > currentSliceEnd
        then
          crossOverTraces.add(traces)
    }

    def histogram(events: java.util.ArrayList[TimeEvent]): (Double, Map[Int, Double]) =
      Collections.sort(events)

      val concurrencyDurations = Map[Int, Long]()
      var last = currentSliceStart
      var count = 0
      var clock = .0

      events.forEach { event =>
        val duration = event.time - last
        if duration > 0
        then
          concurrencyDurations.put(count, concurrencyDurations.getOrDefault(count, 0) + duration)
        count += event.delta
        last = event.time
        clock = event.clock
      }

      {
        val duration = currentSliceEnd - last
        if duration > 0
        then
          concurrencyDurations.put(count, concurrencyDurations.getOrDefault(count, 0) + duration)
      }

      // Map the raw durations to absolute percentages
      val percentageHistogram = Map[Int, Double]()
      concurrencyDurations.forEach { (concurrency, duration) =>
        percentageHistogram.put(concurrency, (duration / totalSliceDuration) * 100)
      }

      clock -> percentageHistogram

    val perPIDSweepLine = Map[Long, SweepLine]()

    perPIDEvents.forEach { (pid, eventsʹ) =>
      val (clock, hist) = histogram(eventsʹ)
      perPIDSweepLine.put(pid, SweepLine(currentSliceEnd, ctx.getCurrentKey, clock, hist, Map()))
    }

    if tracesListState.get.iterator.hasNext
    then
      val (clock, hist) = histogram(events)
      out.collect(SweepLine(currentSliceEnd, ctx.getCurrentKey, clock, hist, perPIDSweepLine))

    // --- State Rollover Maintenance ---
    tracesListState.clear()
    if !crossOverTraces.isEmpty
    then
      // Re-populate state only with elements that successfully crossed the boundary line
      tracesListState.addAll(crossOverTraces)

    // Shift state tracking forward to the next time block
    sliceStartState.update(currentSliceEnd)
    ctx.timerService.registerEventTimeTimer(currentSliceEnd + windowDurationMs)


object StatefulSweepLineFunction:

  case class TimeEvent(time: Long, delta: Int, clock: Double) extends Comparable[TimeEvent]:
    override def compareTo(that: TimeEvent): Int =
      if this.time == that.time
      then Integer.compare(this.delta, that.delta)
      else java.lang.Long.compare(this.time, that.time)
