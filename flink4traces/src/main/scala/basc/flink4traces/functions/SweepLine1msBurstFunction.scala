package basc
package flink4traces
package functions

import java.util.{ HashMap => Map }

import java.time.Duration

import org.apache.flink.streaming.api.windowing.windows.TimeWindow
import org.apache.flink.streaming.api.functions.windowing.ProcessAllWindowFunction
import org.apache.flink.util.Collector

import sweepline.{ SweepLine, SweepLine1msBurst }


object SweepLine1msBurstFunction extends ProcessAllWindowFunction[SweepLine, SweepLine1msBurst, TimeWindow]:
  override def process(context: ProcessAllWindowFunction[SweepLine, SweepLine1msBurst, TimeWindow]#Context,
                       elements: java.lang.Iterable[SweepLine],
                       out: Collector[SweepLine1msBurst]): Unit =
    // Everything inside 'elements' shares the exact same timestamp
    val windowTimestamp = context.window.getStart

    val perPIDSweepLine1msBurst = Map[Long, SweepLine1msBurst]()
    val perPIDHistograms = Map[Long, Map[String, SweepLine]]()

    elements.forEach {
      _.perPIDSweepLine.forEach { (pid, element) =>
        if !perPIDHistograms.containsKey(pid)
        then
          perPIDHistograms.put(pid, Map())
        perPIDHistograms.get(pid).put(element.label, element)
      }
    }

    perPIDHistograms.forEach { (pid, histograms) =>
      val elements = histograms.values
      var clock = .0

      elements.forEach { element =>
        clock = math.max(clock, element.clock)
      }

      perPIDSweepLine1msBurst.put(pid, SweepLine1msBurst(windowTimestamp, clock, histograms, null))
    }

    val histograms = Map[String, SweepLine]()

    elements.forEach { element =>
      histograms.put(element.label, element)
    }

    out.collect(SweepLine1msBurst(windowTimestamp, .0, histograms, perPIDSweepLine1msBurst))
