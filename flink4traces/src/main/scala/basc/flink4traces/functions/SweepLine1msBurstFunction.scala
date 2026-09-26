package basc
package flink4traces
package functions

import java.util.{ HashMap => Map }

import org.apache.flink.streaming.api.windowing.windows.TimeWindow
import org.apache.flink.streaming.api.functions.windowing.ProcessAllWindowFunction
import org.apache.flink.util.Collector

import sweepline.{ SweepLine, SweepLine1msBurst }


class SweepLine1msBurstFunction extends ProcessAllWindowFunction[SweepLine, SweepLine1msBurst, TimeWindow]:
  override def process(context: ProcessAllWindowFunction[SweepLine, SweepLine1msBurst, TimeWindow]#Context,
                       elements: java.lang.Iterable[SweepLine],
                       out: Collector[SweepLine1msBurst]): Unit =
    // Everything inside 'elements' shares the exact same timestamp
    val windowTimestamp = context.window.getStart

    val perUUIDSweepLine1msBurst = Map[String, SweepLine1msBurst]()
    val perUUIDHistograms = Map[String, Map[String, SweepLine]]()

    elements.forEach {
      _.perUUIDSweepLine.forEach { (uuid, element) =>
        if !perUUIDHistograms.containsKey(uuid)
        then
          perUUIDHistograms.put(uuid, Map())
        perUUIDHistograms.get(uuid).put(element.label, element)
      }
    }

    perUUIDHistograms.forEach { (uuid, histograms) =>
      val elements = histograms.values
      var clock = .0

      elements.forEach { element =>
        clock = math.max(clock, element.clock)
      }

      perUUIDSweepLine1msBurst.put(uuid, SweepLine1msBurst(windowTimestamp, clock, histograms, null))
    }

    val histograms = Map[String, SweepLine]()

    elements.forEach { element =>
      histograms.put(element.label, element)
    }

    out.collect(SweepLine1msBurst(windowTimestamp, .0, histograms, perUUIDSweepLine1msBurst))
