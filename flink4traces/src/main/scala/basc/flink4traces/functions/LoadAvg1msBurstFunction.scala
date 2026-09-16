package basc
package flink4traces
package functions

import java.util.{ HashMap => Map }

import org.apache.flink.streaming.api.windowing.windows.TimeWindow
import org.apache.flink.streaming.api.functions.windowing.ProcessAllWindowFunction
import org.apache.flink.util.Collector

import loadavg.{ LoadAvg, LoadAvg1msBurst }


class LoadAvg1msBurstFunction extends ProcessAllWindowFunction[LoadAvg, LoadAvg1msBurst, TimeWindow]:
  override def process(context: ProcessAllWindowFunction[LoadAvg, LoadAvg1msBurst, TimeWindow]#Context,
                       elements: java.lang.Iterable[LoadAvg],
                       out: Collector[LoadAvg1msBurst]): Unit =
    // Everything inside 'elements' shares the exact same timestamp
    val windowTimestamp = context.window.getStart

    val perPIDLoadAvg1msBurst = Map[Long, LoadAvg1msBurst]()
    val perPIDAverages = Map[Long, Map[String, LoadAvg]]()

    elements.forEach {
      _.perPIDLoadAvg.forEach { (pid, element) =>
        if !perPIDAverages.containsKey(pid)
        then
          perPIDAverages.put(pid, Map())
        perPIDAverages.get(pid).put(element.label, element)
      }
    }

    perPIDAverages.forEach { (pid, averages) =>
      val elements = averages.values
      var clock = .0

      elements.forEach { element =>
        clock = math.max(clock, element.clock)
      }

      perPIDLoadAvg1msBurst.put(pid, LoadAvg1msBurst(windowTimestamp, clock, averages, null))
    }

    val averages = Map[String, LoadAvg]()

    elements.forEach { element =>
      averages.put(element.label, element)
    }

    out.collect(LoadAvg1msBurst(windowTimestamp, .0, averages, perPIDLoadAvg1msBurst))
