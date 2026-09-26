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

    val perUUIDLoadAvg1msBurst = Map[String, LoadAvg1msBurst]()
    val perUUIDAverages = Map[String, Map[String, LoadAvg]]()

    elements.forEach {
      _.perUUIDLoadAvg.forEach { (uuid, element) =>
        if !perUUIDAverages.containsKey(uuid)
        then
          perUUIDAverages.put(uuid, Map())
        perUUIDAverages.get(uuid).put(element.label, element)
      }
    }

    perUUIDAverages.forEach { (uuid, averages) =>
      val elements = averages.values
      var clock = .0

      elements.forEach { element =>
        clock = math.max(clock, element.clock)
      }

      perUUIDLoadAvg1msBurst.put(uuid, LoadAvg1msBurst(windowTimestamp, clock, averages, null))
    }

    val averages = Map[String, LoadAvg]()

    elements.forEach { element =>
      averages.put(element.label, element)
    }

    out.collect(LoadAvg1msBurst(windowTimestamp, .0, averages, perUUIDLoadAvg1msBurst))
