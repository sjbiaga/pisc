package basc

import java.util.{ HashMap => Map }

import org.apache.avro.Schema
import org.apache.avro.generic.GenericRecord

import org.apache.flink.api.common.functions.FlatMapFunction
import org.apache.flink.util.Collector


package object flink4traces:

  case class Traces(pid: Long,
                    number: Long, clock: Double, started: Long, ended: Long,
                    agent: String, name: String, polarity: Option[Boolean],
                    key: String, guard: Boolean, label: String,
                    rate: String, delay: Double, duration: Option[Double],
                    dir_cap: String, from: String, to: String,
                    snapshot: Option[String])

  object Traces:

    object GenericRecord2Traces extends FlatMapFunction[GenericRecord, Traces]:
      override def flatMap(record: GenericRecord, out: Collector[Traces]): Unit =
        try
          val pid = record.get("pid").asInstanceOf[Long]
          val number = record.get("number").asInstanceOf[Long]
          val clock = record.get("clock").asInstanceOf[Double]
          val started = record.get("started").asInstanceOf[Long]
          val ended = record.get("ended").asInstanceOf[Long]
          val agent = record.get("agent").toString
          val name = record.get("name").toString
          val polarity = Option(record.get("polarity")).map(_.asInstanceOf[Boolean])
          val key = record.get("key").toString
          val guard = record.get("guard").asInstanceOf[Boolean]
          val label = record.get("label").toString
          val rate = record.get("rate").toString
          val delay = record.get("delay").asInstanceOf[Double]
          val duration = Option(record.get("duration")).map(_.asInstanceOf[Double])
          val dir_cap = record.get("dir_cap").toString
          val from = record.get("from").toString
          val to = record.get("to").toString
          val snapshot = Option(record.get("snapshot")).map(_.toString)
          out.collect {
            Traces(pid,
                   number, clock, started, ended,
                   agent, name, polarity,
                   key, guard, label,
                   rate, delay, duration,
                   dir_cap, from, to,
                   snapshot)
          }
        catch _.printStackTrace()


  case class LoadAvg(timestamp: Long,
                     perPIDLoadAvg: Map[Long, LoadAvg],
                     clock: Double,
                     oneMinuteLoad: Double,
                     tenMinutesLoad: Double,
                     fifteenMinutesLoad: Double)

  private val _schema = """{
    "namespace": "pisc.avro",
    "type": "record",
    "name": "BioAmbients2Scala",
    "fields": [
      { "name" : "pid", "type": "long" },

      { "name" : "number", "type": "long" },
      { "name" : "clock", "type": "double" },
      { "name" : "started", "type": "long" },
      { "name" : "ended", "type": "long" },

      { "name" : "agent", "type": "string" },
      { "name" : "name", "type": "string" },
      { "name" : "polarity", "type": ["null", "boolean"] },

      { "name" : "key", "type": "string" },
      { "name" : "guard", "type": "boolean" },
      { "name" : "label", "type": "string" },

      { "name" : "rate", "type": "string" },
      { "name" : "delay", "type": "double" },
      { "name" : "duration", "type": ["null", "double"] },

      { "name" : "dir_cap", "type": "string" },
      { "name" : "from", "type": "string" },
      { "name" : "to", "type": "string" },
      { "name" : "snapshot", "type": ["null", "string"] }
    ]
  }"""

  val schema = Schema.Parser().parse(_schema)
