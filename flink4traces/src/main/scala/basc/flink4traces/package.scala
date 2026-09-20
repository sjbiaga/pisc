package basc

import org.apache.avro.Schema
import org.apache.avro.generic.{ GenericData, GenericRecord }

import org.apache.flink.api.common.functions.FlatMapFunction
import org.apache.flink.util.Collector


package object flink4traces:

  object Rate:
    def parse(rate: String): Rate =
      rate.charAt(0) match
        case '∞' => ∞(rate.substring(2, rate.length - 1).toLong)
        case '⊤' => ⊤(rate.substring(2, rate.length - 1).toLong)
        case _   => `ℝ⁺`(java.math.BigDecimal(rate.substring(3, rate.length - 1)))
  sealed trait Rate extends Any
  case class ∞(weight: Long) extends AnyVal with Rate
  case class `ℝ⁺`(rate: java.math.BigDecimal) extends AnyVal with Rate
  case class ⊤(weight: Long) extends AnyVal with Rate

  case class Traces(pid: Long,
                    number: Long, causes: List[Long],
                    clock: Double, started: Long, ended: Long,
                    agent: String, name: String, polarity: Option[Boolean],
                    key: String, guard: Boolean, label: String, keyBy: String,
                    rate: Rate, probability: java.math.BigDecimal,
                    delay: Double, syncRate: Double,
                    dir_cap: String, from: String, to: String,
                    snapshot: Option[String])

  object Traces:

    object GenericRecord2Traces extends FlatMapFunction[GenericRecord, Traces]:
      override def flatMap(record: GenericRecord, out: Collector[Traces]): Unit =
        try
          val pid = record.get("pid").asInstanceOf[Long]
          val number = record.get("number").asInstanceOf[Long]
          var causes = List.empty[Long]
          record.get("causes").asInstanceOf[GenericData.Array[Long]].forEach(causes ::= _)
          val clock = record.get("clock").asInstanceOf[Double]
          val started = record.get("started").asInstanceOf[Long]
          val ended = record.get("ended").asInstanceOf[Long]
          val agent = record.get("agent").toString
          val name = record.get("name").toString
          val polarity = Option(record.get("polarity")).map(_.asInstanceOf[Boolean])
          val key = record.get("key").toString
          val guard = record.get("guard").asInstanceOf[Boolean]
          val label = record.get("label").toString
          val keyBy = record.get("keyBy").toString
          val rate = Rate.parse(record.get("rate").toString)
          val probability = java.math.BigDecimal(record.get("probability").toString)
          val delay = Option(record.get("delay")).map(_.asInstanceOf[Double]).getOrElse(Double.PositiveInfinity)
          val syncRate = Option(record.get("syncRate")).map(_.asInstanceOf[Double]).getOrElse(Double.PositiveInfinity)
          val dir_cap = record.get("dir_cap").toString
          val from = record.get("from").toString
          val to = record.get("to").toString
          val snapshot = Option(record.get("snapshot")).map(_.toString)
          out.collect:
            Traces(pid,
                   number, causes,
                   clock, started, ended,
                   agent, name, polarity,
                   key, guard, label, keyBy,
                   rate, probability,
                   delay, syncRate,
                   dir_cap, from, to, snapshot)
        catch _.printStackTrace()


  private val _schema = """{
    "namespace": "pisc.avro",
    "type": "record",
    "name": "BioAmbients2Scala",
    "fields": [
      { "name" : "pid", "type": "long" },

      { "name" : "number", "type": "long" },
      { "name" : "causes", "type": { "type": "array", "items": "long", "default": [] } },

      { "name" : "clock", "type": "double" },
      { "name" : "started", "type": "long" },
      { "name" : "ended", "type": "long" },

      { "name" : "agent", "type": "string" },
      { "name" : "name", "type": "string" },
      { "name" : "polarity", "type": ["null", "boolean"] },

      { "name" : "key", "type": "string" },
      { "name" : "guard", "type": "boolean" },
      { "name" : "label", "type": "string" },
      { "name" : "keyBy", "type": "string" },

      { "name" : "rate", "type": "string" },
      { "name" : "probability", "type": "string" },

      { "name" : "delay", "type": ["null", "double"] },
      { "name" : "syncRate", "type": ["null", "double"] },

      { "name" : "dir_cap", "type": "string" },
      { "name" : "from", "type": "string" },
      { "name" : "to", "type": "string" },
      { "name" : "snapshot", "type": ["null", "string"] }
    ]
  }"""

  val schema = Schema.Parser().parse(_schema)
