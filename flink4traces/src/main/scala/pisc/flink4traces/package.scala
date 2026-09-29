package pisc

import java.math.BigDecimal

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
        case _   => `ℝ⁺`(BigDecimal(rate.substring(3, rate.length - 1)))
  sealed trait Rate extends Any
  case class ∞(weight: Long) extends AnyVal with Rate
  case class `ℝ⁺`(rate: BigDecimal) extends AnyVal with Rate
  case class ⊤(weight: Long) extends AnyVal with Rate

  enum Plugin:
    case causes(causes: Set[Long])
    case parents(numbers: Set[Long])
    case probability(probability: BigDecimal)
    case syncRate(rate: Option[BigDecimal])
    case whatIf(fraction: (BigDecimal, BigDecimal), difference: BigDecimal)

  case class Traces(uuid: String, number: Long,
                    clock: Double, started: Long, ended: Long,
                    agent: String, name: String, polarity: Option[Boolean],
                    key: String, guard: Boolean, label: String, keyBy: String,
                    rate: Rate, plugins: Seq[Plugin], delay: Option[Double])

  object Traces:

    object GenericRecord2Traces extends FlatMapFunction[GenericRecord, Traces]:
      override def flatMap(record: GenericRecord, out: Collector[Traces]): Unit =
        try
          val uuid = record.get("uuid").toString
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
          val keyBy = record.get("keyBy").toString
          val rate = Rate.parse(record.get("rate").toString)
          var plugins = List.empty[Plugin]
          record.get("plugins").asInstanceOf[GenericData.Array[GenericRecord]].forEach { pluginRecord =>
            plugins ::= {
              pluginRecord.getSchema.getName match
                case "causes"      =>
                  var causes = List.empty[Long]
                  pluginRecord.get("causes").asInstanceOf[GenericData.Array[Long]].forEach(causes ::= _)
                  Plugin.causes(causes.toSet)
                case "parents"     =>
                  var numbers = List.empty[Long]
                  pluginRecord.get("numbers").asInstanceOf[GenericData.Array[Long]].forEach(numbers ::= _)
                  Plugin.parents(numbers.toSet)
                case "probability" =>
                  val probability = BigDecimal(pluginRecord.get("probability").toString)
                  Plugin.probability(probability)
                case "syncRate"    =>
                  val syncRate = Option(pluginRecord.get("syncRate")).map(_.asInstanceOf[String]).map(BigDecimal(_))
                  Plugin.syncRate(syncRate)
                case "whatIf"      =>
                  val fraction = pluginRecord.get("fraction").asInstanceOf[GenericRecord]
                  val numerator = BigDecimal(fraction.get("numerator").toString)
                  val denominator = BigDecimal(fraction.get("denominator").toString)
                  val difference = BigDecimal(pluginRecord.get("difference").toString)
                  Plugin.whatIf(numerator -> denominator, difference)
            }
          }
          val delay = Option(record.get("delay")).map(_.asInstanceOf[Double])
          out.collect:
            Traces(uuid, number,
                   clock, started, ended,
                   agent, name, polarity,
                   key, guard, label, keyBy,
                   rate, plugins, delay)
        catch _.printStackTrace()


  private val _schema = """{
    "namespace": "pisc.avro",
    "type": "record",
    "name": "BioAmbients2Scala",
    "fields": [
      { "name" : "uuid", "type": "string" },
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
      { "name" : "keyBy", "type": "string" },

      { "name" : "rate", "type": "string" },
      { "name" : "plugins",
        "type": {
          "type": "array",
          "items": [
            { "name": "causes",
              "type": "record",
              "fields": [
                { "name": "causes", "type": { "type": "array", "items": "long" } }
              ]
            },
            { "name": "parents",
              "type": "record",
              "fields": [
                { "name": "numbers", "type": { "type": "array", "items": "long" } }
              ]
            },
            { "name": "probability",
              "type": "record",
              "fields": [
                { "name": "probability", "type": "string" }
              ]
            },
            { "name": "syncRate",
              "type": "record",
              "fields": [
                { "name": "rate", "type": ["null", "string"] }
              ]
            },
            { "name": "whatIf",
              "type": "record",
              "fields": [
                { "name": "fraction",
                  "type": {
                    "name": "fraction",
                    "type": "record",
                    "fields": [
                      { "name": "numerator", "type": "string" },
                      { "name": "denominator", "type": "string" }
                    ]
                  }
                },
                { "name": "difference", "type": "string" }
              ]
            }
          ],
          "default": []
        }
      },

      { "name" : "delay", "type": ["null", "double"] }
    ]
  }"""

  val schema = Schema.Parser().parse(_schema)
