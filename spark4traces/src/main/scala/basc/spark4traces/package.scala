package basc

import scala.jdk.CollectionConverters._

import org.apache.spark.sql.Row

import org.apache.avro.Schema
import org.apache.avro.generic.{ GenericData, GenericRecord }


package object spark4traces {

  case class Rate(immediate: Option[Long] = None,
                  timed: Option[BigDecimal] = None,
                  passive: Option[Long] = None)

  object Rate {

    def parse(rate: String): Rate =
      rate.charAt(0) match {
        case '∞' => Rate(immediate = Some(rate.substring(2, rate.length - 1).toLong))
        case '⊤' => Rate(passive = Some(rate.substring(2, rate.length - 1).toLong))
        case  _  => Rate(timed = Some(BigDecimal(rate.substring(3, rate.length - 1))))
      }

  }

  case class Plugin(causes: Option[Set[Long]] = None,
                    parents: Option[Set[Long]] = None,
                    probability: Option[BigDecimal] = None,
                    syncRate: Option[Option[BigDecimal]] = None,
                    whatIf: Option[((BigDecimal, BigDecimal), BigDecimal)] = None)

  case class Traces(uuid: String, number: Long,
                    clock: Double, started: Long, ended: Long,
                    agent: String, name: String, polarity: Option[Boolean],
                    key: String, guard: Boolean, label: String, keyBy: String,
                    rate: Rate, plugins: Map[String, Plugin], delay: Option[Double],
                    dir_cap: String, from: String, to: String,
                    snapshot: Option[String])

  object Traces {

    def apply(record: GenericRecord): Traces = {
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
      var plugins = Map.empty[String, Plugin]
      record.get("plugins").asInstanceOf[GenericData.Array[GenericRecord]].forEach { pluginRecord =>
        plugins += pluginRecord.getSchema.getName -> {
          pluginRecord.getSchema.getName match {
            case "causes"      =>
              var causes = List.empty[Long]
              pluginRecord.get("causes").asInstanceOf[GenericData.Array[Long]].forEach(causes ::= _)
              Plugin(causes = Some(causes.toSet))
            case "parents"     =>
              var numbers = List.empty[Long]
              pluginRecord.get("numbers").asInstanceOf[GenericData.Array[Long]].forEach(numbers ::= _)
              Plugin(parents = Some(numbers.toSet))
            case "probability" =>
              val probability = BigDecimal(pluginRecord.get("probability").toString)
              Plugin(probability = Some(probability))
            case "syncRate"    =>
              val syncRate = Option(pluginRecord.get("syncRate")).map(_.asInstanceOf[String]).map(BigDecimal(_))
              Plugin(syncRate = Some(syncRate))
            case "whatIf"      =>
              val fraction = pluginRecord.get("fraction").asInstanceOf[GenericRecord]
              val numerator = BigDecimal(fraction.get("numerator").toString)
              val denominator = BigDecimal(fraction.get("denominator").toString)
              val difference = BigDecimal(pluginRecord.get("difference").toString)
              Plugin(whatIf = Some(numerator -> denominator -> difference))
          }
        }
      }
      val delay = Option(record.get("delay")).map(_.asInstanceOf[Double])
      val dir_cap = record.get("dir_cap").toString
      val from = record.get("from").toString
      val to = record.get("to").toString
      val snapshot = Option(record.get("snapshot")).map(_.toString)

      Traces(uuid, number,
             clock, started, ended,
             agent, name, polarity,
             key, guard, label, keyBy,
             rate, plugins, delay,
             dir_cap, from, to, snapshot)
    }

  }


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

      { "name" : "delay", "type": ["null", "double"] },

      { "name" : "dir_cap", "type": "string" },
      { "name" : "from", "type": "string" },
      { "name" : "to", "type": "string" },
      { "name" : "snapshot", "type": ["null", "string"] }
    ]
  }"""

  val schema = new Schema.Parser().parse(_schema)

  def row2GenericRecord(row: Row, schema: Schema): GenericRecord = {
    val record = new GenericData.Record(schema)
    schema.getFields.forEach { field =>
      val i = field.pos
      val schema = unwrapNullable(field.schema)
      if (row.isNullAt(i)) {
        record.put(field.name, null)
      } else {
        schema.getType match {
          case Schema.Type.RECORD =>
            record.put(field.name, row2GenericRecord(row.getAs[Row](i), schema))

          case Schema.Type.STRING =>
            record.put(field.name, row.getAs[String](i))

          case Schema.Type.DOUBLE =>
            record.put(field.name, row.getAs[Double](i))

          case Schema.Type.INT =>
            record.put(field.name, row.getAs[Int](i))

          case Schema.Type.LONG =>
            record.put(field.name, row.getAs[Long](i))

          case Schema.Type.BOOLEAN =>
            record.put(field.name, row.getAs[Boolean](i))

          case Schema.Type.ARRAY =>
            val elemSchema = schema.getElementType

            elemSchema.getType match {
              case Schema.Type.UNION =>
                val schemas = unwrapUnion(elemSchema)
                val rows = row.getSeq[Row](i)
                val avroList = (0 until rows.length).map { j =>
                  val row = rows(j)
                  val iʹ = (0 until row.length).find(!row.isNullAt(_)).get
                  row2GenericRecord(row.getAs[Row](iʹ), schemas(iʹ))
                }.asJava
                record.put(field.name, new GenericData.Array(schema, avroList))

              case Schema.Type.RECORD =>
                record.put(field.name, new GenericData.Array(schema, row.getSeq[Row](i).map(row2GenericRecord(_, elemSchema)).asJava))

              case _ =>
                record.put(field.name, new GenericData.Array(schema, row.getSeq[Any](i).asJava))
            }
        }
      }
    }
    record
  }

  private def unwrapNullable(schema: Schema): Schema =
    if (schema.getType == Schema.Type.UNION)
      schema.getTypes.asScala.find(_.getType != Schema.Type.NULL).getOrElse(schema)
    else
      schema

  private def unwrapUnion(schema: Schema): List[Schema] =
    schema.getTypes.asScala.toList

}
