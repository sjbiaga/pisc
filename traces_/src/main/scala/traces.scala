/*
 * Copyright (c) 2023-2026 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * [Except as contained in this notice, the name of Sebastian I. Gliţa-Catina
 * shall not be used in advertising or otherwise to promote the sale, use
 * or other dealings in this Software without prior written authorization
 * from Sebastian I. Gliţa-Catina.]
 */

import _root_.scala.jdk.CollectionConverters.*

import _root_.scala.collection.immutable.{ List, Map, Set }

import _root_.io.circe.{ Codec, Encoder }
import _root_.io.circe.syntax.*


package object `Π-traces`:

  enum KeyBy:
    case HID, ANY, AGENT_LABEL

  enum Plugin derives Codec.AsObject:
    case syncRate(rate: Option[BigDecimal])
    case probability(probability: BigDecimal)
    case whatIf(factor: BigDecimal, term: Option[BigDecimal])

  object Plugin:
    given Encoder[BigDecimal] = Encoder.encodeString.contramap(_.toString)


  var `π-traces`: `Π-Traces` = null


  enum `Π-Backend`:
    case same, redpanda, elasticmq


  sealed trait `Π-Traces`:
    lazy val uuid = java.util.UUID.randomUUID.toString.replaceAll("-", "")
    protected var rootLabels = Map[Long, String]()
    val backend: `Π-Backend` = `Π-Backend`.same
    def apply(number: Long, causes: Set[Long],
              clock: Double, started: Long, ended: Long,
              agent: String, name: String, polarity: Option[Boolean],
              key: String, guard: Boolean, label: String, keyBy: KeyBy,
              rate: String, plugins: Seq[Plugin], delay: Double,
              dir_cap: String, from: String, to: String, snapshot: Option[String]): Unit =
      keyBy match
        case KeyBy.HID if causes.isEmpty && !rootLabels.contains(number) =>
          rootLabels += number -> (agent + '-' + label)
        case _ =>
    def close: Unit


  case object `Π-ConsoleCSV` extends `Π-Traces`:
    override def apply(number: Long, _causes: Set[Long],
                       clock: Double, started: Long, ended: Long,
                       agent: String, name: String, polarity: Option[Boolean],
                       key: String, guard: Boolean, label: String, _keyBy: KeyBy,
                       rate: String, _plugins: Seq[Plugin], delay: Double,
                       dir_cap: String, from: String, to: String, snapshot: Option[String]): Unit =
      printf("%d,%s,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",
             number, clock, started, ended,
             agent, name, polarity.getOrElse(""),
             key, guard, label,
             rate, delay,
             dir_cap, from, to)
    override def close: Unit = {}


  case class `Π-FileCSV`(filename: String) extends `Π-Traces`:
    import _root_.java.io.{ PrintStream, FileOutputStream }
    override def apply(number: Long, _causes: Set[Long],
                       clock: Double, started: Long, ended: Long,
                       agent: String, name: String, polarity: Option[Boolean],
                       key: String, guard: Boolean, label: String, _keyBy: KeyBy,
                       rate: String, _plugins: Seq[Plugin], delay: Double,
                       dir_cap: String, from: String, to: String, snapshot: Option[String]): Unit =
      `Π-FileCSV`.csv.printf("%s,%d,%s,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",
                             uuid,
                             number, clock, started, ended,
                             agent, name, polarity.getOrElse(""),
                             key, guard, label,
                             rate, delay,
                             dir_cap, from, to)
      if snapshot.isDefined
      then
        var ps: PrintStream = null
        try
          ps = PrintStream(FileOutputStream(uuid + '-' + number + '-' + polarity.getOrElse("") + ".xml", false), true)
          ps.println(snapshot.get)
        finally
          if ps ne null then try ps.close catch _ => {}
    override def close: Unit =
      `Π-FileCSV`.csv.close

  object `Π-FileCSV`:
    import _root_.java.io.{ PrintStream, FileOutputStream }
    lazy val csv: PrintStream =
      val config: `Π-FileCSV` = `π-traces`.asInstanceOf[`Π-FileCSV`]
      PrintStream(FileOutputStream(config.filename + ".csv", true), true)


  case class `Π-AmazonSQS`(override val backend: `Π-Backend`,
                           endpoint: String,
                           region: String,
                           accessKey: String,
                           secretKey: String,
                           queue: String) extends `Π-Traces`:
    import software.amazon.awssdk.services.sqs.model.{ DeleteQueueRequest, SendMessageRequest }
    override def apply(number: Long, causes: Set[Long],
                       clock: Double, started: Long, ended: Long,
                       agent: String, name: String, polarity: Option[Boolean],
                       key: String, guard: Boolean, label: String, _keyBy: KeyBy,
                       rate: String, plugins: Seq[Plugin], delay: Double,
                       dir_cap: String, from: String, to: String, _snapshot: Option[String]): Unit =
      super.apply(number, causes,
                  clock, started, ended,
                  agent, name, polarity,
                  key, guard, label, _keyBy,
                  rate, plugins, delay,
                  dir_cap, from, to, _snapshot)
      val keyBy =
        _keyBy match
          case KeyBy.HID => uuid + '-' + rootLabels((rootLabels.keySet & causes).headOption.getOrElse(number)).replaceAll("∥", "|")
          case KeyBy.ANY => "ANY"
          case _         => agent + '-' + label.replaceAll("∥", "|")
      val (client, queueUrl) = `Π-AmazonSQS`.client_queueUrl
      val snapshot = _snapshot.fold(null)("\"" + _.replaceAll("\"", "\\\\\\\"").replaceAll("""([\n\t])""", """\\\\$1""") + "\"")
      val message =
        s"""{
            |"uuid":"$uuid",
            |"number":$number,"causes":${causes.mkString("[", ",", "]")},
            |"clock":$clock,"started":$started,"ended":$ended,
            |"agent":"$agent","name":"$name","polarity":${polarity.getOrElse(null)},
            |"key":"$key","guard":$guard,"label":"$label","keyBy":"$keyBy",
            |"rate":"$rate","plugins":${plugins.asJson.noSpaces},
            |"delay":${if delay.isPosInfinity then null else delay},
            |"dir_cap":"$dir_cap","from":"$from","to":"$to","snapshot":$snapshot
            |}""".stripMargin.replaceAll("\n", "").trim
      val request = SendMessageRequest
        .builder
        .queueUrl(queueUrl)
        .messageGroupId(keyBy)
        .messageBody(message)
        .build
      client.sendMessage(request)
    override def close: Unit =
      val (client, queueUrl) = `Π-AmazonSQS`.client_queueUrl
      try
        client.deleteQueue(DeleteQueueRequest.builder.queueUrl(queueUrl).build)
      catch _ => {}
      finally
        client.close

  object `Π-AmazonSQS`:

    import java.net.URI

    import software.amazon.awssdk.auth.credentials.{ StaticCredentialsProvider, AwsBasicCredentials }
    import software.amazon.awssdk.regions.Region
    import software.amazon.awssdk.services.sqs.SqsClient
    import software.amazon.awssdk.services.sqs.model.CreateQueueRequest

    lazy val client_queueUrl: (SqsClient, String) =
      val config: `Π-AmazonSQS` = `π-traces`.asInstanceOf[`Π-AmazonSQS`]
      val _client = SqsClient.builder
        .credentialsProvider(StaticCredentialsProvider.create(AwsBasicCredentials.create(config.accessKey, config.secretKey)))
        .region(Region.of(config.region))
        .endpointOverride(URI.create(config.endpoint))
        .build
      _client -> _client.createQueue(CreateQueueRequest.builder().queueName(config.queue).build).queueUrl


  case class `Π-Kafka`(override val backend: `Π-Backend`,
                       servers: List[String],
                       schemaRegistryUrl: String,
                       topic: String) extends `Π-Traces`:
    import org.apache.avro.generic.{ GenericData, GenericRecord }
    import org.apache.kafka.clients.producer.ProducerRecord
    override def apply(number: Long, causes: Set[Long],
                       clock: Double, started: Long, ended: Long,
                       agent: String, name: String, polarity: Option[Boolean],
                       key: String, guard: Boolean, label: String, _keyBy: KeyBy,
                       rate: String, plugins: Seq[Plugin], delay: Double,
                       dir_cap: String, from: String, to: String, snapshot: Option[String]): Unit =
      super.apply(number, causes,
                  clock, started, ended,
                  agent, name, polarity,
                  key, guard, label, _keyBy,
                  rate, plugins, delay,
                  dir_cap, from, to, snapshot)
      val avroRecord = GenericData.Record(`Π-Kafka`.schema)
      avroRecord.put("uuid", uuid)
      avroRecord.put("number", number)
      avroRecord.put("causes", causes.asJava)
      avroRecord.put("clock", clock)
      avroRecord.put("started", started)
      avroRecord.put("ended", ended)
      avroRecord.put("agent", agent)
      avroRecord.put("name", name)
      avroRecord.put("polarity", polarity.getOrElse(null))
      avroRecord.put("key", key)
      avroRecord.put("guard", guard)
      avroRecord.put("label", label)
      avroRecord.put("rate", rate)
      avroRecord.put("plugins", plugins.map {
        case Plugin.syncRate(rate) =>
          val syncRateRecord = GenericData.Record(`Π-Kafka`.syncRatePluginSchema)
          syncRateRecord.put("rate", rate.map(_.toString).getOrElse(null))
          syncRateRecord
        case Plugin.probability(probability) =>
          val probRecord = GenericData.Record(`Π-Kafka`.probabilityPluginSchema)
          probRecord.put("probability", probability.toString)
          probRecord
        case Plugin.whatIf(factor, term) =>
          val whatIfRecord = GenericData.Record(`Π-Kafka`.whatIfPluginSchema)
          whatIfRecord.put("factor", factor.toString)
          whatIfRecord.put("term", term.map(_.toString).getOrElse(null))
          whatIfRecord
      }.asJava)
      avroRecord.put("delay", if delay.isPosInfinity then null else delay)
      avroRecord.put("dir_cap", dir_cap)
      avroRecord.put("from", from)
      avroRecord.put("to", to)
      avroRecord.put("snapshot", snapshot.map(_.replaceAll("""\\n""", "\n").replaceAll("""\\t""", "\t")).getOrElse(null))
      backend match
        case `Π-Backend`.redpanda =>
          val keyBy =
            _keyBy match
              case KeyBy.HID => s"""{"hid":"${uuid + '-' + rootLabels((rootLabels.keySet & causes).headOption.getOrElse(number))}"}"""
              case KeyBy.ANY => s"""{"label":"$agent-$label"}"""
              case _         => s"""{"label":"ANY"}"""
          avroRecord.put("keyBy", keyBy)
          val record = ProducerRecord[String, String](topic, keyBy, avroRecord.toString)
          `Π-Kafka`.Redpanda.producer.send(record)
        case _ =>
          val keyBy =
            _keyBy match
              case KeyBy.HID => uuid + '-' + rootLabels((rootLabels.keySet & causes).headOption.getOrElse(number))
              case KeyBy.ANY => "ANY"
              case _         => agent + '-' + label
          avroRecord.put("keyBy", keyBy)
          val record = ProducerRecord[String, GenericRecord](topic, keyBy, avroRecord)
          `Π-Kafka`.Kafka.producer.send(record)
    override def close: Unit =
      backend match
        case `Π-Backend`.redpanda =>
          `Π-Kafka`.Redpanda.producer.flush
          `Π-Kafka`.Redpanda.producer.close
        case _ =>
          `Π-Kafka`.Kafka.producer.flush
          `Π-Kafka`.Kafka.producer.close
      import org.apache.kafka.clients.admin.{ AdminClient, AdminClientConfig }
      val props = java.util.Properties()
      props.put(AdminClientConfig.BOOTSTRAP_SERVERS_CONFIG, servers.mkString(","))
      var adminClient: AdminClient = null
      try
        adminClient = AdminClient.create(props)
        adminClient.deleteTopics(java.util.Collections.singletonList(topic)).all.get
      catch _ => {}
      finally
        if adminClient ne null then adminClient.close

  object `Π-Kafka`:

    import org.apache.avro.generic.{ GenericData, GenericRecord }
    import org.apache.avro.Schema
    import org.apache.kafka.clients.producer.{ KafkaProducer, ProducerConfig, ProducerRecord }
    import org.apache.kafka.common.serialization.StringSerializer
    import io.confluent.kafka.serializers.{ AbstractKafkaSchemaSerDeConfig, KafkaAvroSerializer }
    import io.confluent.kafka.serializers.subject.{ RecordNameStrategy, TopicNameStrategy }

    private val _schema = """{
      "namespace": "pisc.avro",
      "type": "record",
      "name": "BioAmbients2Scala",
      "fields": [
        { "name" : "uuid", "type": "string" },

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
        { "name" : "plugins",
          "type": {
            "type": "array",
            "items": [
              { "name": "syncRate",
                "type": "record",
                "fields": [
                  { "name": "rate", "type": ["null", "string"] }
                ]
              },
              { "name": "probability",
                "type": "record",
                "fields": [
                  { "name": "probability", "type": "string" }
                ]
              },
              { "name": "whatIf",
                "type": "record",
                "fields": [
                  { "name": "factor", "type": "string" },
                  { "name": "term", "type": ["null", "string"] }
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

    val schema = Schema.Parser().parse(_schema)
    val pluginsSchema = schema.getField("plugins").schema.getElementType.getTypes
    val syncRatePluginSchema = pluginsSchema.stream.filter(_.getName == "syncRate").findFirst.get
    val probabilityPluginSchema = pluginsSchema.stream.filter(_.getName == "probability").findFirst.get
    val whatIfPluginSchema = pluginsSchema.stream.filter(_.getName == "whatIf").findFirst.get

    object Redpanda:

      lazy val producer: KafkaProducer[String, String] =
        val config: `Π-Kafka` = `π-traces`.asInstanceOf[`Π-Kafka`]
        val props = java.util.Properties()
        props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, config.servers.mkString(","))
        props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, classOf[StringSerializer])
        props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, classOf[StringSerializer])
        props.put("schema.registry.url", config.schemaRegistryUrl)
        props.put(AbstractKafkaSchemaSerDeConfig.VALUE_SUBJECT_NAME_STRATEGY, classOf[RecordNameStrategy])
        KafkaProducer[String, String](props)

    object Kafka:

      lazy val producer: KafkaProducer[String, GenericRecord] =
        val config: `Π-Kafka` = `π-traces`.asInstanceOf[`Π-Kafka`]
        val props = java.util.Properties()
        props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, config.servers.mkString(","))
        props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, classOf[StringSerializer])
        props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, classOf[KafkaAvroSerializer])
        props.put(ProducerConfig.ENABLE_IDEMPOTENCE_CONFIG, true)
        props.put("schema.registry.url", config.schemaRegistryUrl)
        props.put(AbstractKafkaSchemaSerDeConfig.VALUE_SUBJECT_NAME_STRATEGY, classOf[TopicNameStrategy])
        KafkaProducer[String, GenericRecord](props)


  case class `Π-RabbitMQ`(host: String, port: Int, exchange: String, username: String = "guest", password: String = "guest") extends `Π-Traces`:
    override def apply(number: Long, causes: Set[Long],
                       clock: Double, started: Long, ended: Long,
                       agent: String, name: String, polarity: Option[Boolean],
                       key: String, guard: Boolean, label: String, _keyBy: KeyBy,
                       rate: String, plugins: Seq[Plugin], delay: Double,
                       dir_cap: String, from: String, to: String, _snapshot: Option[String]): Unit =
      super.apply(number, causes,
                  clock, started, ended,
                  agent, name, polarity,
                  key, guard, label, _keyBy,
                  rate, plugins, delay,
                  dir_cap, from, to, _snapshot)
      val keyBy =
        _keyBy match
          case KeyBy.HID => uuid + '-' + rootLabels((rootLabels.keySet & causes).headOption.getOrElse(number))
          case KeyBy.ANY => "ANY"
          case _         => agent + '-' + label
      val snapshot = _snapshot.fold(null)("\"" + _.replaceAll("\"", "\\\\\\\"").replaceAll("""([\n\t])""", """\\\\$1""") + "\"")
      val message =
        s"""{
            |"uuid":"$uuid",
            |"number":$number,"causes":${causes.mkString("[", ",", "]")},
            |"clock":$clock,"started":$started,"ended":$ended,
            |"agent":"$agent","name":"$name","polarity":${polarity.getOrElse(null)},
            |"key":"$key","guard":$guard,"label":"$label","keyBy":"$keyBy",
            |"rate":"$rate","plugins":${plugins.asJson.noSpaces},
            |"delay":${if delay.isPosInfinity then null else delay},
            |"dir_cap":"$dir_cap","from":"$from","to":"$to","snapshot":$snapshot
            |}""".stripMargin.replaceAll("\n", "").trim
        .getBytes("UTF-8")
      `Π-RabbitMQ`.conn_channel._2.basicPublish(exchange, keyBy, null, message)
    override def close: Unit =
      try
        `Π-RabbitMQ`.conn_channel._2.exchangeDelete(exchange)
      catch _ => {}
      finally
        `Π-RabbitMQ`.conn_channel._2.close
        `Π-RabbitMQ`.conn_channel._1.close

  object `Π-RabbitMQ`:

    import com.rabbitmq.client.{ ConnectionFactory, Connection, Channel }

    lazy val conn_channel: (Connection, Channel) =
      val config: `Π-RabbitMQ` = `π-traces`.asInstanceOf[`Π-RabbitMQ`]

      val factory = ConnectionFactory()
      factory.setUsername(config.username)
      factory.setPassword(config.password)
      factory.setHost(config.host)
      factory.setPort(config.port)

      val connection = factory.newConnection
      val channel: Channel = connection.createChannel

      channel.exchangeDeclare(config.exchange, "topic", true, false, false, null)

      connection -> channel
