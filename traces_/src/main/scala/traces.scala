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

import _root_.scala.collection.immutable.List


package object `Π-traces`:

  var `π-traces`: `Π-Traces` = null


  enum `Π-Backend`:
    case same, redpanda, elasticmq


  sealed trait `Π-Traces`:
    val backend: `Π-Backend` = `Π-Backend`.same
    def apply(number: Long, clock: Double, started: Long, ended: Long,
              agent: String, name: String, polarity: Option[Boolean],
              key: String, guard: Boolean, label: String,
              rate: String, delay: Double, duration: Double,
              dir_cap: String, from: String, to: String, snapshot: Option[String]): Unit
    def close: Unit


  case object `Π-ConsoleCSV` extends `Π-Traces`:
    override def apply(number: Long, clock: Double, started: Long, ended: Long,
                       agent: String, name: String, polarity: Option[Boolean],
                       key: String, guard: Boolean, label: String,
                       rate: String, delay: Double, duration: Double,
                       dir_cap: String, from: String, to: String, snapshot: Option[String]): Unit =
      printf("%d,%d,%s,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",
             ProcessHandle.current.pid,
             number, clock, started, ended,
             agent, name, polarity.getOrElse(""),
             key, guard, label,
             rate, delay, duration,
             dir_cap, from, to)
    override def close: Unit = {}


  case class `Π-FileCSV`(filename: String) extends `Π-Traces`:
    import _root_.java.io.{ PrintStream, FileOutputStream }
    override def apply(number: Long, clock: Double, started: Long, ended: Long,
                       agent: String, name: String, polarity: Option[Boolean],
                       key: String, guard: Boolean, label: String,
                       rate: String, delay: Double, duration: Double,
                       dir_cap: String, from: String, to: String, snapshot: Option[String]): Unit =
      `Π-FileCSV`.csv.printf("%d,%d,%s,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",
                             ProcessHandle.current.pid,
                             number, clock, started, ended,
                             agent, name, polarity.getOrElse(""),
                             key, guard, label,
                             rate, delay, duration,
                             dir_cap, from, to)
      if snapshot.isDefined
      then
        var ps: PrintStream = null
        try
          ps = PrintStream(FileOutputStream("" + number + "-" + polarity.getOrElse("") + ".xml", false), true)
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
    override def apply(number: Long, clock: Double, started: Long, ended: Long,
                       agent: String, name: String, polarity: Option[Boolean],
                       key: String, guard: Boolean, label: String,
                       rate: String, delay: Double, duration: Double,
                       dir_cap: String, from: String, to: String, snapshot: Option[String]): Unit =
      val (client, queueUrl) = `Π-AmazonSQS`.client_queueUrl
      val message = s"""
                    {"pid":${ProcessHandle.current.pid},
                     "number":$number,"clock":$clock,"started":$started,"ended":$ended,
                     "agent":"$agent","name":"$name","polarity":${polarity.getOrElse(null)},
                     "key":"$key","guard":$guard,"label":"$label",
                     "rate":"$rate","delay":$delay,"duration":${if duration.isNaN then null else duration},
                     "dir_cap":"$dir_cap","from":"$from","to":"$to","snapshot":"${snapshot.getOrElse(null)}"
                    }"""
      val request = SendMessageRequest
        .builder
        .queueUrl(queueUrl)
        .messageBody(message)
        .build
      client.sendMessage(request)
    override def close: Unit =
      val (client, queueUrl) = `Π-AmazonSQS`.client_queueUrl
      client.deleteQueue(DeleteQueueRequest.builder.queueUrl(queueUrl).build)
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
    override def apply(number: Long, clock: Double, started: Long, ended: Long,
                       agent: String, name: String, polarity: Option[Boolean],
                       key: String, guard: Boolean, label: String,
                       rate: String, delay: Double, duration: Double,
                       dir_cap: String, from: String, to: String, snapshot: Option[String]): Unit =
      val avroRecord = GenericData.Record(`Π-Kafka`.schema)
      avroRecord.put("pid", ProcessHandle.current.pid)
      avroRecord.put("number", number)
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
      avroRecord.put("delay", delay)
      avroRecord.put("duration", if duration.isNaN then null else duration)
      avroRecord.put("dir_cap", dir_cap)
      avroRecord.put("from", from)
      avroRecord.put("to", to)
      avroRecord.put("snapshot", snapshot.getOrElse(null))
      backend match
        case `Π-Backend`.redpanda =>
          val record = ProducerRecord[String, String](topic, s"""{"label":"$label"}""", avroRecord.toString)
          `Π-Kafka`.Redpanda.producer.send(record)
        case _ =>
          val record = ProducerRecord[String, GenericRecord](topic, label, avroRecord)
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
      finally
        if adminClient ne null then adminClient.close

  object `Π-Kafka`:

    import org.apache.avro.generic.{ GenericData, GenericRecord }
    import org.apache.avro.Schema
    import org.apache.kafka.clients.producer.{ KafkaProducer, ProducerConfig, ProducerRecord }
    import org.apache.kafka.common.serialization.StringSerializer
    import io.confluent.kafka.serializers.{ AbstractKafkaSchemaSerDeConfig, KafkaAvroSerializer }
    import io.confluent.kafka.serializers.subject.RecordNameStrategy

    private val _schema = """{
      "namespace": "pisc",
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
        props.put("schema.registry.url", config.schemaRegistryUrl)
        props.put(AbstractKafkaSchemaSerDeConfig.VALUE_SUBJECT_NAME_STRATEGY, classOf[RecordNameStrategy])
        KafkaProducer[String, GenericRecord](props)


  case class `Π-RabbitMQ`(host: String, port: Int, queue: String, username: String = "guest", password: String = "guest") extends `Π-Traces`:
    override def apply(number: Long, clock: Double, started: Long, ended: Long,
                       agent: String, name: String, polarity: Option[Boolean],
                       key: String, guard: Boolean, label: String,
                       rate: String, delay: Double, duration: Double,
                       dir_cap: String, from: String, to: String, snapshot: Option[String]): Unit =
      val message = s"""
                    {"pid":${ProcessHandle.current.pid},
                     "number":$number,"clock":$clock,"started":$started,"ended":$ended,
                     "agent":"$agent","name":"$name","polarity":${polarity.getOrElse(null)},
                     "key":"$key","guard":$guard,"label":"$label",
                     "rate":"$rate","delay":$delay,"duration":${if duration.isNaN then null else duration},
                     "dir_cap":"$dir_cap","from":"$from","to":"$to","snapshot":"${snapshot.getOrElse(null)}"
                    }"""
        .getBytes("UTF-8")
      `Π-RabbitMQ`.conn_channel._2.basicPublish("", queue, null, message)
    override def close: Unit =
      `Π-RabbitMQ`.conn_channel._2.queueDelete(queue)
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

      channel.queueDeclare(config.queue, true, false, false, null)

      connection -> channel
