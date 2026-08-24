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


  sealed trait `Π-Traces`:
    def apply(number: Long, clock: Double, started: Long, ended: Long,
              agent: String, name: String, polarity: Option[Boolean],
              key: String, guard: Boolean, label: String,
              rate: String, delay: Double, duration: Double): Unit
    def close: Unit


  case object `Π-ConsoleCSV` extends `Π-Traces`:
    override def apply(number: Long, clock: Double, started: Long, ended: Long,
                       agent: String, name: String, polarity: Option[Boolean],
                       key: String, guard: Boolean, label: String,
                       rate: String, delay: Double, duration: Double): Unit =
      printf("%d,%s,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",
             number, clock, started, ended,
             agent, name, polarity.getOrElse(""),
             key, guard, label,
             rate, delay, duration)
    override def close: Unit = {}


  case class `Π-FileCSV`(filename: String) extends `Π-Traces`:
    import _root_.java.io.{ PrintStream, FileOutputStream }
    override def apply(number: Long, clock: Double, started: Long, ended: Long,
                       agent: String, name: String, polarity: Option[Boolean],
                       key: String, guard: Boolean, label: String,
                       rate: String, delay: Double, duration: Double): Unit =
      `Π-FileCSV`.csv.printf("%d,%s,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",
                             number, clock, started, ended,
                             agent, name, polarity.getOrElse(""),
                             key, guard, label,
                             rate, delay, duration)
    override def close: Unit =
      `Π-FileCSV`.csv.close

  object `Π-FileCSV`:
    import _root_.java.io.{ PrintStream, FileOutputStream }
    lazy val csv: PrintStream =
      val config: `Π-FileCSV` = `π-traces`.asInstanceOf[`Π-FileCSV`]
      PrintStream(FileOutputStream(config.filename + ".csv", true), true)


  case class `Π-AmazonSQS`(endpoint: String, region: String, accessKey: String, secretKey: String, queue: String) extends `Π-Traces`:
    import software.amazon.awssdk.services.sqs.model.{ DeleteQueueRequest, SendMessageRequest }
    override def apply(number: Long, clock: Double, started: Long, ended: Long,
                       agent: String, name: String, polarity: Option[Boolean],
                       key: String, guard: Boolean, label: String,
                       rate: String, delay: Double, duration: Double): Unit =
      val (client, queueUrl) = `Π-AmazonSQS`.client_queueUrl
      val message = s"""
                    {"number":$number,"clock":$clock,"started":$started,"ended":$ended,
                     "agent":"$agent","name":"$name","polarity":${polarity.getOrElse(null)},
                     "key":"$key","guard":$guard,"label":"$label",
                     "rate":"$rate","delay":$delay,"duration":$duration
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


  case class `Π-Kafka`(servers: List[String], topic: String) extends `Π-Traces`:
    import org.apache.avro.generic.{ GenericData, GenericRecord }
    import org.apache.kafka.clients.producer.ProducerRecord
    override def apply(number: Long, clock: Double, started: Long, ended: Long,
                       agent: String, name: String, polarity: Option[Boolean],
                       key: String, guard: Boolean, label: String,
                       rate: String, delay: Double, duration: Double): Unit =
      val avroRecord = GenericData.Record(`Π-Kafka`.schema)
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
      avroRecord.put("duration", duration)
      val record = ProducerRecord[String, GenericRecord](topic, "pisc", avroRecord)
      `Π-Kafka`.producer.send(record).get
    override def close: Unit =
      `Π-Kafka`.producer.close

  object `Π-Kafka`:

    import org.apache.avro.generic.{ GenericData, GenericRecord }
    import org.apache.avro.Schema
    import org.apache.kafka.clients.producer.{ KafkaProducer, ProducerConfig, ProducerRecord }
    import org.apache.kafka.common.serialization.StringSerializer
    import io.confluent.kafka.serializers.KafkaAvroSerializer

    private val _schema = """{
      "namespace": "pisc.avro",
      "type": "record",
      "name": "pisc",
      "fields": [
        { "name" : "number", "type": "long" },
        { "name" : "clock", "type": "number" },
        { "name" : "started", "type": "long" },
        { "name" : "ended", "type": "long" },

        { "name" : "agent", "type": "string" },
        { "name" : "name", "type": "string" },
        { "name" : "polarity", "type": ["null", "boolean"] },

        { "name" : "key", "type": "string" },
        { "name" : "guard", "type": "boolean" },
        { "name" : "label", "type": "string" },

        { "name" : "rate", "type": "string" },
        { "name" : "delay", "type": "number" },
        { "name" : "duration", "type": "number" }
      ]
    }"""

    val schema = Schema.Parser().parse(_schema)

    lazy val producer: KafkaProducer[String, GenericRecord] =
      val config: `Π-Kafka` = `π-traces`.asInstanceOf[`Π-Kafka`]
      val props = java.util.Properties()
      props.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, config.servers.mkString(","))
      props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, classOf[StringSerializer]) //.getCanonicalName)
      props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, classOf[KafkaAvroSerializer]) //.getCanonicalName)
      props.put("schema.registry.url", "http://localhost:8081")
      KafkaProducer[String, GenericRecord](props)


  case class `Π-RabbitMQ`(host: String, port: Int, queue: String) extends `Π-Traces`:
    override def apply(number: Long, clock: Double, started: Long, ended: Long,
                       agent: String, name: String, polarity: Option[Boolean],
                       key: String, guard: Boolean, label: String,
                       rate: String, delay: Double, duration: Double): Unit =
      val message = s"""
                    {"number":$number,"clock":$clock,"started":$started,"ended":$ended,
                     "agent":"$agent","name":"$name","polarity":${polarity.getOrElse(null)},
                     "key":"$key","guard":$guard,"label":"$label",
                     "rate":"$rate","delay":$delay,"duration":$duration
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
      factory.setHost(config.host)
      factory.setPort(config.port)

      val connection = factory.newConnection
      val channel: Channel = connection.createChannel

      channel.queueDeclare(config.queue, true, false, false, null)

      connection -> channel
