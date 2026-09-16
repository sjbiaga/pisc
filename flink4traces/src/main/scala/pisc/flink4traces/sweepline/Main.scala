package pisc
package flink4traces
package sweepline

import org.apache.flink.connector.kafka.source.KafkaSource
import org.apache.flink.connector.kafka.source.enumerator.initializer.OffsetsInitializer

import org.apache.flink.api.common.eventtime.WatermarkStrategy
import org.apache.flink.util.ParameterTool

import org.apache.avro.generic.GenericRecord
import org.apache.flink.formats.avro.registry.confluent.ConfluentRegistryAvroDeserializationSchema

import org.apache.flink.streaming.api.environment.StreamExecutionEnvironment
import org.apache.flink.streaming.api.datastream.DataStream


object Main:

  def main(args: Array[String]): Unit =

    val params = ParameterTool.fromArgs(args)

    val kafkaTopic = params.getRequired("topic")
    val port = params.getInt("port", 7224)
    val kafkaBrokers = params.get("bootstrap-servers", "kafka:29092")
    val schemaRegistryUrl = params.get("schema-registry", "http://schema-registry:8081")
    val windowDuration = params.getLong("window-duration", 5L)

    val env = StreamExecutionEnvironment.getExecutionEnvironment

    env.getConfig.setGlobalJobParameters(params)

    val avroDeserializer = ConfluentRegistryAvroDeserializationSchema.forGeneric(schema, schemaRegistryUrl)

    val kafkaSource = KafkaSource.builder[GenericRecord]()
      .setBootstrapServers(kafkaBrokers)
      .setTopics(kafkaTopic)
      .setGroupId("flink-functions4traces-sweepline-group")
      .setStartingOffsets(OffsetsInitializer.latest())
      .setValueOnlyDeserializer(avroDeserializer)
      .build()

    val avroRecordStream = env.fromSource(
      kafkaSource,
      WatermarkStrategy.noWatermarks(),
      "Kafka Avro Traces"
    )

    val tracesStream: DataStream[Traces] =
      avroRecordStream.flatMap(Traces.GenericRecord2Traces)

    SweepLinePipeline(tracesStream, windowDuration, kafkaTopic, port)

    env.execute(s"flink-functions4traces-analytics-sweepline")
