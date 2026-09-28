package basc
package flink4traces
package isd

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
    val kafkaBrokers = params.get("bootstrap-servers", "kafka:29092")
    val schemaRegistryUrl = params.get("schema-registry", "http://schema-registry:8081")
    val port = params.getInt("port", 7424)
    val windowDuration = params.getLong("window-duration", 60000)
    val windowInterval = params.getLong("window-interval", 10000)
    val allowedLateness = params.getLong("allowed-lateness", 5000)
    //val weightBlowoutThreshold = params.getDouble("weight-blowout-threshold", 1.1) // 1.1 -> 1.6 : aggressive ≈ 3x -> 5x
    //val weightBlowoutThreshold = params.getDouble("weight-blowout-threshold", 2.3) // 2.3 -> 3.0 : standard ≈ 10x -> 20x
    //val weightBlowoutThreshold = params.getDouble("weight-blowout-threshold", 4.6) // catastrophic ≈ 100x
    val weightBlowoutThreshold = params.getDouble("weight-blowout-threshold", 2.0) // 9x

    val env = StreamExecutionEnvironment.getExecutionEnvironment

    env.getConfig.setGlobalJobParameters(params)

    val avroDeserializer = ConfluentRegistryAvroDeserializationSchema.forGeneric(schema, schemaRegistryUrl)

    val kafkaSource = KafkaSource.builder[GenericRecord]()
      .setBootstrapServers(kafkaBrokers)
      .setTopics(kafkaTopic)
      .setGroupId("flink-functions4traces-isd-group")
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

    ISDPipeline(tracesStream,
                windowDuration,
                windowInterval,
                allowedLateness,
                weightBlowoutThreshold,
                port,
                kafkaTopic)

    env.execute(s"flink-functions4traces-analytics-isd")
