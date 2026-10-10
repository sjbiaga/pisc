package pisc
package beam4traces
package isd

import scala.jdk.CollectionConverters._

import com.spotify.scio._
import com.spotify.scio.values.WindowOptions

import org.apache.beam.sdk.coders.StringUtf8Coder
import org.apache.beam.sdk.transforms.ParDo
import org.apache.beam.sdk.extensions.avro.coders.AvroCoder
//import org.apache.beam.sdk.io.elasticsearch.ElasticsearchIO
import org.apache.beam.sdk.io.kafka.KafkaIO
import org.apache.beam.sdk.values.KV

import org.apache.avro.generic.GenericRecord
import org.apache.kafka.common.serialization.StringDeserializer

import org.joda.time.{ Duration, Instant }


object Main {

  def main(cmdlineArgs: Array[String]): Unit = {

    // 1. Initialize the Scio Context
    val (sc, args) = ContextAndArgs(cmdlineArgs)

    // Define configuration values (can alternatively be passed via `args`)
    val kafkaBootstrap = args.optional("kafka-bootstrap").getOrElse("localhost:9092")
    val schemaRegistryUrl = args.optional("schema-registry").getOrElse("http://localhost:8081")
    val kafkaTopic        = args.required("kafka-topic")

    val opensearchHost = args.optional("opensearch-host").getOrElse("localhost")
    val opensearchPort = args.int("opensearch-port", 9200)
    val opensearchUser = args.optional("opensearch-user").getOrElse("admin")
    val opensearchPass = args.optional("opensearch-pass").getOrElse("")

    val windowDuration = args.long("window-duration", 5)
    val degeneracyThreshold = args.double("degeneracy-threshold", -5.0)
    //val blowoutThreshold = args.double("blowout-threshold", 1.1) // 1.1 -> 1.6 : aggressive ≈ 3x -> 5x
    //val blowoutThreshold = args.double("blowout-threshold", 2.3) // 2.3 -> 3.0 : standard ≈ 10x -> 20x
    val blowoutThreshold = args.double("blowout-threshold", 4.6) // catastrophic ≈ 100x

    // 2. Configure Kafka Consumer Options with Schema Registry details
    val kafkaConsumerConfig = Map[String, AnyRef](
      "bootstrap.servers" -> kafkaBootstrap,
      "schema.registry.url" -> schemaRegistryUrl,
      "group.id" -> "pisc-beam4traces-isd",
      "auto.offset.reset" -> "latest",
      "enable.auto.commit" -> "false",
      "value.subject.name.strategy" -> "io.confluent.kafka.serializers.subject.TopicNameStrategy"
    ).asJava

    // 3. Build the KafkaIO read step using Apache Beam's Java SDK underlying integration
    val kafkaSource = KafkaIO.read[String, GenericRecord]()
      .withBootstrapServers(kafkaBootstrap)
      .withTopic(kafkaTopic)
      .withKeyDeserializerAndCoder(
        classOf[StringDeserializer],
        StringUtf8Coder.of()
      )
      .withValueDeserializerAndCoder(
        classOf[KafkaAvroDeserializer],
        AvroCoder.of(classOf[GenericRecord], schema)
      )
      .withConsumerConfigUpdates(kafkaConsumerConfig)
      .withoutMetadata() // Returns just the KV pair instead of full KafkaRecord metadata

    // 4. Build the Pipeline graph
    val counterfactualStream = sc.customInput("ReadFromKafka", kafkaSource) // Wraps standard Beam Java IO inside Scio
      .map(kv => Traces(kv.getValue))
      .map(traces => KV.of(traces.keyBy, traces))
      .applyTransform(ParDo.of(new StatefulImportanceSamplingFn(degeneracyThreshold, blowoutThreshold)))
      .timestampBy(it => new Instant(it.timestamp.getTime))
      .withFixedWindows(
        duration = Duration.standardSeconds(windowDuration),
        options = WindowOptions()
      )

    val essStream = counterfactualStream
      .map(it => (it.hid, java.lang.Double.valueOf(it.logWeight)))
      .groupByKey
      .map { case (hid, logWeights) => KV.of(hid, logWeights.asJava) }
      .applyTransform(ParDo.of(new ESSDiagnosticsFn()))
      .map(it => (it.hid, it))
      .asMapSideInput

    val rcaStream = counterfactualStream
      .filter(it => it.isDegeneracyAlert || it.isBlowoutAlert)
      .withSideInputs(essStream)
      .map { (result, ctx) =>
        val ess = ctx(essStream).get(result.hid)
        rca.RootCauseReport(result.hid,
                            result.number,
                            result.clock,
                            new Instant(result.timestamp.getTime).toString,
                            result.logWeight,
                            result.importanceWeight,
                            if (result.isDegeneracyAlert) "DEGENERACY" else "BLOWOUT",
                            ess.map(_.count).getOrElse(0L),
                            ess.map(_.effectiveSampleSize).getOrElse(.0),
                            ess.map(_.degeneracyRatio).getOrElse(.0),
                            result.criticalPath)
      }
      .toSCollection

    val index = "stochasticpicalculus_beam4traces_isd_rca"

    // if (opensearchPass.isEmpty)
    //   rcaStream
    //     .map(_.toJson)
    //     .saveAsCustomOutput(
    //       "WriteToOpenSearch",
    //       ElasticsearchIO.write()
    //         .withIdFn(rca.RootCauseReport.docIdFn)
    //         .withUseStatefulBatches(true)            // Optimizes memory usage for streaming writes
    //         .withMaxBatchSize(1000)                  // Max documents per bulk index call
    //         .withMaxBatchSizeBytes(4 * 1024 * 1024)  // 5 MB max bulk payload
    //         .withConnectionConfiguration(
    //           ElasticsearchIO.ConnectionConfiguration
    //             .create(Array(s"http://$opensearchHost:$opensearchPort"), index, "_doc")
    //         )
    //     )
    // else
    //   rcaStream
    //     .map(_.toJson)
    //     .saveAsCustomOutput(
    //       "WriteToOpenSearch",
    //       ElasticsearchIO.write()
    //         .withIdFn(rca.RootCauseReport.docIdFn)
    //         .withUseStatefulBatches(true)            // Optimizes memory usage for streaming writes
    //         .withMaxBatchSize(1000)                  // Max documents per bulk index call
    //         .withMaxBatchSizeBytes(4 * 1024 * 1024)  // 5 MB max bulk payload
    //         .withConnectionConfiguration(
    //           ElasticsearchIO.ConnectionConfiguration
    //             .create(Array(s"https://$opensearchHost:$opensearchPort"), index, "_doc")
    //             .withUsername(opensearchUser)
    //             .withPassword(opensearchPass)
    //             .withTrustSelfSignedCerts(true)
    //         )
    //     )

    rcaStream
      .map { it => (it.hid + '_' + it.number) -> it.toJson }
      .applyTransform {
        ParDo.of {
          new OpenSearchWriteFn(opensearchHost,
                                opensearchPort,
                                opensearchUser,
                                opensearchPass,
                                index,
                                1000)
        }
      }

    // 5. Execute the streaming pipeline (Runs continuously on your Direct Runner)
    sc.run().waitUntilFinish(scala.concurrent.duration.Duration.Inf)
  }

}
