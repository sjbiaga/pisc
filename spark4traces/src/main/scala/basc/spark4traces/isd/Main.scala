package basc
package spark4traces
package isd

import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._
import org.apache.spark.sql.avro.functions._

//import org.opensearch.spark.sql._

import scopt.OParser

import org.apache.spark.sql.streaming.{ GroupStateTimeout, OutputMode }


case class AppConfig(kafkaBootstrap: String = "kafka:29092",
                     schemaRegistryURL: String = "http://schema-registry:8081",
                     kafkaTopic: String = "topic",
                     opensearchHost: String = "opensearch",
                     opensearchPort: Int = 9200,
                     opensearchUser: String = "admin",
                     opensearchPass: String = "Admin!Password-123",
                     watermarkInterval: Long = 10,
                     windowDuration: Long = 5,
                     degeneracyThreshold: Double = -5.0,
                     //blowoutThreshold: Double = 1.1) // 1.1 -> 1.6 : aggressive ≈ 3x -> 5x
                     //blowoutThreshold: Double = 2.3) // 2.3 -> 3.0 : standard ≈ 10x -> 20x
                     blowoutThreshold: Double = 4.6) // catastrophic ≈ 100x


object Main {

  def main(args: Array[String]): Unit = {

    val builder = OParser.builder[AppConfig]
    val parser = {
      import builder._
      OParser.sequence(
        programName("Importance Sampling Diagnostics"),
        head("IS Diagnostics Spark Sink", "1.0"),

        opt[String]('k', "kafka-bootstrap")
          .action((x, c) => c.copy(kafkaBootstrap = x))
          .text("Kafka bootstrap servers (host:port)"),

        opt[String]('r', "schema-registry")
          .action((x, c) => c.copy(schemaRegistryURL = x))
          .text("Schema registry URL (http://host:port)"),

        opt[String]('t', "kafka-topic")
          .action((x, c) => c.copy(kafkaTopic = x))
          .text("Kafka topic to subscribe to"),

        opt[String]('o', "opensearch-host")
          .action((x, c) => c.copy(opensearchHost = x))
          .text("OpenSearch cluster host"),

        opt[Int]('p', "opensearch-port")
          .action((x, c) => c.copy(opensearchPort = x))
          .text("OpenSearch REST API port"),

        opt[String]('u', "opensearch-user")
          .action((x, c) => c.copy(opensearchUser = x))
          .text("OpenSearch cluster user"),

        opt[String]('s', "opensearch-pass")
          .action((x, c) => c.copy(opensearchPass = x))
          .text("OpenSearch cluster password"),

        opt[Long]('w', "watermark-interval")
          .action((x, c) => c.copy(watermarkInterval = x))
          .text("Watermark interval in seconds"),

        opt[Long]('i', "window-duration")
          .action((x, c) => c.copy(windowDuration = x))
          .text("Window duration in seconds"),

        opt[Double]('d', "degeneracy-threshold")
          .action((x, c) => c.copy(degeneracyThreshold = x))
          .text("Degeneracy threshold"),

        opt[Double]('b', "blowout-threshold")
          .action((x, c) => c.copy(blowoutThreshold = x))
          .text("Blowout threshold")
      )
    }

    OParser.parse(parser, args, AppConfig()) match {
      case Some(config) => run(config)
      case None =>
        System.exit(1)
    }

  }

  private def run(config: AppConfig): Unit = {

    val spark = SparkSession.builder()
      .appName("Importance Sampling Diagnostics")
      .master("local[*]")
      .getOrCreate()
    import spark.implicits._

    val rawInputStream = spark.readStream
      .format("kafka")
      .option("kafka.bootstrap.servers", config.kafkaBootstrap)
      .option("subscribe", config.kafkaTopic)
      .option("startingOffsets", "earliest")
      .load()

    val parsedStream = rawInputStream
      .select(
        from_avro(
          expr("substring(value, 6)"), // Strips 5-byte Confluent wire format header
          schema.toString,
        ).as("data")
      ).select("data.*")

    val tracesStream = parsedStream.mapPartitions(_.map((row2GenericRecord(_, schema)) andThen Traces.apply))

    // tracesStream.writeStream
    //   .format("console")
    //   .option("truncate", false)
    //   .outputMode(OutputMode.Append())
    //   .start()
    //   .awaitTermination()

    val counterfactualStreamʹ = tracesStream
      .groupByKey(_.keyBy)
      .flatMapGroupsWithState[TraceState, CounterfactualWeightResult](
        outputMode = OutputMode.Append(),
        timeoutConf = GroupStateTimeout.NoTimeout()
      )(ImportanceSamplingProcessor(config.degeneracyThreshold, config.blowoutThreshold))

    val counterfactualStream = counterfactualStreamʹ
      .withWatermark("timestamp", s"${config.watermarkInterval} seconds")

    // counterfactualStream.writeStream
    //   .format("console")
    //   .option("truncate", false)
    //   .outputMode(OutputMode.Append())
    //   .start()
    //   .awaitTermination()

    val alertStream = counterfactualStream
      .filter($"isDegeneracyAlert" || $"isBlowoutAlert")
      .select(
        $"hid",
        $"number",
        $"timestamp".as("alertTimestamp"),
        $"logWeight",
        $"importanceWeight",
        when($"isDegeneracyAlert", "DEGENERACY").otherwise("BLOWOUT").as("anomalyType"),
        $"criticalPath"
      )

    // alertStream.writeStream
    //   .format("console")
    //   .option("truncate", false)
    //   .outputMode(OutputMode.Append())
    //   .start()
    //   .awaitTermination()

    val essUdaf = udaf(new EssAggregator())

    val essStream = counterfactualStream
      .groupBy($"hid", window($"timestamp", s"${config.windowDuration} seconds"))
      .agg(essUdaf($"logWeight").as("metrics"))
      .select(
        $"hid",
        $"window.start".as("windowStart"),
        $"window.end".as("windowEnd"),
        $"metrics.count".as("activeTrajectories"),
        $"metrics.ess".as("effectiveSampleSize"),
        ($"metrics.ess" / $"metrics.count").as("degeneracyRatio"),
        $"metrics.maxLogWeight".as("maxLogWeight"),
        $"metrics.minLogWeight".as("minLogWeight")
      ).as[EssWindowDiagnostics]

    // essStream.writeStream
    //   .format("console")
    //   .option("truncate", false)
    //   .outputMode(OutputMode.Update())
    //   .start()
    //   .awaitTermination()

    val rcaStream = alertStream.as("a")
      .join(essStream.as("s"),
            $"a.hid" === $"s.hid" &&
            $"alertTimestamp" >= $"windowStart" &&
            $"alertTimestamp" <= $"windowEnd",
            joinType = "inner" // Emits as soon as window boundary resolves
      ).select(
        $"a.hid",
        $"number",
        $"alertTimestamp".as("timestamp"),
        $"logWeight",
        $"importanceWeight",
        $"anomalyType",
        $"activeTrajectories",
        $"effectiveSampleSize",
        $"degeneracyRatio",
        $"criticalPath"
      ).as[rca.RootCauseReport]

    // rcaStream.writeStream
    //   .format("console")
    //   .option("truncate", false)
    //   .outputMode(OutputMode.Append())
    //   .start()
    //   .awaitTermination()

    rcaStream
      // Generate a unique document ID for idempotent writes
      .withColumn("doc_id", concat($"hid", lit("_"), $"number"))

      .writeStream

      .format("opensearch")
      // Target index
      .option("opensearch.resource", "bioambients_spark4traces_isd_rca")

      // Connection details
      .option("opensearch.nodes", s"https://${config.opensearchHost}:${config.opensearchPort}")
      .option("opensearch.nodes.wan.only", true) // Essential when running in containerized/K8s environments
      .option("opensearch.net.ssl", true)
      .option("opensearch.net.ssl.cert.allow.self.signed", true)
      .option("opensearch.net.ssl.verify", false)

      // Authentication
      .option("opensearch.net.http.auth.user", config.opensearchUser)
      .option("opensearch.net.http.auth.pass", config.opensearchPass)

      // Mapping & Routing Configuration
      .option("opensearch.mapping.id", "doc_id") // Use doc_id as document _id for idempotency
      .option("opensearch.batch.size.entries", 1024)
      .option("opensearch.batch.write.retry.count", 3)
      .option("opensearch.batch.write.retry.wait", "5s")

      .option("checkpointLocation", "/tmp/spark-checkpoints/opensearch-rca-alerts")

      // Streaming execution mode
      .outputMode(OutputMode.Append())
      .start()
      .awaitTermination()

  }

}
