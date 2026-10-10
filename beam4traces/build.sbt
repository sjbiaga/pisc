val scioVersion = "0.15.10"
val beamVersion = "2.76.0"
val openSearchVersion = "3.10.0"

libraryDependencies ++= Seq(
  // Core Scio & Local Runner
  "com.spotify" %% "scio-core" % scioVersion,
  "org.apache.beam" % "beam-runners-direct-java" % beamVersion,

  // 2. Kafka Connection & Avro Handling
  "org.apache.beam" % "beam-sdks-java-extensions-avro" % beamVersion,
  "org.apache.beam" % "beam-sdks-java-io-kafka" % beamVersion,
  "org.apache.avro" % "avro" % "1.12.2",
  "io.confluent" % "kafka-avro-serializer" % "7.6.14", // Provides KafkaAvroDeserializer

  // 3. Target Database: Elasticsearch / OpenSearch Integration
  "org.apache.beam" % "beam-sdks-java-io-elasticsearch" % beamVersion,

  "org.opensearch.client" % "opensearch-java" % openSearchVersion,

  "io.circe" %% "circe-generic" % "0.14.17",

  // Logging Runtime
  "org.slf4j" % "slf4j-simple" % "2.0.20"
)

// Prevent library evictions or dependency version mismatch crashes during runtime
dependencyOverrides ++= Seq(
  "org.apache.beam" % "beam-sdks-java-core" % beamVersion,
  "com.fasterxml.jackson.core"     % "jackson-core"         % "2.15.4",
  "com.fasterxml.jackson.core"     % "jackson-databind"     % "2.15.4",
  "com.fasterxml.jackson.core"     % "jackson-annotations"  % "2.15.4",
  "com.fasterxml.jackson.module"  %% "jackson-module-scala" % "2.15.4",
  "com.github.luben" % "zstd-jni" % "1.5.6-3"
)

fork := true

javaOptions += "-Dorg.apache.avro.SERIALIZABLE_PACKAGES=java.util,org.apache.beam"
