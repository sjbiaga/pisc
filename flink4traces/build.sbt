Compile / run := Defaults.runTask(Compile / fullClasspath, Compile / run / mainClass, Compile / run / runner).evaluated

Compile / run / fork := true

Global / cancelable := true

libraryDependencies ++= Seq("org.apache.flink" % "flink-streaming-java",
                            "org.apache.flink" % "flink-clients",
                            "org.apache.flink" % "flink-avro",
                            "org.apache.flink" % "flink-avro-confluent-registry"
                        ).map(_ % "2.2.1") ++
                        Seq("org.apache.flink" % "flink-connector-kafka" % "5.0.0-2.2") ++
                        Seq("org.java-websocket" % "Java-WebSocket" % "1.6.0")

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) =>
    xs match {
      case "MANIFEST.MF" :: Nil => MergeStrategy.discard
      case _ => MergeStrategy.first
    }
  case "reference.conf" => MergeStrategy.concat
  case _ => MergeStrategy.first
}
