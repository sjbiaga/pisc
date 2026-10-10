libraryDependencies ++= Seq("org.apache.spark" %% "spark-sql",
                            "org.apache.spark" %% "spark-streaming").map(_ % "4.2.0" % "provided") ++
                        Seq("org.apache.spark" %% "spark-sql-kafka-0-10",
                            "org.apache.spark" %% "spark-avro").map(_ % "4.2.0") ++
                        Seq("org.opensearch.client" %% "opensearch-spark-40" % "2.0.0" excludeAll(
                              ExclusionRule(organization = "org.apache.avro"),
                              ExclusionRule(organization = "org.apache.spark")
                            ),
                            "com.github.scopt" %% "scopt" % "4.1.0")

assembly / assemblyMergeStrategy := {
  case PathList("org", "apache", "avro", _*) => MergeStrategy.discard
  case PathList("META-INF", xs @ _*) =>
    xs map { _.toLowerCase } match {
      case "manifest.mf" :: Nil | "index.list" :: Nil | "dependencies" :: Nil =>
        MergeStrategy.discard
      case ps if ps.last.endsWith(".sf") || ps.last.endsWith(".dsa") || ps.last.endsWith(".rsa") =>
        MergeStrategy.discard
      case "services" :: _ =>
        MergeStrategy.concat
      case _ =>
        MergeStrategy.discard
    }
  case "reference.conf" => MergeStrategy.concat
  case _                => MergeStrategy.first
}
