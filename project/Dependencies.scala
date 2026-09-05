import sbt._

object Dependencies {
  lazy val breeze = "org.scalanlp" %% "breeze" % "2.1.0"
  lazy val scaffeine = "com.github.blemale" %% "scaffeine" % "5.3.0"
  lazy val ip4s = "com.comcast" %% "ip4s-core" % "3.8.0"
  lazy val catseffect = "org.typelevel" %% "cats-effect" % "3.7.1"
  lazy val akka = "com.typesafe.akka" %% "akka-actor-typed" % "2.10.21"
  lazy val pekko = "org.apache.pekko" %% "pekko-actor-typed" % "1.7.0"
  lazy val fs2 = "co.fs2" %% "fs2-core" % "3.13.0"
  lazy val zc = "dev.zio" %% "zio-concurrent" % "2.1.26"
  lazy val zh = "dev.zio" %% "zio-http" % "3.11.4"
  lazy val zs = "dev.zio" %% "zio-streams" % "2.1.26"
  lazy val zic = "dev.zio" %% "zio-interop-cats" % "23.1.0.13"
  lazy val parsercombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
  lazy val scalameta = "org.scalameta" %% "scalameta" % "4.17.3"
  lazy val kafka = "org.apache.kafka" % "kafka-clients" % "4.3.1"
  lazy val avro = "org.apache.avro" % "avro" % "1.12.2"
  lazy val avroʹ = "io.confluent" % "kafka-avro-serializer" % "8.3.1" exclude ("org.apache.kafka", "kafka-clients")
  lazy val rabbitmq = "com.rabbitmq" % "amqp-client" % "5.35.0"
  lazy val amazonsqs = "software.amazon.awssdk" % "sqs" % "2.54.12"
  lazy val circe = "io.circe" %% "circe-generic" % "0.14.16"
  lazy val http4s = Seq("org.http4s" %% "http4s-circe",
                        "org.http4s" %% "http4s-dsl",
                        "org.http4s" %% "http4s-ember-client",
                        "org.http4s" %% "http4s-ember-server"
                    ).map(_ % "0.23.36")
  lazy val munit = "org.scalameta" %% "munit" % "1.3.5"
}
