import sbt._

object Dependencies {
  lazy val catseffect = "org.typelevel" %% "cats-effect" % "3.7.0"
  lazy val akka = "com.typesafe.akka" %% "akka-actor-typed" % "2.10.19"
  lazy val pekko = "org.apache.pekko" %% "pekko-actor-typed" % "1.6.0"
  lazy val fs2 = "co.fs2" %% "fs2-core" % "3.13.0"
  lazy val monix = "io.monix" %% "monix" % "3.4.1"
  lazy val zc = "dev.zio" %% "zio-concurrent" % "2.1.26"
  lazy val zs = "dev.zio" %% "zio-streams" % "2.1.26"
  lazy val parsercombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
  lazy val scalameta = "org.scalameta" %% "scalameta" % "4.17.0"
  lazy val munit = "org.scalameta" %% "munit" % "1.3.3"
}
