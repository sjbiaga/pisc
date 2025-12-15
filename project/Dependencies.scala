import sbt._

object Dependencies {
  lazy val catseffect = "org.typelevel" %% "cats-effect" % "3.7.0-RC1"
  lazy val akka = "com.typesafe.akka" %% "akka-actor-typed" % "2.10.12"
  lazy val pekko = "org.apache.pekko" %% "pekko-actor-typed" % "1.3.0"
  lazy val fs2 = "co.fs2" %% "fs2-core" % "3.13.0-M7"
  lazy val zs = "dev.zio" %% "zio-streams" % "2.1.23"
  lazy val parsercombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
  lazy val scalameta = "org.scalameta" %% "scalameta" % "4.14.2"
  lazy val munit = "org.scalameta" %% "munit" % "1.2.1"
}
