import sbt._

object Dependencies {
  lazy val breeze = "org.scalanlp" %% "breeze" % "2.1.0"
  lazy val scaffeine = "com.github.blemale" %% "scaffeine" % "5.3.0"
  lazy val catseffect = "org.typelevel" %% "cats-effect" % "3.7.0"
  lazy val akka = "com.typesafe.akka" %% "akka-actor-typed" % "2.10.17"
  lazy val pekko = "org.apache.pekko" %% "pekko-actor-typed" % "1.5.0"
  lazy val fs2 = "co.fs2" %% "fs2-core" % "3.13.0"
  lazy val zc = "dev.zio" %% "zio-concurrent" % "2.1.25"
  lazy val zs = "dev.zio" %% "zio-streams" % "2.1.25"
  lazy val parsercombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
  lazy val scalameta = "org.scalameta" %% "scalameta" % "4.16.0"
  lazy val munit = "org.scalameta" %% "munit" % "1.2.4"
}
