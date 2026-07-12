import sbt._

object Dependencies {
  lazy val breeze = "org.scalanlp" %% "breeze" % "2.1.0"
  lazy val scaffeine = "com.github.blemale" %% "scaffeine" % "5.3.0"
  lazy val catseffect = "org.typelevel" %% "cats-effect" % "3.7.0"
  lazy val catsstm = "io.github.timwspence" %% "cats-stm" % "0.13.5"
  lazy val fs2 = "co.fs2" %% "fs2-core" % "3.13.0"
  lazy val zc = "dev.zio" %% "zio-concurrent" % "2.1.26"
  lazy val zs = "dev.zio" %% "zio-streams" % "2.1.26"
  lazy val zic = "dev.zio" %% "zio-interop-cats" % "23.1.0.13"
  lazy val parsercombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
  lazy val scalameta = "org.scalameta" %% "scalameta" % "4.17.2"
  lazy val munit = "org.scalameta" %% "munit" % "1.3.3"
}
