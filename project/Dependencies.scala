import sbt._

object Dependencies {
  lazy val catseffect = "org.typelevel" %% "cats-effect" % "3.7.0"
  lazy val catsstm = "io.github.timwspence" %% "cats-stm" % "0.13.5"
  lazy val zio = "dev.zio" %% "zio" % "2.1.26"
  lazy val parsercombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
  lazy val scalameta = "org.scalameta" %% "scalameta" % "4.17.3"
  lazy val munit = "org.scalameta" %% "munit" % "1.3.5"
}
