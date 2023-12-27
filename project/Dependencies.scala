import sbt._

object Dependencies {
  lazy val parsercombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"
  lazy val catseffect = "org.typelevel" %% "cats-effect" % "3.5.2"
  lazy val munit = "org.scalameta" %% "munit" % "0.7.29"
}
