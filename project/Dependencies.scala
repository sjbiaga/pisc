import sbt._

object Dependencies {
  lazy val parsercombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
  lazy val scalameta = ("org.scalameta" %% "scalameta" % "4.9.8").cross(CrossVersion.for3Use2_13)
  lazy val munit = "org.scalameta" %% "munit" % "1.0.0-RC1"
}
