import sbt._

object Dependencies {
  lazy val parsercombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"
  lazy val scalameta = ("org.scalameta" %% "scalameta" % "4.9.0").cross(CrossVersion.for3Use2_13)
  lazy val munit = "org.scalameta" %% "munit" % "0.7.29"
}
