import sbt._

object Dependencies {
  lazy val catseffect = "org.typelevel" %% "cats-effect" % "3.6-0142603"
  lazy val parsercombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
  lazy val scalameta = ("org.scalameta" %% "scalameta" % "4.11.2").cross(CrossVersion.for3Use2_13)
  lazy val munit = "org.scalameta" %% "munit" % "1.0.2"
}
