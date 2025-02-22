import sbt._

object Dependencies {
  lazy val catseffect = "org.typelevel" %% "cats-effect" % "3.6-28f8f29"
  lazy val parsercombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
  lazy val scalameta = ("org.scalameta" %% "scalameta" % "4.12.7").cross(CrossVersion.for3Use2_13)
  lazy val munit = "org.scalameta" %% "munit" % "1.1.0"
}
