import Dependencies._

ThisBuild / scalaVersion := "3.5.2-RC2"

Global / resolvers += "scala-integration" at "https://scala-ci.typesafe.com/artifactory/scala-integration/"

val scala2Opts = Seq("-feature", "-language:implicitConversions", "-deprecation", "-Ytasty-reader")
val scala3Opts = Seq("-feature", "-language:implicitConversions", "-indent", "-Xwiki-syntax", "-Xmax-inlines", "128", "-new-syntax")

// val scala2Opts = Seq("-feature", "-language:implicitConversions", "-explaintypes", "-deprecation", "-Ytasty-reader")
// val scala3Opts = Seq("-feature", "-language:implicitConversions", "-explain-types", "-indent", "-new-syntax")

lazy val root = (project in file("."))
  .aggregate(main)
  .settings(
    name := "Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.5.2-RC2",
    crossScalaVersions ++= Seq("2.13.15", "3.5.2-RC2"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(scalameta, parsercombinators, munit % Test)
  )

lazy val main = (project in file("main"))
  .settings(
    name := "main Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.5.2-RC2",
    crossScalaVersions ++= Seq("2.13.15", "3.5.2-RC2"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, munit % Test)
  )

unmanagedSources / excludeFilter := "spi*.scala" || "loop.scala" || "stats.scala" || "examples/*.scala"

// ThisBuild / evictionErrorLevel := Level.Info

Global / bloopExportJarClassifiers := Some(Set("sources"))
Global / onChangedBuildSource := ReloadOnSourceChanges
//Global / onChangedBuildSource := IgnoreSourceChanges

Test / parallelExecution := false
