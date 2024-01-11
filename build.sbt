import Dependencies._

ThisBuild / scalaVersion := "3.3.1"

Global / resolvers += "scala-integration" at "https://scala-ci.typesafe.com/artifactory/scala-integration/"

val scala2Opts = Seq("-feature", "-language:implicitConversions", "-deprecation", "-Ytasty-reader")
val scala3Opts = Seq("-feature", "-language:implicitConversions", "-indent", "-Xwiki-syntax", "-Xmax-inlines", "128", "-new-syntax")

// val scala2Opts = Seq("-feature", "-language:implicitConversions", "-explaintypes", "-deprecation", "-Ytasty-reader")
// val scala3Opts = Seq("-feature", "-language:implicitConversions", "-explain-types", "-indent", "-new-syntax")

lazy val root = (project in file("."))
  .aggregate(parser, generator)
  .dependsOn(parser, generator)
  .settings(
    name := "pisc",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.3.1",
    crossScalaVersions ++= Seq("2.13.12", "3.3.1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(munit % Test)
  )

lazy val parser = (project in file("parser"))
  .settings(
    name := "pisc.parser",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.3.1",
    crossScalaVersions ++= Seq("2.13.12", "3.3.1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(parsercombinators, munit % Test)
  )

lazy val generator = (project in file("generator"))
  .dependsOn(parser)
  .settings(
    name := "pisc.generator",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "2.13.12",
    crossScalaVersions ++= Seq("2.13.12", "3.3.1"),
    scalacOptions ++= scala2Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(scalameta, munit % Test)
  )

unmanagedSources / excludeFilter := "spi.scala" || "loop.scala" || "stats.scala" || "examples/*.scala"

// ThisBuild / evictionErrorLevel := Level.Info

Global / bloopExportJarClassifiers := Some(Set("sources"))
Global / onChangedBuildSource := ReloadOnSourceChanges
//Global / onChangedBuildSource := IgnoreSourceChanges
