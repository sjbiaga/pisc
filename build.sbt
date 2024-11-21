import Dependencies._

ThisBuild / scalaVersion := "3.6.2-RC1"

Global / resolvers += "scala-integration" at "https://scala-ci.typesafe.com/artifactory/scala-integration/"

val scala2Opts = Seq("-feature", "-language:implicitConversions", "-deprecation", "-Ytasty-reader")
val scala3Opts = Seq("-feature", "-language:implicitConversions", "-indent", "-Xwiki-syntax", "-Xmax-inlines", "128", "-new-syntax")

// val scala2Opts = Seq("-feature", "-language:implicitConversions", "-explaintypes", "-deprecation", "-Ytasty-reader")
// val scala3Opts = Seq("-feature", "-language:implicitConversions", "-explain-types", "-indent", "-new-syntax")

lazy val root = (project in file("."))
  .aggregate(main, main_)
  .settings(
    name := "Polyadic π-Calculus[async]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.6.2-RC1",
    crossScalaVersions ++= Seq("2.13.15", "3.6.2-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(scalameta, parsercombinators, munit % Test)
  )

lazy val main = (project in file("main"))
  .settings(
    name := "main Polyadic π-Calculus[async]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.6.2-RC1",
    crossScalaVersions ++= Seq("2.13.15", "3.6.2-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(catseffect, scalameta, munit % Test)
  )

lazy val main_ = (project in file("main_"))
  .settings(
    name := "main_ Polyadic π-Calculus[async]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.6.2-RC1",
    crossScalaVersions ++= Seq("2.13.15", "3.6.2-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(catseffect, scalameta, munit % Test)
  )

unmanagedSources / excludeFilter := "ppi*.scala" || "examples/*.scala"

// ThisBuild / evictionErrorLevel := Level.Info

Global / bloopExportJarClassifiers := Some(Set("sources"))
Global / onChangedBuildSource := ReloadOnSourceChanges
//Global / onChangedBuildSource := IgnoreSourceChanges

Test / parallelExecution := false
