import Dependencies._
import CommandMAin._

ThisBuild / scalaVersion := "3.9.0-RC4"

val scala2Opts = Seq("-feature", "-language:implicitConversions", "-deprecation", "-Ytasty-reader")
val scala3Opts = Seq("-feature", "-language:implicitConversions", "-indent", "-Xwiki-syntax", "-Xmax-inlines", "128", "-new-syntax")

// val scala2Opts = Seq("-feature", "-language:implicitConversions", "-explaintypes", "-deprecation", "-Ytasty-reader")
// val scala3Opts = Seq("-feature", "-language:implicitConversions", "-explain-types", "-indent", "-new-syntax")

lazy val root = (project in file("."))
  .aggregate(`ce-main`)
  .settings(
    name := "MobileAmbients[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC4",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC4"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    commands += main,
    libraryDependencies ++= Seq(scalameta, parsercombinators, munit % Test)
  )

lazy val `ce-main` = (project in file("ce/main"))
  .settings(
    name := "ce MobileAmbients[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC4",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC4"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(catseffect, catsstm, munit % Test)
  )

unmanagedSources / excludeFilter := "ce*/*.scala" || "[fz]s*/*.scala" || "examples/*.scala"

// ThisBuild / evictionErrorLevel := Level.Info

Global / bloopExportJarClassifiers := Some(Set("sources"))
Global / onChangedBuildSource := ReloadOnSourceChanges
//Global / onChangedBuildSource := IgnoreSourceChanges

Test / parallelExecution := true
