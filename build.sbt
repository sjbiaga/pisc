import Dependencies._
import CommandMAin._

ThisBuild / scalaVersion := "3.10.2-RC1"

val scala2Opts = Seq("-feature", "-language:implicitConversions", "-deprecation", "-Ytasty-reader")
val scala3Opts = Seq("-feature", "-language:implicitConversions", "-indent", "-Xwiki-syntax", "-Xmax-inlines", "128", "-new-syntax")

// val scala2Opts = Seq("-feature", "-language:implicitConversions", "-explaintypes", "-deprecation", "-Ytasty-reader")
// val scala3Opts = Seq("-feature", "-language:implicitConversions", "-explain-types", "-indent", "-new-syntax")

lazy val root = (project in file("ma"))
  .aggregate(`ce-main`, `zio-main`)
  .settings(
    name := "MobileAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.2-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.2-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    commands += main,
    libraryDependencies ++= Seq(scalameta, parsercombinators, munit % Test)
  )

lazy val `ce-main` = (project in file("ce/main"))
  .settings(
    name := "ce MobileAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.2-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.2-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(catseffect, catsstm, munit % Test)
  )

lazy val `zio-main` = (project in file("zio/main"))
  .settings(
    name := "zio MobileAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.2-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.2-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(zio, munit % Test)
  )

// ThisBuild / evictionErrorLevel := Level.Info

Global / bloopExportJarClassifiers := Some(Set("sources"))
Global / onChangedBuildSource := ReloadOnSourceChanges
//Global / onChangedBuildSource := IgnoreSourceChanges

Test / parallelExecution := true
