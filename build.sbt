import Dependencies._
import CommandBAin._

ThisBuild / scalaVersion := "3.9.0-RC5"

val scala2Opts = Seq("-feature", "-language:implicitConversions", "-deprecation", "-Ytasty-reader")
val scala3Opts = Seq("-feature", "-language:implicitConversions", "-indent", "-Xwiki-syntax", "-Xmax-inlines", "128", "-new-syntax")

// val scala2Opts = Seq("-feature", "-language:implicitConversions", "-explaintypes", "-deprecation", "-Ytasty-reader")
// val scala3Opts = Seq("-feature", "-language:implicitConversions", "-explain-types", "-indent", "-new-syntax")

lazy val root = (project in file("."))
  .aggregate(`ce-main`, `ce-main_`, `cef-main`, `cef-main_`, `zio-main`, `zio-main_`, `ziof-main`, `ziof-main_`, `fs2-main`, `fs2-main_`, `zs-main`, `zs-main_`)
  .settings(
    name := "BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    commands += bain,
    libraryDependencies ++= Seq(scalameta, parsercombinators, munit % Test)
  )

lazy val `ce-main` = (project in file("ce/main"))
  .settings(
    name := "ce BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, catsstm, munit % Test)
  )

lazy val `ce-main_` = (project in file("ce/main_"))
  .settings(
    name := "ce BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, catsstm, munit % Test)
  )

lazy val `cef-main` = (project in file("cef/main"))
  .settings(
    name := "cef BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, catsstm, munit % Test)
  )

lazy val `cef-main_` = (project in file("cef/main_"))
  .settings(
    name := "cef BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, catsstm, munit % Test)
  )

lazy val `zio-main` = (project in file("zio/main"))
  .settings(
    name := "zio BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zic, munit % Test)
  )

lazy val `zio-main_` = (project in file("zio/main_"))
  .settings(
    name := "zio BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zic, munit % Test)
  )

lazy val `ziof-main` = (project in file("ziof/main"))
  .settings(
    name := "ziof BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zic, munit % Test)
  )

lazy val `ziof-main_` = (project in file("ziof/main_"))
  .settings(
    name := "ziof BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zic, munit % Test)
  )

lazy val `fs2-main` = (project in file("fs2/main"))
  .settings(
    name := "fs2 BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, fs2, catsstm, munit % Test)
  )

lazy val `fs2-main_` = (project in file("fs2/main_"))
  .settings(
    name := "fs2_ BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, fs2, catsstm, munit % Test)
  )

lazy val `zs-main` = (project in file("zs/main"))
  .settings(
    name := "zs BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zs, zic, munit % Test)
  )

lazy val `zs-main_` = (project in file("zs/main_"))
  .settings(
    name := "zs_ BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zs, zic, munit % Test)
  )

unmanagedSources / excludeFilter := "ce*/*.scala" || "[fz]s*/*.scala" || "examples/*.scala"

// ThisBuild / evictionErrorLevel := Level.Info

Global / bloopExportJarClassifiers := Some(Set("sources"))
Global / onChangedBuildSource := ReloadOnSourceChanges
//Global / onChangedBuildSource := IgnoreSourceChanges

Test / parallelExecution := true
