import Dependencies._
import CommandBAin._

ThisBuild / scalaVersion := "3.9.0-RC1"

val akkaSecureToken = "cAzJkaebGFNkNrv2ILttVDQWmf3u4ThOcE_EbfzM0-N8lDhx"

Global / resolvers ++= Seq("akka-secure-mvn" at s"https://repo.akka.io/$akkaSecureToken/secure",
                           Resolver.url("akka-secure-ivy", url(s"https://repo.akka.io/$akkaSecureToken/secure"))(Resolver.ivyStylePatterns))

val scala2Opts = Seq("-feature", "-language:implicitConversions", "-deprecation", "-Ytasty-reader")
val scala3Opts = Seq("-feature", "-language:implicitConversions", "-indent", "-Xwiki-syntax", "-Xmax-inlines", "128", "-new-syntax")

// val scala2Opts = Seq("-feature", "-language:implicitConversions", "-explaintypes", "-deprecation", "-Ytasty-reader")
// val scala3Opts = Seq("-feature", "-language:implicitConversions", "-explain-types", "-indent", "-new-syntax")

lazy val root = (project in file("."))
  .aggregate(`ce-main`, `ce-main_`, `cef-main`, `cef-main_`, `fs2-main`, `fs2-main_`, `zs-main`, `zs-main_`)
  .settings(
    name := "BioAmbients[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    commands += bain,
    libraryDependencies ++= Seq(scalameta, parsercombinators, munit % Test)
  )

lazy val `ce-main` = (project in file("ce/main"))
  .settings(
    name := "ce BioAmbients[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, catsstm, munit % Test)
  )

lazy val `ce-main_` = (project in file("ce/main_"))
  .settings(
    name := "ce BioAmbients[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, catsstm, munit % Test)
  )

lazy val `cef-main` = (project in file("cef/main"))
  .settings(
    name := "cef BioAmbients[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, catsstm, munit % Test)
  )

lazy val `cef-main_` = (project in file("cef/main_"))
  .settings(
    name := "cef BioAmbients[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, catsstm, munit % Test)
  )

lazy val `fs2-main` = (project in file("fs2/main"))
  .settings(
    name := "fs2 BioAmbients π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, fs2, catsstm, munit % Test)
  )

lazy val `fs2-main_` = (project in file("fs2/main_"))
  .settings(
    name := "fs2_ BioAmbients π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, fs2, catsstm, munit % Test)
  )

lazy val `zs-main` = (project in file("zs/main"))
  .settings(
    name := "zs BioAmbients π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zs, zic, munit % Test)
  )

lazy val `zs-main_` = (project in file("zs/main_"))
  .settings(
    name := "zs_ BioAmbients π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zs, zic, munit % Test)
  )

unmanagedSources / excludeFilter := "ce*/*.scala" || "[fz]s*/*.scala" || "examples/*.scala"

// ThisBuild / evictionErrorLevel := Level.Info

Global / bloopExportJarClassifiers := Some(Set("sources"))
Global / onChangedBuildSource := ReloadOnSourceChanges
//Global / onChangedBuildSource := IgnoreSourceChanges

Test / parallelExecution := true
