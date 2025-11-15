import Dependencies._
import CommandPin._

ThisBuild / scalaVersion := "3.7.4-RC3"

//val akkaSecureToken = "TODO"

Global / resolvers ++= Seq("jitpack" at "https://jitpack.io",
//                           "akka-secure-mvn" at s"https://repo.akka.io/$akkaSecureToken/secure",
//                           Resolver.url("akka-secure-ivy", url(s"https://repo.akka.io/$akkaSecureToken/secure"))(Resolver.ivyStylePatterns),
                           "scala-integration" at "https://scala-ci.typesafe.com/artifactory/scala-integration/")

val scala2Opts = Seq("-feature", "-language:implicitConversions", "-deprecation", "-Ytasty-reader")
val scala3Opts = Seq("-feature", "-language:implicitConversions", "-indent", "-Xwiki-syntax", "-Xmax-inlines", "128", "-new-syntax")

// val scala2Opts = Seq("-feature", "-language:implicitConversions", "-explaintypes", "-deprecation", "-Ytasty-reader")
// val scala3Opts = Seq("-feature", "-language:implicitConversions", "-explain-types", "-indent", "-new-syntax")

lazy val root = (project in file("."))
  .aggregate(`ce-main`, `ce-main_`, `ca-main`, `ca-main_`, `akka-main`, `akka-main_`)
  .settings(
    name := "π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.7.4-RC3",
    crossScalaVersions ++= Seq("2.13.17", "3.7.4-RC3"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    commands += pin,
    libraryDependencies ++= Seq(scalameta, parsercombinators, munit % Test)
  )

lazy val `ce-main` = (project in file("ce/main"))
  .settings(
    name := "ce π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.7.4-RC3",
    crossScalaVersions ++= Seq("2.13.17", "3.7.4-RC3"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(catseffect, munit % Test)
  )

lazy val `ce-main_` = (project in file("ce/main_"))
  .settings(
    name := "ce_ π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.7.4-RC3",
    crossScalaVersions ++= Seq("2.13.17", "3.7.4-RC3"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(catseffect, munit % Test)
  )

lazy val `ca-main` = (project in file("ca/main"))
  .settings(
    name := "ca π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.7.4-RC3",
    crossScalaVersions ++= Seq("2.13.17", "3.7.4-RC3"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(catsactors, munit % Test)
  )

lazy val `ca-main_` = (project in file("ca/main_"))
  .settings(
    name := "ca_ π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.7.4-RC3",
    crossScalaVersions ++= Seq("2.13.17", "3.7.4-RC3"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(catsactors, munit % Test)
  )

lazy val `akka-main` = (project in file("akka/main"))
  .settings(
    name := "akka π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.7.4-RC3",
    crossScalaVersions ++= Seq("2.13.17", "3.7.4-RC3"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(akka, munit % Test)
  )

lazy val `akka-main_` = (project in file("akka/main_"))
  .settings(
    name := "akka_ π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.7.4-RC3",
    crossScalaVersions ++= Seq("2.13.17", "3.7.4-RC3"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(akka, munit % Test)
  )

unmanagedSources / excludeFilter := "c[ea]/pi*.scala" || "*[ae][kk][ao]/pi*.scala" || "examples/*.scala"

// ThisBuild / evictionErrorLevel := Level.Info

Global / bloopExportJarClassifiers := Some(Set("sources"))
Global / onChangedBuildSource := ReloadOnSourceChanges
//Global / onChangedBuildSource := IgnoreSourceChanges

Test / parallelExecution := true
