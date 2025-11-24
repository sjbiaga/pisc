import Dependencies._
import CommandSPin._

ThisBuild / scalaVersion := "3.7.4"

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
  .aggregate(`ce-main`, `ce-main_`, `cef-main`, `cef-main_`, `akka-main`, `akka-main_`, `pekko-main`, `pekko-main_`)
  .settings(
    name := "Stochastic π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.7.4",
    crossScalaVersions ++= Seq("2.13.18", "3.7.4"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    commands += spin,
    libraryDependencies ++= Seq(scalameta, parsercombinators, munit % Test)
  )

lazy val `ce-main` = (project in file("ce/main"))
  .settings(
    name := "ce Stochastic π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.7.4",
    crossScalaVersions ++= Seq("2.13.18", "3.7.4"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, munit % Test)
  )

lazy val `ce-main_` = (project in file("ce/main_"))
  .settings(
    name := "ce_ Stochastic π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.7.4",
    crossScalaVersions ++= Seq("2.13.18", "3.7.4"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, munit % Test)
  )

lazy val `cef-main` = (project in file("cef/main"))
  .settings(
    name := "cef Stochastic π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.7.4",
    crossScalaVersions ++= Seq("2.13.18", "3.7.4"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, munit % Test)
  )

lazy val `cef-main_` = (project in file("cef/main_"))
  .settings(
    name := "cef_ Stochastic π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.7.4",
    crossScalaVersions ++= Seq("2.13.18", "3.7.4"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, munit % Test)
  )

lazy val `akka-main` = (project in file("akka/main"))
  .settings(
    name := "akka Stochastic π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.7.4",
    crossScalaVersions ++= Seq("2.13.18", "3.7.4"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, akka, munit % Test)
  )

lazy val `akka-main_` = (project in file("akka/main_"))
  .settings(
    name := "akka_ Stochastic π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.7.4",
    crossScalaVersions ++= Seq("2.13.18", "3.7.4"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, akka, munit % Test)
  )

lazy val `pekko-main` = (project in file("pekko/main"))
  .settings(
    name := "pekko Stochastic π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.7.4",
    crossScalaVersions ++= Seq("2.13.18", "3.7.4"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, pekko, munit % Test)
  )

lazy val `pekko-main_` = (project in file("pekko/main_"))
  .settings(
    name := "pekko_ Stochastic π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.7.4",
    crossScalaVersions ++= Seq("2.13.18", "3.7.4"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, pekko, munit % Test)
  )

unmanagedSources / excludeFilter := "ce/*.scala" || "*[ae]kk[ao]/*.scala" || "examples/*.scala"

// ThisBuild / evictionErrorLevel := Level.Info

Global / bloopExportJarClassifiers := Some(Set("sources"))
Global / onChangedBuildSource := ReloadOnSourceChanges
//Global / onChangedBuildSource := IgnoreSourceChanges

Test / parallelExecution := true
