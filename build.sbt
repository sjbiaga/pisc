import Dependencies._
import CommandPin._

ThisBuild / scalaVersion := "3.8.0-RC5"

val akkaSecureToken = "cAzJkaebGFNkNrv2ILttVDQWmf3u4ThOcE_EbfzM0-N8lDhx"

Global / resolvers ++= Seq("akka-secure-mvn" at s"https://repo.akka.io/$akkaSecureToken/secure",
                           Resolver.url("akka-secure-ivy", url(s"https://repo.akka.io/$akkaSecureToken/secure"))(Resolver.ivyStylePatterns))

val scala2Opts = Seq("-feature", "-language:implicitConversions", "-deprecation", "-Ytasty-reader")
val scala3Opts = Seq("-feature", "-language:implicitConversions", "-indent", "-Xwiki-syntax", "-Xmax-inlines", "128", "-new-syntax")

// val scala2Opts = Seq("-feature", "-language:implicitConversions", "-explaintypes", "-deprecation", "-Ytasty-reader")
// val scala3Opts = Seq("-feature", "-language:implicitConversions", "-explain-types", "-indent", "-new-syntax")

lazy val root = (project in file("."))
  .aggregate(`ce-main`, `ce-main_`, `akka-main`, `akka-main_`, `pekko-main`, `pekko-main_`, `fs2-main`, `fs2-main_`, `monix-main`, `monix-main_`, `zs-main`, `zs-main_`)
  .settings(
    name := "π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.8.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.8.0-RC5"),
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
    scalaVersion := "3.8.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.8.0-RC5"),
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
    scalaVersion := "3.8.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.8.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(catseffect, munit % Test)
  )

lazy val `akka-main` = (project in file("akka/main"))
  .settings(
    name := "akka π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.8.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.8.0-RC5"),
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
    scalaVersion := "3.8.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.8.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(akka, munit % Test)
  )

lazy val `pekko-main` = (project in file("pekko/main"))
  .settings(
    name := "pekko π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.8.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.8.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(pekko, munit % Test)
  )

lazy val `pekko-main_` = (project in file("pekko/main_"))
  .settings(
    name := "pekko_ π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.8.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.8.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(pekko, munit % Test)
  )

lazy val `fs2-main` = (project in file("fs2/main"))
  .settings(
    name := "fs2 π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.8.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.8.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(fs2, munit % Test)
  )

lazy val `fs2-main_` = (project in file("fs2/main_"))
  .settings(
    name := "fs2_ π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.8.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.8.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(fs2, munit % Test)
  )

lazy val `monix-main` = (project in file("monix/main"))
  .settings(
    name := "monix π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.8.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.8.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(monix, munit % Test)
  )

lazy val `monix-main_` = (project in file("monix/main_"))
  .settings(
    name := "monix_ π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.8.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.8.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(monix, munit % Test)
  )

lazy val `zs-main` = (project in file("zs/main"))
  .settings(
    name := "zs π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.8.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.8.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(zc, zs, munit % Test)
  )

lazy val `zs-main_` = (project in file("zs/main_"))
  .settings(
    name := "zs_ π-Calculus[experimental]2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.8.0-RC5",
    crossScalaVersions ++= Seq("2.13.18", "3.8.0-RC5"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(zc, zs, munit % Test)
  )

unmanagedSources / excludeFilter := "ce/pi*.scala" || "*[ae]kk[ao]/pi*.scala" || "[fz]s*/pi*.scala" || "monix/pi*.scala" || "examples/*.scala"

// ThisBuild / evictionErrorLevel := Level.Info

Global / bloopExportJarClassifiers := Some(Set("sources"))
Global / onChangedBuildSource := ReloadOnSourceChanges
//Global / onChangedBuildSource := IgnoreSourceChanges

Test / parallelExecution := true
