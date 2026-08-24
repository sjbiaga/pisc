import Dependencies._
import CommandSPin._

ThisBuild / scalaVersion := "3.9.0-RC6"

val akkaSecureToken = "cAzJkaebGFNkNrv2ILttVDQWmf3u4ThOcE_EbfzM0-N8lDhx"

Global / resolvers ++= Seq("akka-secure-mvn" at s"https://repo.akka.io/$akkaSecureToken/secure",
                           Resolver.url("akka-secure-ivy", url(s"https://repo.akka.io/$akkaSecureToken/secure"))(Resolver.ivyStylePatterns),
                           "confluent" at "https://packages.confluent.io/maven")

val scala2Opts = Seq("-feature", "-language:implicitConversions", "-deprecation", "-Ytasty-reader")
val scala3Opts = Seq("-feature", "-language:implicitConversions", "-indent", "-Xwiki-syntax", "-Xmax-inlines", "128", "-new-syntax")

// val scala2Opts = Seq("-feature", "-language:implicitConversions", "-explaintypes", "-deprecation", "-Ytasty-reader")
// val scala3Opts = Seq("-feature", "-language:implicitConversions", "-explain-types", "-indent", "-new-syntax")

lazy val root = (project in file("."))
  .aggregate(`ce-main`, `ce-main_`, `cef-main`, `cef-main_`, `zio-main`, `zio-main_`, `ziof-main`, `ziof-main_`, `akka-main`, `akka-main_`, `pekko-main`, `pekko-main_`, `fs2-main`, `fs2-main_`, `zs-main`, `zs-main_`)
  .settings(
    name := "Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    commands += spin,
    libraryDependencies ++= Seq(scalameta, parsercombinators, munit % Test)
  )

lazy val traces = (project in file("traces"))
  .settings(
    name := "traces Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(munit % Test)
  )

lazy val traces_ = (project in file("traces_"))
  .settings(
    name := "traces_ Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(kafka, avro, avroʹ, rabbitmq, amazonsqs, munit % Test)
  )

lazy val `ce-main` = (project in file("ce/main"))
  .dependsOn(traces)
  .settings(
    name := "ce Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, munit % Test) ++ http4s.tail.tail
  )

lazy val `ce-main_` = (project in file("ce/main_"))
  .dependsOn(traces_)
  .settings(
    name := "ce_ Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, circe, munit % Test) ++ http4s
  )

lazy val `cef-main` = (project in file("cef/main"))
  .dependsOn(traces)
  .settings(
    name := "cef Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, munit % Test) ++ http4s.tail.tail
  )

lazy val `cef-main_` = (project in file("cef/main_"))
  .dependsOn(traces_)
  .settings(
    name := "cef_ Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, circe, munit % Test) ++ http4s
  )

lazy val `zio-main` = (project in file("zio/main"))
  .dependsOn(traces)
  .settings(
    name := "zio Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zh, zic, munit % Test)
  )

lazy val `zio-main_` = (project in file("zio/main_"))
  .dependsOn(traces_)
  .settings(
    name := "zio_ Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zh, zic, munit % Test)
  )

lazy val `ziof-main` = (project in file("ziof/main"))
  .dependsOn(traces)
  .settings(
    name := "ziof Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zh, zic, munit % Test)
  )

lazy val `ziof-main_` = (project in file("ziof/main_"))
  .dependsOn(traces_)
  .settings(
    name := "ziof_ Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zh, zic, munit % Test)
  )

lazy val `akka-main` = (project in file("akka/main"))
  .dependsOn(traces)
  .settings(
    name := "akka Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, akka, munit % Test)
  )

lazy val `akka-main_` = (project in file("akka/main_"))
  .dependsOn(traces_)
  .settings(
    name := "akka_ Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, akka, munit % Test)
  )

lazy val `pekko-main` = (project in file("pekko/main"))
  .dependsOn(traces)
  .settings(
    name := "pekko Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, pekko, munit % Test)
  )

lazy val `pekko-main_` = (project in file("pekko/main_"))
  .dependsOn(traces_)
  .settings(
    name := "pekko_ Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, pekko, munit % Test)
  )

lazy val `fs2-main` = (project in file("fs2/main"))
  .dependsOn(traces)
  .settings(
    name := "fs2 Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, fs2, munit % Test)
  )

lazy val `fs2-main_` = (project in file("fs2/main_"))
  .dependsOn(traces_)
  .settings(
    name := "fs2_ Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, fs2, munit % Test)
  )

lazy val `zs-main` = (project in file("zs/main"))
  .dependsOn(traces)
  .settings(
    name := "zs Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zs, zic, munit % Test)
  )

lazy val `zs-main_` = (project in file("zs/main_"))
  .dependsOn(traces_)
  .settings(
    name := "zs_ Stochastic π-Calculus2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.9.0-RC6",
    crossScalaVersions ++= Seq("2.13.18", "3.9.0-RC6"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zs, zic, munit % Test)
  )

unmanagedSources / excludeFilter := "ce/*.scala" || "*[ae]kk[ao]/*.scala" || "[fz]s*/*.scala" || "examples/*.scala"

// ThisBuild / evictionErrorLevel := Level.Info

Global / bloopExportJarClassifiers := Some(Set("sources"))
Global / onChangedBuildSource := ReloadOnSourceChanges
//Global / onChangedBuildSource := IgnoreSourceChanges

Test / parallelExecution := true
