import Dependencies._
import CommandBAin._

ThisBuild / scalaVersion := "3.10.0-RC1"

Global / resolvers += "confluent" at "https://packages.confluent.io/maven"

val scala2Opts = Seq("-feature", "-language:implicitConversions", "-deprecation", "-Ytasty-reader")
val scala3Opts = Seq("-feature", "-language:implicitConversions", "-indent", "-Xwiki-syntax", "-Xmax-fuel:1024", "-Xmax-inlines:128", "-new-syntax")

// val scala2Opts = Seq("-feature", "-language:implicitConversions", "-explaintypes", "-deprecation", "-Ytasty-reader")
// val scala3Opts = Seq("-feature", "-language:implicitConversions", "-explain-types", "-indent", "-new-syntax")

lazy val root = (project in file("."))
  .aggregate(feedback, `ce-main`, `ce-main_`, `cef-main`, `cef-main_`, `zio-main`, `zio-main_`, `ziof-main`, `ziof-main_`, `fs2-main`, `fs2-main_`, `zs-main`, `zs-main_`)
  .settings(
    name := "BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    commands += bain,
    libraryDependencies ++= Seq(scalameta, parsercombinators, ip4s, munit % Test)
  )

val feedback = (project in file("feedback"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "feedback BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.0-RC1",
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq("com.github.japgolly.scalajs-react" %%% "core-bundle-cats_effect",
                                "com.github.japgolly.scalajs-react" %%% "extra",
                                "com.github.japgolly.scalajs-react" %%% "extra-ext-monocle3"
                            ).map(_ % "4.0.0")
                        ++  Seq("io.circe" %%% "circe-generic" % "0.14.16",
                                "io.circe" %%% "circe-parser" % "0.14.16",
                                "org.http4s" %%% "http4s-dom" % "0.2.12",
                                "org.http4s" %%% "http4s-circe" % "0.23.36")
  )

lazy val traces = (project in file("traces"))
  .settings(
    name := "traces BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(munit % Test)
  )

lazy val traces_ = (project in file("traces_"))
  .settings(
    name := "traces_ BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(kafka, avro, avroʹ, rabbitmq, amazonsqs, munit % Test)
  )

lazy val `ce-main` = (project in file("ce/main"))
  .dependsOn(traces)
  .settings(
    name := "ce BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, catsstm, munit % Test) ++ http4s.tail.tail
  )

lazy val `ce-main_` = (project in file("ce/main_"))
  .dependsOn(traces_)
  .settings(
    name := "ce BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, catsstm, circe, munit % Test) ++ http4s
  )

lazy val `cef-main` = (project in file("cef/main"))
  .dependsOn(traces)
  .settings(
    name := "cef BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, catsstm, munit % Test) ++ http4s.tail.tail
  )

lazy val `cef-main_` = (project in file("cef/main_"))
  .dependsOn(traces_)
  .settings(
    name := "cef BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, catseffect, catsstm, circe, munit % Test) ++ http4s
  )

lazy val `zio-main` = (project in file("zio/main"))
  .dependsOn(traces)
  .settings(
    name := "zio BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zh, zic, munit % Test)
  )

lazy val `zio-main_` = (project in file("zio/main_"))
  .dependsOn(traces_)
  .settings(
    name := "zio BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zh, zic, munit % Test)
  )

lazy val `ziof-main` = (project in file("ziof/main"))
  .dependsOn(traces)
  .settings(
    name := "ziof BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zh, zic, munit % Test)
  )

lazy val `ziof-main_` = (project in file("ziof/main_"))
  .dependsOn(traces_)
  .settings(
    name := "ziof BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zh, zic, munit % Test)
  )

lazy val `fs2-main` = (project in file("fs2/main"))
  .dependsOn(traces)
  .settings(
    name := "fs2 BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, fs2, catsstm, munit % Test)
  )

lazy val `fs2-main_` = (project in file("fs2/main_"))
  .dependsOn(traces_)
  .settings(
    name := "fs2_ BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, fs2, catsstm, munit % Test)
  )

lazy val `zs-main` = (project in file("zs/main"))
  .dependsOn(traces)
  .settings(
    name := "zs BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zs, zic, munit % Test)
  )

lazy val `zs-main_` = (project in file("zs/main_"))
  .dependsOn(traces_)
  .settings(
    name := "zs_ BioAmbients2Scala",
    organization := "sjb.ia.ga",
    organizationName := "sjbiaga",
    version := "1.0",
    maxErrors := 5,
    scalaVersion := "3.10.0-RC1",
    crossScalaVersions ++= Seq("2.13.18", "3.10.0-RC1"),
    scalacOptions ++= scala3Opts, // :+ "-Xprint:typer",
    libraryDependencies ++= Seq(breeze, scaffeine, zc, zs, zic, munit % Test)
  )

unmanagedSources / excludeFilter := "ce*/*.scala" || "[fz]s*/*.scala" || "examples/*.scala"

// ThisBuild / evictionErrorLevel := Level.Info

Global / bloopExportJarClassifiers := Some(Set("sources"))
Global / onChangedBuildSource := ReloadOnSourceChanges
//Global / onChangedBuildSource := IgnoreSourceChanges

Test / parallelExecution := true
