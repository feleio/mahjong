ThisBuild / scalaVersion := "2.12.20"
ThisBuild / version      := "0.1.0"
ThisBuild / organization := "io.fele"

lazy val core = (project in file("."))
  .settings(
    name := "mahjong",
    logLevel := Level.Info,
    libraryDependencies ++= Seq(
      "org.scala-lang"             %  "scala-reflect"      % "2.12.0",
      "org.scalatest"              %% "scalatest"          % "3.0.1"  % "test",
      "com.typesafe"               %  "config"             % "1.3.1",
      "com.typesafe.scala-logging" %% "scala-logging"      % "3.5.0",
      "ch.qos.logback"             %  "logback-classic"    % "1.1.7",
      "org.mockito"                %  "mockito-all"        % "2.0.2-beta",
      "org.json4s"                 %  "json4s-native_2.12" % "3.5.2",
      "com.microsoft.onnxruntime"  %  "onnxruntime"        % "1.17.1"
    ),
    // Old mockito needs reflective access that JDK 17+ blocks by default
    Test / fork := true,
    Test / javaOptions ++= Seq(
      "--add-opens=java.base/java.lang=ALL-UNNAMED",
      "--add-opens=java.base/java.util=ALL-UNNAMED"
    ),
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", _*) => MergeStrategy.discard
      case "reference.conf"         => MergeStrategy.concat
      case _                        => MergeStrategy.first
    },
    assembly / mainClass := Some("io.fele.app.mahjong.rl.RLGymServer")
  )

val http4sVersion   = "0.23.30"
val circeVersion    = "0.14.10"
val doobieVersion   = "1.0.0-RC5"
val catsEffectV     = "3.5.7"
val fs2Version      = "3.11.0"
val log4catsVersion = "2.7.0"

lazy val server = (project in file("server"))
  .dependsOn(core)
  .settings(
    name := "mahjong-server",
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-ember-server" % http4sVersion,
      "org.http4s"      %% "http4s-dsl"          % http4sVersion,
      "org.http4s"      %% "http4s-circe"        % http4sVersion,
      "io.circe"        %% "circe-core"          % circeVersion,
      "io.circe"        %% "circe-generic"       % circeVersion,
      "io.circe"        %% "circe-parser"        % circeVersion,
      "org.typelevel"   %% "cats-effect"         % catsEffectV,
      "co.fs2"          %% "fs2-core"            % fs2Version,
      "org.tpolecat"    %% "doobie-core"         % doobieVersion,
      "org.tpolecat"    %% "doobie-postgres"     % doobieVersion,
      "org.tpolecat"    %% "doobie-hikari"       % doobieVersion,
      "org.typelevel"   %% "log4cats-slf4j"      % log4catsVersion,
      "ch.qos.logback"  %  "logback-classic"     % "1.4.14",
      "org.scalatest"   %% "scalatest"           % "3.2.18" % Test
    ),
    // Suites share one Postgres and race on CREATE TABLE IF NOT EXISTS otherwise
    Test / parallelExecution := false,
    assembly / mainClass := Some("io.fele.mahjong.server.Main"),
    // Stable jar name so Dockerfile.server doesn't track the version
    assembly / assemblyJarName := "mahjong-server.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "io.netty.versions.properties") => MergeStrategy.first
      case PathList("META-INF", _*)                             => MergeStrategy.discard
      case "module-info.class"                                  => MergeStrategy.discard
      case "reference.conf"                                     => MergeStrategy.concat
      case _                                                    => MergeStrategy.first
    }
  )
