name := "mahjong"
version := "0.1.0"
scalaVersion := "2.12.10"

logLevel := Level.Info

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.12.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.typesafe" % "config" % "1.3.1",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "ch.qos.logback" % "logback-classic" % "1.1.7",
  "org.mockito" % "mockito-all" % "2.0.2-beta",
  "org.json4s" % "json4s-native_2.12" % "3.5.2"

)