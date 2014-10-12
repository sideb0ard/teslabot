import sbt._
import sbt.Keys._

object TeslabotBuild extends Build {

  lazy val teslabot = Project(
    id = "teslabot",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "Teslabot",
      organization := "com.theb0ardside",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.11.2",
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-actor" % "2.3.6",
        // "org.json4s" %% "json4s-native" % "3.2.10",
        "org.json4s" %% "json4s-jackson" % "3.2.10"
      )
    )
  )
}
