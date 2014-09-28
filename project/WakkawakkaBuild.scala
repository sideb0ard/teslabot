import sbt._
import sbt.Keys._

object WakkawakkaBuild extends Build {

  lazy val wakkawakka = Project(
    id = "wakkawakka",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "WakkaWakka",
      organization := "com.theb0ardside",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.11.2",
      libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-actor" % "2.3.6"
      )
    )
  )
}
