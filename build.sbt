name := "scala-myco"
version := "0.1"
scalaVersion := "2.12.8"
organization := "com.jgeof"

lazy val mycorrhiza = (project in file("."))
    .settings(
        name := "Mycorrhiza",
        libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
        libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
        libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"
    )