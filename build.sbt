name := "scala-myco"
version := "0.1"
scalaVersion := "2.12.8"
organization := "com.jgeof"

lazy val mycorrhiza = (project in file("."))
    .settings(
        name := "Mycorrhiza",
        libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    )