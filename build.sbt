name := "scala-myco"
version := "0.1"
scalaVersion := "2.12.8"
organization := "com.jgeof"

lazy val mycorrhiza = (project in file("."))
    .settings(
        name := "Mycorrhiza",
        libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
        libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
        libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
        libraryDependencies  ++= Seq(
            // Last stable release
            "org.scalanlp" %% "breeze" % "0.13.2",

            // Native libraries are not included by default. add this if you want them (as of 0.7)
            // Native libraries greatly improve performance, but increase jar sizes.
            // It also packages various blas implementations, which have licenses that may or may not
            // be compatible with the Apache License. No GPL code, as best I know.
            "org.scalanlp" %% "breeze-natives" % "0.13.2",

            // The visualization library is distributed separately as well.
            // It depends on LGPL code
            "org.scalanlp" %% "breeze-viz" % "0.13.2"
        ),
        resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
    )