ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "proyecto_practicum1.1",
    libraryDependencies ++= Seq(
      "com.github.tototoshi" %% "scala-csv" % "1.3.10",
      "io.github.pityka" %% "nspl-awt" % "0.10.0",
      "io.github.pityka" %% "nspl-core" % "0.10.0"
    )
  )
