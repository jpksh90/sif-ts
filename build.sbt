ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % Test, "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0"
)

lazy val root = (project in file("."))
  .settings(
    name := "sif"
  )


