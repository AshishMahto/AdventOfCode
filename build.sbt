name := "AdventOfCode"

version := "1.0"

scalaVersion := "2.13.1"
  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.3.10",
    "com.lihaoyi" %% "fansi" % "0.4.0",
    "com.lihaoyi" %% "fastparse" % "3.0.0",
)

lazy val Advent3 = project.in(file("Advent3")).settings(
  name := "Advent3",
  scalaVersion := "3.2.1",
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "requests" % "0.8.0",
    "com.lihaoyi" %% "fansi" % "0.4.0",
  ),
  scalacOptions ++= Seq(
  ),
)

Global / onChangedBuildSource := ReloadOnSourceChanges
