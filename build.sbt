name := "AdventOfCode"

version := "1.0"

scalaVersion := "2.13.16"
  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.3.10",
    "com.lihaoyi" %% "fansi" % "0.4.0",
    "com.lihaoyi" %% "fastparse" % "3.0.0",
)

val Advent3 = project.in(file("Advent3")).settings(
  name := "Advent3",
  scalaVersion := "3.3.5",
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "requests" % "0.9.0",
    "com.lihaoyi" %% "fansi" % "0.5.0"
  ),
)

Global / onChangedBuildSource := ReloadOnSourceChanges
