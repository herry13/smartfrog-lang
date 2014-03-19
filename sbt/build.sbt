import AssemblyKeys._

seq(assemblySettings: _*)

name := "sfParser"

version := "0.1"

scalaVersion := "2.10.3"

libraryDependencies += "org.scala-lang" % "scala-library" % "2.10.3"

traceLevel in run := 0

fork in run := true

scalacOptions += "-optimize"

mainClass := Some("org.sf.lang.Parser")
