// Common
scalaVersion := "2.11.8"
scalacOptions += "-deprecation"

// Project
organization := "org.name"
name := "Project Name"
version := "0.0.1-SNAPSHOT"

// Libraries
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6" % Test withSources(),
  "org.scalacheck" %% "scalacheck" % "1.12.5" % Test withSources())
