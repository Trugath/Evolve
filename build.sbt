name := "evolve"

version := "1.0"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-Yinline-warnings",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0-M15" % "test"
