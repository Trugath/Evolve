name := "evolve"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-Yinline-warnings",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings")

libraryDependencies += "org.scalacheck" % "scalacheck_2.11" % "1.12.5" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0-M9" % "test"
