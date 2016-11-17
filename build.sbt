name := "evolve"

version := "0.1"

scalaVersion := "2.12.0"

scalacOptions ++= Seq(
  "-Yinline-warnings",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings")

// https://mvnrepository.com/artifact/org.scalacheck/scalacheck_2.12
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

// https://mvnrepository.com/artifact/org.scalatest/scalatest_2.12
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"