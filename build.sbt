name := "evolve"

version := "0.1"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-Yinline-warnings",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

coverageExcludedPackages := "<empty>;evolve.example.*"