name := "evolve"

version := "0.1"

scalaVersion := "2.12.0-M4"

crossScalaVersions := Seq("2.11.8", "2.12.0-M4")

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings")

resolvers ++= Seq(
  Resolver.sonatypeRepo("public")
)

libraryDependencies += "org.scalacheck" % "scalacheck_2.12.0-M4" % "1.12.5" % "test"

libraryDependencies += "org.scalatest" % "scalatest_2.12.0-M4" % "2.2.6" % "test"