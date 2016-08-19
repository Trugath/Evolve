name := "evolve"

version := "0.1"

scalaVersion := "2.12.0-M5"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings")

resolvers ++= Seq(
  Resolver.sonatypeRepo("public")
)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

coverageExcludedPackages := "<empty>;evolve.example.*"