name := "evolve"

version := "0.1.2"

scalaVersion := "2.13.0"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings")

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers += "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"

// https://mvnrepository.com/artifact/org.scalacheck/scalacheck_2.13
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test

// https://mvnrepository.com/artifact/org.scalatest/scalatest_2.13
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

coverageExcludedPackages := "<empty>;evolve.example.*;evolve.benchmark.*"

parallelExecution in Test := false