name := "evolve"

version := "0.1.2"

scalaVersion := "3.0.2"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers += "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4" % Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % Test
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0" % Test

coverageExcludedPackages := "<empty>;evolve.example.*;evolve.benchmark.*"

parallelExecution in Test := false