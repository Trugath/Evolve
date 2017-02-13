name := "evolve"

version := "0.1.1"

scalaVersion := "2.12.1"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings")

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

// https://mvnrepository.com/artifact/org.scalacheck/scalacheck_2.12
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

// https://mvnrepository.com/artifact/org.scalatest/scalatest_2.12
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2" % "test"

coverageExcludedPackages := "<empty>;evolve.example.*"

parallelExecution in Test := false

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")