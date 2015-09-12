import sbt._

object BuildDef extends Build {
  override lazy val projects = Seq(root)
  lazy val root = Project("evolve", file("."))
}