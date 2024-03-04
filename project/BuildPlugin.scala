import sbt._, Keys._
import sbt.plugins.{JvmPlugin, SbtPlugin}

object BuildPlugin extends AutoPlugin {
  override def trigger = allRequirements

  override def requires = JvmPlugin

  override lazy val projectSettings = baseSettings

  def baseSettings: Seq[sbt.Def.Setting[_]] =
    Seq(
      organization         := "com.nrinaudo",
      organizationHomepage := Some(url("https://nrinaudo.github.io")),
      organizationName     := "Nicolas Rinaudo",
      startYear            := Some(2022),
      scalaVersion         := "3.3.1",
      scalacOptions       ++= Seq("-source", "future", "-Ykind-projector:underscores"),
      licenses             := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.html")),
      developers := List(
        Developer("nrinaudo", "Nicolas Rinaudo", "nicolas@nrinaudo.com", url("https://twitter.com/nicolasrinaudo"))
      ),
      scmInfo := Some(
        ScmInfo(
          url(s"https://github.com/nrinaudo/playground"),
          s"scm:git:git@github.com:nrinaudo/playground.git"
        )
      )
    )
}
