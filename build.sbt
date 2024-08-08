import org.typelevel.sbt.tpolecat.*
import Dependencies.*
import sbt.librarymanagement.Configurations.IntegrationTest

ThisBuild / organization := "net.constructive"
ThisBuild / scalaVersion := "3.4.0"

addCommandAlias("fmt", ";scalafmt ;test:scalafmt ;scalafmtSbt")
addCommandAlias("showDep", ";dependencyBrowseGraph")

lazy val root = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "surreals",
    Defaults.itSettings,
    libraryDependencies ++= Seq(
      Libraries.Algebra,
      Libraries.Droste,
      Libraries.DrosteMacros,
      Libraries.DisciplineMUnit % Test,
      Libraries.ScalacheckCats % Test,
      Libraries.AlgebraLaws % Test
    )
  )
