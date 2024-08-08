import sbt.*

object Dependencies {

  object Versions {

    // cats
    val Cats       = "2.9.0"

    val SCaffeine = "5.3.0"

    // droste (recursion schemes)
    val Droste = "0.9.0"

    // testing framework
    val DisciplineMUnit = "2.0.0"

    val ScalacheckCats = "0.3.2"

  }

  object Libraries {

    def cats(artifact: String): ModuleID = "org.typelevel" %% artifact % Versions.Cats
    def droste(artifact: String): ModuleID = "io.higherkindness" %% artifact % Versions.Droste

    val CatsCore = cats("cats-core")
    val Algebra = cats("algebra")
    val AlgebraLaws = cats("algebra-laws")

    val Droste = droste("droste-core")
    val DrosteMacros = droste("droste-macros")

    val SCaffeine = "com.github.blemale" %% "scaffeine" % Versions.SCaffeine

    val DisciplineMUnit = "org.typelevel" %% "discipline-munit" % Versions.DisciplineMUnit
    val ScalacheckCats = "io.chrisdavenport" %% "cats-scalacheck" % Versions.ScalacheckCats
  }
}
