val commonSettings = Seq(
  scalaVersion := "2.13.5"
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.5",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % "test"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )
