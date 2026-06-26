ThisBuild / scalaVersion := "3.6.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "finally-tagless-scala",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
  )