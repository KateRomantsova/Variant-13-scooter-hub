ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "scooter-hub",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
  )
