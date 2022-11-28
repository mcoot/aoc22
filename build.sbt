val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc22",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies +="com.softwaremill.sttp.client3" %% "core" % "3.3.18",
    libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.6"
  )
