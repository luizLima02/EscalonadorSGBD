val scala3Version = "3.3.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "transacao",
    version := "0.1.0-SNAPSHOT",
    logLevel := Level.Error,
    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
