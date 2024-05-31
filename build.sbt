val scala3Version = "3.4.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "yatzy",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.18" % Test
    )
  )
