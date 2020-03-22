val dottyVersion = "0.23.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    scalacOptions += "-Yexplicit-nulls",
    scalacOptions += "-Ykind-projector",
    scalacOptions += "-Ycheck-init"
  )
