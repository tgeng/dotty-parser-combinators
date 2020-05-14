val dottyVersion = "0.24.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-parser-combinators",
    version := "0.2.1",
    organization := "io.github.tgeng",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
    ),

    scalacOptions += "-Yexplicit-nulls",
    scalacOptions += "-Ykind-projector",
    scalacOptions += "-Ycheck-init",
    scalacOptions += "-language:postfixOps"
  )
