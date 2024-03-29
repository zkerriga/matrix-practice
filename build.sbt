ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "matrix-practice",
    libraryDependencies ++= Seq(
      "org.scalatest"     %% "scalatest"     % "3.2.15" % Test,
      "org.apache.commons" % "commons-math3" % "3.6.1"  % Test,
    ),
  )
