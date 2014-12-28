name := "jsonv"

libraryDependencies :=
  Seq(
    "org.scalaz"    %% "scalaz-core"   % "7.1.0",
    "org.json4s"    %% "json4s-native" % "3.2.11",
    "org.scalatest" %% "scalatest"     % "2.2.1" % Test)

scalacOptions := Seq("-feature", "-deprecation")

scalaVersion := "2.11.4"
