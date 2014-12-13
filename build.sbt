name := "jsonz"

libraryDependencies :=
  Seq(
    "org.scalaz"    %% "scalaz-core"   % "7.1.0",
    "org.json4s"    %% "json4s-native" % "3.2.11",
    "org.scalatest" %% "scalatest"     % "2.2.1" % "test")

scalacOptions := Seq("-feature")
