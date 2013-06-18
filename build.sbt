name in ThisBuild := "bug"

scalaVersion in ThisBuild := "2.10.2"

libraryDependencies in ThisBuild <+= scalaVersion { sv =>
  "org.scala-lang" % "scala-reflect" % sv
}

// resolvers in ThisBuild += "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/"
//
// libraryDependencies in ThisBuild ++= Seq(
//   "play" %% "play-json" % "2.2-SNAPSHOT",
//   "org.scalatest" %% "scalatest" % "1.9.1" % "test"
// )

retrieveManaged in ThisBuild := true

scalacOptions in ThisBuild ++= Seq("-deprecation", "-unchecked", "-feature")
