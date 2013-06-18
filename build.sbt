name in ThisBuild := "bug"

scalaVersion in ThisBuild := "2.10.2"

libraryDependencies in ThisBuild <+= scalaVersion { sv =>
  "org.scala-lang" % "scala-reflect" % sv
}

retrieveManaged in ThisBuild := true

scalacOptions in ThisBuild ++= Seq("-deprecation", "-unchecked", "-feature")
