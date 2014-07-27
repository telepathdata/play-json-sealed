resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= {
  val sq0 = if (scalaVersion.value startsWith "2.10")
    ("org.scalamacros" %% "quasiquotes" % "2.0.0") :: Nil
  else
    Nil
  sq0 ++ Seq(
    "com.typesafe.play" %% "play-json" % "2.3.0"
    // "org.scalatest" %% "scalatest" % "2.2.0" % "test"
  )
}

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)