name := "tinyurl"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "postgresql" % "postgresql" % "9.1-901-1.jdbc4"
)     

val appDependencies = Seq(
  jdbc,
  "postgresql" % "postgresql" % "9.1-901-1.jdbc4"
)

play.Project.playScalaSettings
