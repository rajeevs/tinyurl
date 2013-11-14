name := "tinyurl"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache
)     

val appDependencies = Seq(
  jdbc,
  "postgresql" % "postgresql" % "9.1-903.jdbc4"
)

play.Project.playScalaSettings
