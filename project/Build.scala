import sbt._
import Keys._

object HelloBuild extends Build {
	val appDependencies = Seq(
		"jdbc",
		"postgresql" % "postgresql" % "9.1-901-1.jdbc4"
	      )

}
