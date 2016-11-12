name := "nearest-neighbour"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze-viz" % "0.12",
  "co.theasi" %% "plotly" % "0.2.0-SNAPSHOT"
)
