name := "text_classification"

version := "1.0"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.7.2" % "test"
)

scalacOptions += "-deprecation"

seq(ProguardPlugin.proguardSettings :_*)

proguardOptions += keepMain("Enricher")

proguardOptions ++= Seq(
  keepMain("Enricher"),
  keepAllScala
)
