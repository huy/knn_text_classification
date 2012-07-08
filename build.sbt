name := "text_classification"

version := "1.0"

scalaVersion := "2.9.1"

resolvers += "coddhale repo" at "http://repo.codahale.com"

libraryDependencies ++= Seq(
  "com.codahale" %% "logula" % "2.1.3",
  "org.scalatest" %% "scalatest" % "1.7.2" % "test"
)

scalacOptions += "-deprecation"

seq(ProguardPlugin.proguardSettings :_*)

proguardOptions += keepMain("Enricher")

proguardOptions ++= Seq(
  keepMain("Enricher"),
  keepAllScala
)
