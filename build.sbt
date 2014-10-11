name := "incluse"

organization := "net.datapusher"

version := "0.2"

scalaVersion := "2.11.2"

scalacOptions += "-unchecked"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)
