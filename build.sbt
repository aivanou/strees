name := TreeBuild.NamePrefix + "root"

version := "0.0.1"

scalaVersion := "2.12.2"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

lazy val strees = project.
  settings(Common.settings: _*).
  settings(libraryDependencies ++= Dependencies.commonDependencies).
  settings(libraryDependencies ++= Dependencies.coreDependencies)

lazy val api = project.
  dependsOn(strees).
  settings(Common.settings: _*).
  settings(libraryDependencies ++= Dependencies.apiDependencies)

lazy val root = (project in file(".")).
  aggregate(strees, api)
