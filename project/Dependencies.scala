import sbt._

object Dependencies {

  val slf4jVersion = "1.6.4"
  val slf4jNop = "org.slf4j" % "slf4j-nop" % slf4jVersion
  val circe = "0.8.0"

  val commonDependencies: Seq[ModuleID] = Seq(
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    slf4jNop,
    "junit" % "junit" % "4.12"
  )

  val coreDependencies: Seq[ModuleID] = Seq(
    "org.scalanlp" %% "breeze" % "0.13.2",
    "org.scalanlp" %% "breeze-natives" % "0.13.2",
    "org.scalanlp" %% "breeze-viz" % "0.13.2"
  )

  val apiDependencies: Seq[ModuleID] = Seq(
    "org.scalatra" %% "scalatra" % "2.5.1",
    "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided",
    "org.eclipse.jetty" % "jetty-webapp" % "9.2.15.v20160210" % "container;compile",
    "io.circe" %% "circe-core" % circe,
    "io.circe" %% "circe-parser" % circe,
    "io.circe" %% "circe-generic" % circe,
    "io.circe" %% "circe-literal" % circe
  )
}
