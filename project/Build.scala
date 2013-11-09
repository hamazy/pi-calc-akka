import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "",
    version := "1.0-SNAPSHOT",
    scalaVersion := "2.10.2",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8")
  )
}

object Dependencies {
  val sprayCan = "io.spray" % "spray-can" % "1.2-RC2"
  val sprayRouting = "io.spray" % "spray-routing" % "1.2-RC2"
  val sprayTest = "io.spray" % "spray-testkit" % "1.2-RC2" % "test"
  val akkaActor = "com.typesafe.akka" %% "akka-actor" % "2.2.3"
  val akkaSlf4j = "com.typesafe.akka"   %%  "akka-slf4j" % "2.2.3"
  val akkaTestkit = "com.typesafe.akka" %% "akka-testkit" % "2.2.3" % "test"
  val scalaLogging = "com.typesafe" %% "scalalogging-slf4j" % "1.0.1"
  val logback = "ch.qos.logback" % "logback-classic" % "1.0.13"
  val scalatest = "org.scalatest" %% "scalatest" % "2.0.M8" % "test"
  val pegdown = "org.pegdown" % "pegdown" % "1.0.1" % "test"
  val mockito = "org.mockito" % "mockito-core" % "1.9.5" % "test"

  val commonDeps = Seq(
    sprayCan,
    sprayRouting,
    sprayTest,
    akkaActor,
    akkaSlf4j,
    akkaTestkit,
    scalaLogging,
    logback,
    scalatest,
    pegdown,
    mockito
  )
}

object Resolvers {
  val sprayRepo = "spray repo" at "http://repo.spray.io/"

  val resolvers = Seq(sprayRepo)
}

object PiCalcAkkaBuild extends Build {

  import BuildSettings.buildSettings
  import Dependencies.commonDeps
  import spray.revolver.RevolverPlugin.Revolver
  import org.scalastyle.sbt.ScalastylePlugin.{Settings => ScalastyleSettings}
  import sbtassembly.Plugin.AssemblyKeys
  import sbtassembly.Plugin.assemblySettings
  import ScctPlugin.instrumentSettings

  lazy val root = Project("pi-calc-akka",
    file("."),
    settings = buildSettings ++
      instrumentSettings ++
      ScalastyleSettings ++
      assemblySettings ++
      Revolver.settings) settings (
    libraryDependencies ++= commonDeps,
        resolvers := Resolvers.resolvers,
        mainClass in AssemblyKeys.assembly := Some("com.suguruhamazaki.Boot"),
        testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/report", "-u", "target/junitxml", "-o"))
}
