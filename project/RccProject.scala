import sbt._
import sbt.Keys._

object RccProject extends AutoPlugin {

  // lazy val scala212 = "2.12.15"
  // lazy val scala213 = "2.13.7"
  lazy val scala3 = "3.1.0"
  // lazy val scalaVersions = Seq(scala3)

  override def requires = plugins.JvmPlugin

  override def trigger = allRequirements

  override val projectSettings = Seq(
    organization := "com.raasahsan",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3,
    libraryDependencies ++= Seq(
      Dependencies.munit % Test
    )
  )

  // lazy val crossCompilationSettings = Seq(
  //   crossScalaVersions := scalaVersions
  // )

}
