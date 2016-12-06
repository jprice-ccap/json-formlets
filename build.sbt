name := "json-formlets"

organization := "gov.wicourts"

version := "0.3.4-SNAPSHOT"

scalaVersion := "2.11.8"
crossScalaVersions := Seq("2.11.8", "2.12.1")

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.7" % "compile",
  "io.argonaut" %% "argonaut" % "6.2-RC2",
  "io.argonaut" %% "argonaut-scalaz" % "6.2-RC2"
)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.8.6" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "3.8.6" % "test",
  "org.specs2" %% "specs2-scalacheck" % "3.8.6" % "test",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.2.7-scalacheck-1.13" % "test"
)

// https://tpolecat.github.io/2014/04/11/scalac-flags.html

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",       // yes, this is 2 args
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",        // N.B. doesn't work well with the ??? hole
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Ywarn-unused-import",   // 2.11 only
  "-Yno-predef"   // no automatic import of Predef (removes irritating implicits)
)

//scalacOptions in Test ++= Seq("-Yrangepos")

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
