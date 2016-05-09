name := "json-formlets"

organization := "gov.wicourts"

version := "0.2.5"

scalaVersion := "2.11.7"

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.5" % "compile",
  "io.argonaut" %% "argonaut" % "6.1"
)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.6.5" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "3.6.5" % "test",
  "org.specs2" %% "specs2-scalacheck" % "3.6.5" % "test",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.1.5" % "test"
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

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.6.0")
