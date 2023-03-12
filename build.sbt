name := "Zext"

version := "0.1"

scalaVersion := "3.2.2"

val dottyVersion = "3.2.2"
val org = "org.pyromuffin"



// if this doesn't compile, from sbt try:
// plugin/publishLocal

lazy val plugin = project
  .settings(
    name := "objectifier",
    organization := org,
    version := "1.0.0",
    scalaVersion := dottyVersion,

    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % dottyVersion
  )

lazy val zext = (project in file("."))
  .settings(
    name := "Zext",

    version := "0.1.0",
    scalaVersion := dottyVersion,

    libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.2.0",
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.12.0",
     libraryDependencies += compilerPlugin("org.pyromuffin" %% "objectifier" % "1.0.0")
  )


lazy val root = project
  .aggregate(plugin, zext)

/*
enablePlugins(ScalaNativePlugin)

import scala.scalanative.build._
nativeConfig ~= {
  _.withLTO(LTO.thin)
    .withMode(Mode.releaseFast)
    .withGC(GC.commix)
}
*/
