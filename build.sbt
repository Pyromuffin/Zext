name := "Zext"

version := "0.1"

scalaVersion := "3.1.3"

val dottyVersion = "3.1.3"
val org = "org.pyromuffin"




lazy val plugin = project
  .settings(
    name := "objectifier",
    organization := org,
    version := "1.0.0",
    scalaVersion := dottyVersion,

    libraryDependencies += "org.scala-lang" %% "scala3-compiler" % dottyVersion % "provided"
  )

lazy val zext = (project in file("."))
  .settings(
    name := "Zext",

    version := "0.1.0",
    scalaVersion := dottyVersion,

    libraryDependencies += compilerPlugin("org.pyromuffin" %% "objectifier" % "1.0.0"),
    libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.1.1",

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
