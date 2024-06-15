
name := "Zext"
version := "0.1"
scalaVersion := "3.2.2"

val org = "org.pyromuffin"

lazy val zext = (project in file("."))
  .settings(
    name := "Zext",

    version := "0.1.0",
    autoCompilerPlugins := true,
    addCompilerPlugin("org.pyromuffin" %% "zobjectifier" % "1.0.4"),

    libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.3.0",
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.14.0",
  )

Compile / scalacOptions ++= Seq("-Xplugin-require:Zobjectifier")


/*
enablePlugins(ScalaNativePlugin)

import scala.scalanative.build._
nativeConfig ~= {
  _.withLTO(LTO.thin)
    .withMode(Mode.releaseFast)
    .withGC(GC.commix)
}
*/
