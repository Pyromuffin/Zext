name := "Zext"

version := "0.1"

scalaVersion := "3.1.3"

libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.1.1"


enablePlugins(ScalaNativePlugin)

import scala.scalanative.build._
nativeConfig ~= {
  _.withLTO(LTO.thin)
    .withMode(Mode.releaseFast)
    .withGC(GC.commix)
}
