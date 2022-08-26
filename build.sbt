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

/*
lazy val prepareAppTask = TaskKey[Unit]("prepare-app")
prepareAppTask := {
  val file = sourceDirectory.value / "main"/ "scala" / "Game" / "Touchers.scala"
  IO.append(file, """// Zebra\n""")
}

val file = sourceDirectory.value / "main"/ "scala" / "Game" / "Touchers.scala"

(compile in Compile) := ((compile in Compile) dependsOn prepareAppTask).value
(compileIncremental in Compile) := ((compileIncremental in Compile) dependsOn prepareAppTask).value
*/
/*
Compile / sourceGenerators += Def.task {
  val file = sourceDirectory.value / "main"/ "scala" / "Game" / "Touchers.scala"
  IO.append(file, """// Zebra\n""")
  Seq(file)
}.taskValue

Compile / sourceGenerators += Def.task {
  val file = (Compile / sourceManaged).value / "demo" / "Test.scala"
  IO.write(file, """object Test extends App { println("Hi") }""")
  Seq(file)
}.taskValue

lazy val configTest = settingKey[File]("example")

configTest := sourceDirectory.value / "main"/ "scala" / "Game" / "Touchers.scala"
*/