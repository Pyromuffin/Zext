name := "Zext"

version := "0.1"

scalaVersion := "3.1.3"

libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.1.1"
import sbt.Attributed.data


lazy val generate1 = taskKey[Unit]("Code generation")
generate1 := {
  println("1")
  val touchersFile = (Compile / sourceDirectory).value / "scala" / "Game" / "Touchers.scala"
  val generatedFile = (Compile / sourceDirectory).value / "scala" / "Game" / "Generated.scala"

  println("2")
  IO.write(generatedFile, "package Game; object allObjects")

  println("3")
  val nanos = java.time.LocalTime.now().toNanoOfDay
  val f = new java.io.RandomAccessFile(touchersFile, "rw")
  f.writeBytes("//" + nanos.toString)
  f.close()
}


lazy val touchTouchers = taskKey[Unit]("Code generation")
touchTouchers := {
  println("69")

  val touchersFile = (Compile / sourceDirectory).value / "scala" / "Game" / "Touchers.scala"
  val nanos = java.time.LocalTime.now().toNanoOfDay
  val f = new java.io.RandomAccessFile(touchersFile, "rw")
  f.writeBytes("//" + nanos.toString)
  f.close()
}



lazy val generate3 = taskKey[Unit]("Code generation")
generate3 := {
  println("4")
  val generatedFile = (Compile / sourceDirectory).value / "scala" / "Game" / "Generated.scala"

  val r = (Compile / runner).value
  val cp = (Compile / fullClasspath).value
  r.run("Game.CodeGen", data(cp), Seq(generatedFile.toString), streams.value.log)
}

//generate3 := generate3.dependsOn(generate1).value

lazy val generate4 = taskKey[Unit]("Code generation")
generate4 := {
  val touchersFile = (Compile / sourceDirectory).value / "scala" / "Game" / "Touchers.scala"
  val f = new java.io.RandomAccessFile(touchersFile, "rw")
  f.writeBytes("/////////////////") // to not screw up version control. man this i stupid.
  f.close()
}
generate4 := generate4.dependsOn(Compile / compile).value

enablePlugins(ScalaNativePlugin)

import scala.scalanative.build._
nativeConfig ~= {
  _.withLTO(LTO.thin)
    .withMode(Mode.releaseFast)
    .withGC(GC.commix)
}

