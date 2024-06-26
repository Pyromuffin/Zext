package Zext

import scala.collection.mutable.ArrayBuffer
import Zext.*
import Zext.Actions.*
import Zext.Parser.BuildUnderstandables
import Zext.Rule.*

import scala.ref.WeakReference
import scala.reflect.TypeTest

object reflexively extends ZextObject {
  val name = "reflexively"
  val description = ""
}

object nowhere extends Room {
  val name = "nowhere"
  val description = ""
  proper = true

}


abstract class Player extends ZextObject with Container {

  properties += scenery
  automaticallyListContents = false
  open = false
  transparent = false

  def Move(room: Room): Unit = {
    transferTo(room)
  }

  this.aliases.addOne("self").addOne("me")
}

class WorldState{

  val globals = ArrayBuffer[ZextObject]()
  val rooms = ArrayBuffer[Room]()
  var player : Player = null

  var time = 0
}


object World  {

  var currentWorld = new WorldState
  def player = currentWorld.player
  def currentLocation = player.parentContainer.asInstanceOf[Room]


  def RevealSecrets(path: String, className : String): Unit = {
    val c = Class.forName(s"$path.$className")
    val f = c.getMethod("Reveal")
    f.invoke(null)
  }

  def TouchPackage(path: String): Unit = {

    val cl = ClassLoader.getSystemClassLoader
    val resource = cl.getResource(path)
    val dir = new java.io.File(resource.getFile)
    val files = dir.listFiles()
    val classNames = files.filter(f => f.getName.endsWith("$SecretHolder.class")).map(f => f.getName.stripSuffix(".class"))

    classNames.foreach { cn =>
      RevealSecrets(path, cn)
    }
  }

  object StartingGame extends Action(0) // for hooking.
  object EndingDay extends Action(0)

  val secretNames = Array(
  "Actions$SecretHolder",
  "Inflector$SecretHolder",
  "Interpreter$SecretHolder",
  "Macros$SecretHolder",
  "Person$SecretHolder",
  "Room$SecretHolder",
  "Rule$SecretHolder",
  "Saving$SecretHolder",
  "World$SecretHolder",
  "Container$SecretHolder",
  "Zext$SecretHolder",
  )



  def Init(gamesPlayer : Player, gamePackageName : String): Unit = {

    // i think it will be hard or impossible to switch players, but maybe that's ok.
    currentWorld.player = gamesPlayer

    for(name <- secretNames){
      RevealSecrets("Zext", name)
    }

    TouchPackage(gamePackageName)

    val startingRoom = currentWorld.rooms.find(_.isInstanceOf[StartingRoom]).get
    startingRoom.visited = true
    gamesPlayer.parentContainer = startingRoom
    startingRoom.contents.addOne(gamesPlayer)



    ExecuteAction(StartingGame)

    println("-------------")

  }

  


}
