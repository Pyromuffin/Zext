package Zext


import Game.JunimoGame.*

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


  var potatos = 100

  object murdering_can extends Thing {
    override val name = "murdering can"
    override val description = "not very nice"
  }

}


class Player extends ZextObject with Container {

  val name = "player"
  properties += scenery

  // describe clothes?
  val description = s"You are $farmer, heir of Grandpa, steward of $farm. Your readiness is currently $rigidity." + GetRigidity()

  automaticallyListContents = false
  open = false
  transparent = false

  // parentContainer = nowhere
  // nowhere.contents += this

  var playerName = "Farmer"

  def Move(room: Room): Unit = {
    transferTo(room)
  }

  this.aliases.addOne("self").addOne("me").addOne(playerName).addOne(s"$farmer")
}

class WorldState{

  val globals = ArrayBuffer[ZextObject]()
  val rooms = ArrayBuffer[Room]()
  //var allObjects : ArrayBuffer[ZextObject] = null
  var player = new Player
  var time = 0
}


object World  {

  var currentWorld = new WorldState
  def player = currentWorld.player
  def currentLocation = player.parentContainer.asInstanceOf[Room]


  def TouchPackage(path: String): Unit = {

    val cl = ClassLoader.getSystemClassLoader
    val resource = cl.getResource(path)
    val dir = new java.io.File(resource.getFile)
    val files = dir.listFiles()
    val classNames = files.filter(f => f.getName.endsWith("$SecretHolder.class")).map(f => f.getName.stripSuffix(".class"))

    classNames.foreach { cn =>

      val c = Class.forName(s"$path.$cn")
      val f = c.getMethod("Reveal")
      f.invoke(null)
    }
  }

  object StartingGame extends Action(0) // for hooking.
  object EndingDay extends Action(0)

  def Init(): Unit = {

    TouchPackage("Zext")
    TouchPackage("Game")

    // currentWorld.player.Move(Game.FarmHouse)
    player.parentContainer = Game.FarmHouse
    Game.FarmHouse.contents.addOne(player)

   // currentWorld.globals.addOne(Game.FarmHouse.butt)
   // currentWorld.globals.addOne(Game.Porch.parsnip)

    //val p = Game.Porch.parsnip.asInstanceOf[Thing]
    //p.properties.addOne(wet)

    ExecuteAction(StartingGame)

    println("-------------")

  }

  private var _noun : ZextObject = null
  private var _secondNoun : ZextObject = null

  def SetNoun(z: ZextObject) : Unit = {
    _noun = z
  }
  def SetSecondNoun(z: ZextObject) : Unit = {
    _secondNoun = z
  }

  def noun : ZextObject = _noun
  def noun[T](using TypeTest[Property, T]) : T = {
    val maybe = _noun.get[T]
    if(maybe.isDefined) maybe.get
    else _noun.asInstanceOf[T]
  }

  def secondNoun: ZextObject = _secondNoun
  def secondNoun[T](using TypeTest[Property, T]): T = {
    val maybe = _secondNoun.get[T]
    if (maybe.isDefined) maybe.get
    else _secondNoun.asInstanceOf[T]
  }

  object is {
    override def toString: String = noun.be
  }

}
