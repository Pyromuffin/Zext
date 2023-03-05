package Zext


import Game.JunimoGame.*

import scala.collection.mutable.ArrayBuffer
import Zext.*
import Zext.Actions.*
import Zext.Parser.BuildUnderstandables
import Zext.Rule.*

import scala.reflect.TypeTest

object reflexively extends ZextObject {
  name = "reflexively"
  global = true
}

object nowhere extends Room {
  global = true
  name = "nowhere"
  proper = true
}


object player extends ZextObject with Container {  //there are two me's to disambiguate?

  name = "player"
  properties += scenery

  // describe clothes?
  description = s"You are $farmer, heir of Grandpa, steward of $farm. Your readiness is currently $rigidity." + GetRigidity()

  automaticallyListContents = false
  open = false
  transparent = false

  parentContainer = nowhere
  nowhere.contents += this

  var playerName = "Farmer"
  var lucreHeld = 0

  def Move(room: Room): Unit ={
    transferTo(room)
  }

  this.aliases.addOne("self").addOne("me").addOne(playerName).addOne(s"$farmer")
}




object World extends Container {

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


  def currentLocation = player.parentContainer.asInstanceOf[Room]
  var time = 10

  object StartingGame extends Action(0) // for hooking.
  object EndingDay extends Action(0)



  def Init(): Unit = {

    TouchPackage("Zext")
    TouchPackage("Game")

    player.Move(Game.FarmHouse)

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
