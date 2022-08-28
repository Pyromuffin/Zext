package Zext

import Game.{Everything, FarmHouse, JunimoGame}

import scala.collection.mutable.ArrayBuffer
import Zext.*
import Zext.Actions.*
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


object player extends ZextObject with Container {

  name = "player"
  properties += scenery

  parentContainer = nowhere
  nowhere.contents += this

  var playerName = "Farmer"

  def Move(room: Room): Unit ={
    transferTo(room)
  }

}




object World extends Container {

  Macros.TouchEveryone(Actions)
  Macros.TouchEveryone(ZextObject)
  val members = Everything.members

  def currentLocation = player.parentContainer.asInstanceOf[Room]
  var time = 10

  object StartingGame extends Action(0) // for hooking.

  def Init(): Unit = {
    player.Move(Game.FarmHouse)

    ZextObject.all.foreach{ z =>
      Parser.Understand(z, Seq(z.name).concat(z.aliases)* )
    }

    execute(StartingGame)

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
