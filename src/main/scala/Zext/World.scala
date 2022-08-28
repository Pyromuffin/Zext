package Zext

import Game.{Everything, FarmHouse, JunimoGame}

import scala.collection.mutable.ArrayBuffer
import Zext.*
import Zext.Actions.*
import Zext.Rule.*

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

  var noun: ZextObject = null
  var secondNoun: ZextObject = null


  object is {
    override def toString: String = noun.be
  }

}
