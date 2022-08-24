package Zext

import Game.JunimoGame

import scala.collection.mutable.ArrayBuffer
import Zext.*
import Zext.Actions.*
import Zext.Rule.*


object World extends Container {

  var playerName = "Farmer"
  var inventory = new Container {}
  var location: Room = null
  var time = 10

  object StartingGame extends Action // for hooking.

  def Init(): Unit = {
    Macros.TouchEveryone(Actions)
    Macros.TouchEveryone(JunimoGame)

    location = Game.FarmHouse

    ZextObject.all.foreach{ z =>
      Parser.Understand(z, z.name)
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
