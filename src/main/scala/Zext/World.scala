package Zext

import Game.{FarmHouse, JunimoGame}

import scala.collection.mutable.ArrayBuffer
import Zext.*
import Zext.Actions.*
import Zext.Rule.*


object World extends Container {

  var playerName = "Farmer"
  var inventory = new Container {}
  var currentLocation: Room = null
  var time = 10

  object StartingGame extends Action // for hooking.

  def Init(): Unit = {

    Game.Touchers

    currentLocation = Game.FarmHouse

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
