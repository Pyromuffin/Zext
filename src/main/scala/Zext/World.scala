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

    execute(StartingGame)

    println("-------------")

    location = Game.Path
  }

  var noun: ZextObject = null
  var secondNoun: ZextObject = null

  object is {
    override def toString: String = noun.be
  }

}
