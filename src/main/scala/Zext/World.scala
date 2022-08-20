package Zext

import Game.JunimoGame

import scala.collection.mutable.ArrayBuffer
import Zext.*
import Zext.Actions.*
import Zext.Rule.*


object World extends Container {

  var playerName = "Zebra"
  var inventory = ArrayBuffer[ZextObject]()
  var location: Room = null
  var time = 10
  var currentRoom: Room = null

  object StartingGame extends Action // for hooking.

  def Init(): Unit = {
    Macros.TouchEveryone(Actions)
    Macros.TouchEveryone(Game.Rooms)
    Macros.TouchEveryone(JunimoGame)
    currentRoom = null

    execute(StartingGame)

    println("-------------")

    location = Game.Forest
  }

  var noun: ZextObject = null
  var secondNoun: ZextObject = null

  object is {
    override def toString: String = noun.be
  }

}
