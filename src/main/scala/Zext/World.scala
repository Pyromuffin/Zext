package Zext

import scala.collection.mutable.ArrayBuffer

import Zext.*


object World extends Container {

  var playerName = "Zebra"
  var inventory = ArrayBuffer[ZextObject]()
  var location: Room = null
  var time = 10
  var currentRoom: Room = null

  def Init(): Unit = {
    Macros.TouchEveryone(Actions)
    Macros.TouchEveryone(Game.Rooms)
    currentRoom = null

    println("-------------")

    location = Game.Rooms.bathroom
  }

  var noun: ZextObject = null
  var secondNoun: ZextObject = null

  object is {
    override def toString: String = noun.be
  }

}
