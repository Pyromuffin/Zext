package Zext

import Zext.Interpreter.Say
import scala.collection.mutable.ArrayBuffer

object World extends Container{

  var playerName = "Zebra"
  var inventory = ArrayBuffer[ZextObject]()
  var location : Room = null
  var time = 10
  var currentRoom : Room = null

   def Init(): Unit =
   {
      Macros.TouchEveryone(Actions)
      Macros.TouchEveryone(Rooms)
      currentRoom = null

      println("-------------")

      location = Rooms.bathroom
   }

  var noun : ZextObject = null
  var secondNoun : ZextObject = null

  object is {
    override def toString: String = noun.be
  }

}
