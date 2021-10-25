package Zext

import Zext.Interpreter.Say
import scala.collection.mutable.ArrayBuffer

given container : Container = World

object World extends Container{

  var playerName = "Zebra"
  var inventory = ArrayBuffer[ZextObject]()
  var location : Room = null
  var time = 10


   def Init(): Unit =
   {
      Macros.TouchEveryone(Actions)
      Macros.TouchEveryone(Rooms)
      location = Rooms.bathroom
   }

  var noun : ZextObject = null

  object is {
    override def toString: String = noun.be
  }

}