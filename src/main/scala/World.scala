package Zext

import Zext.Interpreter.Say
import Zext.*
import scala.collection.mutable.ArrayBuffer

given container : Container = World

object World extends Container{

  var playerName = "Zebra"
  var inventory = ArrayBuffer[ZextObject]()
  var location : Room = null
  var time = 10


  object takingInventory extends Action("inventory", "i"){

    override def executeNone() = {
      var s = "I am holding "
      for(i <- inventory){
        s += i.indefinite + ", "
      }
      s = s.stripSuffix(", ")
      s += "."
      Say(s)
      true
    }
  }

  var noun : ZextObject = null

  object is {
    override def toString: String = noun.be
  }

}