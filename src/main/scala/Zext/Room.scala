package Zext

import Zext.Actions.*
import Zext.Direction.opposing
import Zext.Interpreter.Say
import Zext.Rule.{after, carryOut, execute}
import Zext.World.{location, noun}

import scala.collection.mutable

enum Direction extends ZextObject {
  case north, east, south, west

  proper = true
}




object Direction{

  north.name = "north"
  north.aliases += "n"
  south.name = "south"
  south.aliases += "s"
  east.name = "east"
  east.aliases += "e"
  west.name = "west"
  west.aliases += "w"

  def opposing(direction: Direction) = {
    var ret : Direction = north
    if (direction == north) ret = south
    if (direction == west) ret = east;
    if (direction == east) ret = west;
    if (direction == south) ret = north;

    ret
  }
}



class Room extends ZextObject with Container {

  given Room = this
  World.currentRoom = this

  var visited = false
  val connections = mutable.HashMap[Direction, Room]()


  def OnEnter(): Unit = {
    visited = true
  }

  def connect(direction: Direction)(implicit room : Room) = {
    room.connections(direction) = this
    this.connections(opposing(direction)) = room
  }



}