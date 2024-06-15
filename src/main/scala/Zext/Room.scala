package Zext

import Zext.Actions.*
import Zext.Direction.opposing
import Zext.Interpreter.Say
import Zext.Rule.{after, inflict, execute}
import Zext.World.{currentLocation, noun}

import scala.collection.mutable

enum Direction extends ZextObject {
  case north, east, south, west
  val names = Array("north", "east", "south", "west")

  override val name: String = names(ordinal)
  override val description: StringExpression = ""
  proper = true
}




object Direction{

  north.aliases += "n"
  south.aliases += "s"
  east.aliases += "e"
  west.aliases += "w"

  World.currentWorld.globals.addAll(Seq(north, east, south, west))

  def opposing(direction: Direction) = {
    var ret : Direction = north
    if (direction == north) ret = south
    if (direction == west) ret = east;
    if (direction == east) ret = west;
    if (direction == south) ret = north;

    ret
  }
}


abstract class Room extends ZextObject with Container {

  World.currentWorld.rooms.append(this)

  val here = this
  var visited = false
  val connections = mutable.HashMap[Direction, Room]()
  def asDestination = Condition.fromObject(this)

  def OnEnter(): Unit = {
    visited = true
  }

  def Connect(direction: Direction, destination : Room) = {
    this.connections(direction) = destination
    destination.connections(opposing(direction)) = this
  }

}
