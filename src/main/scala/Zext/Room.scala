package Zext

import Zext.Actions.*
import Zext.Direction.opposing
import Zext.Interpreter.Say
import Zext.QueryPrecedence.Location
import Zext.Rule.{after, execute, inflict}
import Zext.World.{currentLocation, currentWorld}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

enum Direction extends ZextObject {
  case north, east, south, west, up, down
  val names = Array("north", "east", "south", "west", "up", "down")

  override val name: StringExpression = names(ordinal)
  override val description: StringExpression = ""
  proper = true
}


object Direction{

  north.aliases += "n"
  south.aliases += "s"
  east.aliases += "e"
  west.aliases += "w"
  up.aliases += "u"
  down.aliases += "d"

  World.currentWorld.globals.addAll(Seq(north, east, south, west, up, down))

  def opposing(direction: Direction) = {
    var ret : Direction = north
    if (direction == north) ret = south
    if (direction == west) ret = east;
    if (direction == east) ret = west;
    if (direction == south) ret = north;
    if (direction == up) ret = down;
    if (direction == down) ret = up;

    ret
  }
}

trait StartingRoom

abstract class Room extends ZextObject with Container {

  World.currentWorld.rooms.append(this)

  val here = Condition(this == currentLocation, Location )
  var visited = false
  val connections = mutable.HashMap[Direction, Room]()
  val backdrops = mutable.HashSet[Backdrop]()

  def addBackdrop(backdrop: Backdrop) : Unit = {
    backdrops.add(backdrop)
  }

  def Connect(direction: Direction, destination : Room) = {
    this.connections(direction) = destination
    destination.connections(opposing(direction)) = this
  }

  def ConnectOneWay(direction: Direction, destination: Room) = {
    this.connections(direction) = destination
  }

  def DisconnectOneWay(direction: Direction) = {
    this.connections.remove(direction)
  }

  def Disconnect(direction: Direction) = {
    val dest = this.connections(direction)
    this.connections.remove(direction)
    dest.connections.remove(opposing(direction))
  }
}


object everywhere extends RoomRegion


class Backdrop extends ZextObject with Container {
  override val name = this.getClass.getSimpleName
  override val description = ""
}

class RoomRegion extends ZextObject {

  override val name = this.getClass.getSimpleName
  override val description = ""

  World.currentWorld.regions.append(this)

  val rooms = mutable.HashSet[Room]()
  val backdrops = mutable.HashSet[Backdrop]()

  def addBackdrop(backdrop: Backdrop) : Unit = {
    backdrops.add(backdrop)
  }

  def addRoom(room : Room) : Unit = rooms.add(room)
  def addRooms(r : Room*) : Unit = rooms.addAll(r)
}