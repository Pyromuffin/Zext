package Zext

import Zext.Direction.opposing
import Zext.Interpreter.Say
import Zext.Rule.{after, execute}
import Zext.World.{location, noun}
import Actions.*

import scala.collection.mutable

enum Direction:
  case north, east, south, west

object Direction{

  def opposing(direction: Direction) = {
    var ret : Direction = north
    if (direction == north) ret = south
    if (direction == west) ret = east;
    if (direction == east) ret = west;
    if (direction == south) ret = north;

    ret
  }

  for(i <- Direction.values) {
    val str : String = i.toString
    val action = new Action(str, str.charAt(0).toString) {
      Rule.carryOut(this) {
        val other = location.connections.get(i)
        other match{
          case Some(r) => {
            Say(s"I went $i to $r.\n")
            location = r
            execute(examining, location)
          }
          case _ => {
            Say(s"I can't go $str.")
          }
        }
        true
      }
    }
  }
}



class Room extends ZextObject with Container {

  given Room = this
  World.currentRoom = this

  val connections = mutable.HashMap[Direction, Room]()

  def connect(direction: Direction)(implicit room : Room) = {
    room.connections(direction) = this
    this.connections(opposing(direction)) = room
  }



}