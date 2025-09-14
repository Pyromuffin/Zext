package Zext

import Zext.Actions.*
import Zext.Interpreter.Say
import Zext.QueryPrecedence.Location
import Zext.Relation.*
import Zext.Relations.Direction
import Zext.Rule.{after, inflict}
import Zext.World.currentWorld

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

implicit object Backdropping extends Relation[Backdrop, Room | RoomRegion] with ManyToMany {
  extension [X <: Source](s: X)
    infix def backdrops[Y <: Target](target: Y*): X = relates(s, target)
}


implicit object RoomRegioning extends Relation[RoomRegion, Room] with ManyToMany {
  extension [X <: Source](s: X)
    infix def designates[Y <: Target](target: Y*): X = relates(s, target)
}



abstract class Room extends ZextObject with Container {

  World.currentWorld.rooms.append(this)

  enterable = true
  autoexplode = false
  pluralized = Some(false)

  val here = Condition(this == player.location, Location )
  var visited = false


  def disconnect(dir : Direction) : Unit = {
    removeRelation(dir.relation)
  }

}


object everywhere extends RoomRegion("everywhere")


class Backdrop(val name : StringExpression = getClass.getName) extends ZextObject with Container {
  val description = ""
}

class RoomRegion(val name : StringExpression = getClass.getName) extends ZextObject {
  override val description = "room region: " + name

  World.currentWorld.regions.append(this)

  def rooms = relations(RoomRegioning)
  def here = Condition(rooms.contains(player.room), Location)

}