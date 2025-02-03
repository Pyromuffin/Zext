package Zext

import Zext.Actions.*
import Zext.Interpreter.Say
import Zext.QueryPrecedence.Location
import Zext.Relation.*
import Zext.Rule.{after, execute, inflict}
import Zext.World.{currentWorld, playerLocation, playerRoom}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

implicit object Backdropping extends Relation[Backdrop, Room | RoomRegion, ManyToMany] {
  extension [X <: Source : TT](s: SC[X])
    infix def backdrops[Y <: Target : {QC, TT}](target: SC[Y]*): X = relates(s, target)
}


implicit object RoomRegioning extends Relation[RoomRegion, Room, ManyToMany] {
  extension [X <: Source : TT](s: SC[X])
    infix def designates[Y <: Target : {QC, TT}](target: SC[Y]*): X = relates(s, target)
}


trait StartingRoom

abstract class Room extends ZextObject with Container {

  World.currentWorld.rooms.append(this)

  enterable = true
  autoexplode = false
  pluralized = Some(false)

  val here = Condition(this == playerLocation, Location )
  var visited = false

}


object everywhere extends RoomRegion("everywhere")


class Backdrop(val name : StringExpression = getClass.getName) extends ZextObject with Container

class RoomRegion(val name : StringExpression = getClass.getName) extends Relatable {
  override def toString = name

  World.currentWorld.regions.append(this)

  def rooms = relations(RoomRegioning)
  def here = Condition(rooms.contains(playerRoom), Location)

}