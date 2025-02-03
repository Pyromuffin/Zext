package Zext

import Relation.*

import scala.collection.mutable.ArrayBuffer

object Relations {

  object Likes extends Relation[Thing, Thing, ManyToMany] {
    extension [X <: Source : TT](s: SC[X])
      infix def likes[Y <: Target : {QC, TT}](target: SC[Y]*): X = relates(s, target)
  }

  implicit object Composition extends Relation[Thing, Thing, ManyToOne] {

    override val precedence = QueryPrecedence.Content

    extension [X <: Source](s: X) {
      def isComposite = s.relations(Composition).nonEmpty
      def compositeObject = s.relations(Composition).get
    }

    extension [X <: Source : TT](s: SC[X])
      infix def makes[Y <: Target : {QC, TT}](target: SC[Y]): X = relates(s, target)

    extension [X <: Target : TT](s: SC[X])
      infix def made_from[Y <: Source : {QC, TT}](target: SC[Y]*): X = reverseRelates(s, target)
  }


  implicit object Containment extends Relation[ZContainer, Thing, OneToMany] {

    override val precedence = QueryPrecedence.Content

    extension [X <: Source : TT](s: SC[X])
      infix def contains[Y <: Target : {QC, TT}](target: SC[Y]*): X = relates(s, target)

    extension [X <: Target : TT](s: SC[X])
      infix def inside[Y <: Source : {QC, TT}](target: SC[Y]): X = reverseRelates(s, target)

  }



  case class DirectionalRelation(opposite: DirectionalRelation, name: String) extends ReciprocalRelation[Room, Room, OneToOne](opposite) {
    override val precedence = QueryPrecedence.Location
    DirectionalRelation.directions.addOne(this)
  }


  object DirectionalRelation {
    val directions = ArrayBuffer[DirectionalRelation]()
  }

  case class Direction(override val name: String, relation : Relation[Room,Room,OneToOne]) extends ZextObject {
    override val description = "just a direction"
  }

  val east = Direction("east", RoomAdjacency.east_going)
  val west = Direction("west", RoomAdjacency.west_going)
  val south = Direction("south", RoomAdjacency.south_going)
  val north = Direction("north", RoomAdjacency.north_going)
  val in = Direction("in", RoomAdjacency.in_going)
  val out = Direction("out", RoomAdjacency.out_going)
  val up = Direction("up", RoomAdjacency.up_going)
  val down = Direction("down", RoomAdjacency.down_going)


  implicit object RoomAdjacency extends ConditionalRelation[Room, Room, OneToOne] {
    // the adjacency relation defines how rooms are connected
    override val precedence = QueryPrecedence.Location

    override def condition(source: Room, target: Room): Boolean = {
      for (direction <- DirectionalRelation.directions) {
        val adjacent = source.relations(direction)
        if (adjacent.contains(target)) return true
      }
      false
    }

    extension [X <: Source](room: X) {
      def connections(direction: Direction) : Option[Room] = {
        room.relations(direction.relation)
      }
    }

    implicit object east_going extends DirectionalRelation(west_going, "east") {
      extension [X <: Source : TT](s: SC[X])
        infix def eastward[Y <: Target : {QC, TT}](target: SC[Y]): X = reciprocates(s, target)
    }

    implicit object west_going extends DirectionalRelation(east_going, "west") {
      extension [X <: Source : TT](s: SC[X])
        infix def westward[Y <: Target : {QC, TT}](target: SC[Y]): X = reciprocates(s, target)
    }

    implicit object north_going extends DirectionalRelation(south_going, "north") {
      extension [X <: Source : TT](s: SC[X])
        infix def northward[Y <: Target : {QC, TT}](target: SC[Y]): X = reciprocates(s, target)
    }

    implicit object south_going extends DirectionalRelation(north_going, "south") {
      extension [X <: Source : TT](s: SC[X])
        infix def southward[Y <: Target : {QC, TT}](target: SC[Y]): X = reciprocates(s, target)
    }

    implicit object in_going extends DirectionalRelation(out_going, "in") {
      extension [X <: Source : TT](s: SC[X])
        infix def inward[Y <: Target : {QC, TT}](target: SC[Y]): X = reciprocates(s, target)
    }

    implicit object out_going extends DirectionalRelation(in_going, "out") {
      extension [X <: Source : TT](s: SC[X])
        infix def outward[Y <: Target : {QC, TT}](target: SC[Y]): X = reciprocates(s, target)
    }

    implicit object up_going extends DirectionalRelation(down_going, "up") {
      extension [X <: Source : TT](s: SC[X])
        infix def upward[Y <: Target : {QC, TT}](target: SC[Y]): X = reciprocates(s, target)
    }

    implicit object down_going extends DirectionalRelation(up_going, "down") {
      extension [X <: Source : TT](s: SC[X])
        infix def downward[Y <: Target : {QC, TT}](target: SC[Y]): X = reciprocates(s, target)
    }
  }
}