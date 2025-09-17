package Zext

import Relation.*
import Zext.Actions.{UnderstandAlias, does, going}
import Zext.Idea.innate
import Zext.Relations.Direction.*
import Zext.Rule.inflict

import scala.annotation.targetName
import scala.collection.mutable.ArrayBuffer
import scala.reflect.TypeTest

object Relations {

  implicit object Composition extends Relation[Thing, Thing] with ManyToOne {

    override val precedence = QueryPrecedence.Content

    extension [X <: Thing](s: X) {
      def isComposite = s.queryRelated(Composition).nonEmpty
      def compositeObject = s.queryRelated(Composition).get
    }

    extension [X <: Source](s: X)
      infix def makes[Y <: Target](target: Y): X = {
        relates(s, target)
      }


    extension [X <: Target](s: X)
      infix def made_from[Y <: Source](target: Y*): X = {
        reverseRelates(s, target)
      }
  }


  implicit object Containment extends Relation[ZContainer, Thing] with OneToMany{

    override val precedence = QueryPrecedence.Content

    extension [X <: Source](s: X) {
      infix def holds[Y <: Target](target: Y*) = {
        relates(s, target)
      }

    }

    extension [X <: Target](s: X)
      infix def inside[Y <: Source](target: Y): X = reverseRelates(s, target)

  }

  // @todo see if we can someday fix this requiring call by name for direction 
  abstract class DirectionalRelation(direction : => Direction) extends ReciprocalRelation[Room, Room] with OneToOne {
    override val precedence = QueryPrecedence.Location
    override def getReciprocal = direction.opposite
  }




  object Direction {
    val directions = ArrayBuffer[Direction]()

    import RoomAdjacency.*

    val east = Direction("east", east_going, west_going)
    val west = Direction("west", west_going, east_going)
    val south = Direction("south", south_going, north_going)
    val north = Direction("north", north_going, south_going)
    val in = Direction("in", in_going, out_going)
    val out = Direction("out", out_going, in_going)
    val up = Direction("up", up_going, down_going)
    val down = Direction("down", down_going, up_going)

  }

  case class Direction(override val name: StringExpression, relation : DirectionalRelation, opposite : DirectionalRelation) extends Idea(name) {
    Direction.directions.addOne(this)
    UnderstandAlias(name.toString, going, this)
    this is (innate, unlisted)
  }



  implicit object RoomAdjacency extends ConditionalRelation[Room, Room] with ManyToMany {
    // the adjacency relation defines how rooms are connected
    override val precedence = QueryPrecedence.Location
    
    override def condition(source: Room, target: Room): Boolean = {
      for (direction <- Direction.directions) {
        val adjacent = source.queryRelated(direction.relation)
        if (adjacent.contains(target)) return true
      }
      false
    }


    extension (room: Room) {
      def connections(direction: Direction) : Option[Room] = {
        room.queryRelated(direction.relation)
      }
    }


    implicit object east_going extends DirectionalRelation(east) {
      extension [X <: Source](s: X)
        infix def eastward[Y <: Target](target: Y): X = reciprocates(s, target)
    }

    implicit object west_going extends DirectionalRelation(west)  {
      extension [X <: Source](s: X)
        infix def westward[Y <: Target](target: Y): X = reciprocates(s, target)
    }

    implicit object north_going extends DirectionalRelation(north) {
      extension [X <: Source](s: X)
        infix def northward[Y <: Target](target: Y): X = reciprocates(s,target)
    }

    implicit object south_going extends DirectionalRelation(south)  {
      extension [X <: Source](s: X)
        infix def southward[Y <: Target](target: Y): X = reciprocates(s, target)
    }

    implicit object in_going extends DirectionalRelation(in)  {
      extension [X <: Source](s: X)
        infix def inward[Y <: Target](target: Y): X = reciprocates(s, target)
    }

    implicit object out_going extends DirectionalRelation(out)  {
      extension [X <: Source](s: X)
        infix def outward[Y <: Target](target: Y): X = reciprocates(s, target)
    }

    implicit object up_going extends DirectionalRelation(up)  {
      extension [X <: Source](s: X)
        infix def upward[Y <: Target](target: Y): X = reciprocates(s, target)
    }

    implicit object down_going extends DirectionalRelation(down)  {
      extension [X <: Source](s: X)
        infix def downward[Y <: Target](target: Y): X = reciprocates(s, target)
    }
  }
}