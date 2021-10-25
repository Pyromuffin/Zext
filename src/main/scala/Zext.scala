package Zext


import Globals.*
import Query.Property
import Rule.*
import Direction.*
import Interpreter.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.{implicitConversions, postfixOps}

given container : Container = World

object World extends Container{

    val start = bedRoom
    var playerName = "Zebra"
    var inventory = ArrayBuffer[ZextObject]()
    var location : Room = null


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


}
import World._



trait Property {

}



object holdable extends Property
object fixed extends Property
object scenery extends Property
object wet extends Property
case class initialDescription(desc : StringExpression) extends Property

trait Container {
    var contents : ArrayBuffer[ZextObject] = ArrayBuffer[ZextObject]()
}

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
            override def executeNone() = {
                val other = location.connections.get(i)
                other match{
                    case Some(r) => {
                        Say(s"I went $i to $r.\n")
                        location = r
                        execute(examining)
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


case class Connection(room: Room, direction : Direction)

class Room(using container : Container) extends ZextObject with Container {
    given currentContainer : Container = this
    given room : Room = this

    val connections = mutable.HashMap[Direction, Room]()

    def connect(direction: Direction)(implicit room : Room) = {
        room.connections(direction) = this
        this.connections(opposing(direction)) = room
    }

    object going extends Action("go") {
        override def executeOne(noun: ZextObject): Boolean = {

            true
        }
    }

    after(examining, classOf[Room]) {
        val r = noun.asInstanceOf[Room]
        val visible = r.contents.filterNot(_ ? scenery)
        if(!visible.isEmpty){
            var s = "You can see "
            for(i <- visible){
                if( !i.?(scenery) )
                s += i.indefinite + ", "
            }
            s = s.stripSuffix(", ")
            s += "."
            Say(s)
        }
    }


    inline def Thing : thing = {
        thing() a Macros.variableName
    }

    extension(d: StringExpression)  {
        inline def Thing = {
            thing() a Macros.variableName desc d
        }
        inline def unary_~ : thing = {
            thing() a Macros.variableName desc d
        }
    }

}

object ZextObject {
    val nouns = ArrayBuffer[ZextObject]()
}


class ZextObject(using container : Container) {
    var definiteArticle : String = "the"
    var indefiniteArticle : String = "a"
    var name : StringExpression = ""
    var aliases = ArrayBuffer[StringExpression]()
    var description : StringExpression = ""
    var properties : ArrayBuffer[Property] = ArrayBuffer[Property]()
    var plural = false
    var proper = false

    var parentContainer : Container = container
    container.contents += this

    ZextObject.nouns += this

    def definite : String = {
        if(proper)
            return name.toString

        definiteArticle + " " + name
    }

    def indefinite : String = {
        if(proper)
            return name.toString

        indefiniteArticle + " " + name
    }

    def be : String = {
        if(plural)
            return "are"
        "is"
    }

    def ?(property: Property) : Boolean =  {
        properties.contains(property)
    }

    def get[P <: Property] = {

    }

    override def toString: String = definite
}



class thing(using container : Container) extends ZextObject{

    def desc(desc : StringExpression)  ={
        description = desc
        this
    }

    def is(prop: Property) = {
        properties += prop
        this
    }

    def are(prop: Property) = {
        properties += prop
        this
    }

    def and(prop: Property) = {
        properties += prop
        this
    }

    def a(name : StringExpression) = {
        this.name = name
        this
    }

    def the(name : StringExpression) = {
        this.name = name
        this
    }

    def some(name : StringExpression) = {
        this.name = name
        plural = true
        indefiniteArticle = "some"
        this
    }

    def named(name : StringExpression) = {
        this.name = name
        proper = true
        this
    }

    def has(prop: Property) = {
        properties += prop
        this
    }
}

//class person extends thing

class Supporter(using container: Container) extends thing with Container

class Named(named : String)(using parent: Container) extends thing{
    name = named
}


object Globals {
    var noun : ZextObject = null

    object is {
        override def toString: String = Globals.noun.be
    }
}

object taking extends Action("take", "get") {


    override def executeNone() : Boolean = {
        Say(s"I can't take nothing.")
        true
    }

    override def executeOne(zextObject: ZextObject) : Boolean = {
        Say(s"I took $noun.")
        inventory += noun
        val container = noun.parentContainer.contents
        container.remove( container.indexOf(noun) )
        true
    }
}



object examining extends Action("examine", "x", "look" ) {

    override def executeNone() : Boolean = {
        noun = location
        Say(location.name)
        Say(location.description)
        true
    }

    override def executeOne(noun : ZextObject) : Boolean = {
        val immediate = s"$noun: ${noun.description}"
        Say(immediate)
        true
    }
}
