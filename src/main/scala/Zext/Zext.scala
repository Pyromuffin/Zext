package Zext

import Zext.Direction.*
import Zext.Interpreter.*
import Zext.Query.Property
import Zext.Rule.*
import Zext.World.*
import Zext.thing.NounAmount

import java.lang.reflect.Constructor
import scala.collection.mutable.ArrayBuffer
import scala.language.{implicitConversions, postfixOps}


trait Property


object holdable extends Property
object fixed extends Property
object scenery extends Property
object wet extends Property
case class initialDescription(desc : StringExpression) extends Property


trait Container {
    given c : Container = this
    var contents : ArrayBuffer[ZextObject] = ArrayBuffer[ZextObject]()
    var open = true
    var transparent = true
}


object ZextObject {
    val nouns = ArrayBuffer[ZextObject]()
}

class ZextObject {

    var definiteArticle : String = "the"
    var indefiniteArticle : String = "a"
    var name : String = ""
    var aliases = ArrayBuffer[String]()
    var description : StringExpression = ""
    var properties : ArrayBuffer[Property] = ArrayBuffer[Property]()
    var pluralized = false
    var proper = false
    var global = false

    var parentContainer : Container = null
    ZextObject.nouns += this

    def TransferTo(container: Container) = {
        parentContainer.contents.remove(parentContainer.contents.indexOf(this))
        parentContainer = container
        container.contents.addOne(this)
    }

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
        if(pluralized)
            return "are"
        "is"
    }

    def ?(property: Property) : Boolean =  {
        properties.contains(property)
    }


    def isVisible(room: Room) = {

        /*
        conditions for item visibility:
        1) in the same container
        2) in the player's inventory
        3) in a transparent container contained (transitively) within the room
        4) the current room or an adjacent room (? maybe ?),
        5) a global object
        */

        val sameRoom = room == parentContainer
        val inInventory = parentContainer == inventory
        val connectedRoom = room.connections.values.exists(_ == this)

        // if this breaks, i deserve it
        val visibleTransitively = {
            var parent = parentContainer
            while(parent != null && parent.transparent && parent != room && parent.isInstanceOf[ZextObject]){
                parent = parent.asInstanceOf[ZextObject].parentContainer
            }

            parent == room
        }



        global || sameRoom || room == this || inInventory || connectedRoom || visibleTransitively
    }

    def isAccessible = {

    }

    override def toString: String = definite
}



object thing {
    extension(d: StringExpression)  {
        inline def unary_~(using c : Container) : thing = {
            thing() named Macros.variableName desc d
        }
    }

    enum NounAmount {
        case singular, plural, some
    }
}


class thing(using c : Container) extends ZextObject{

    parentContainer = c
    c.contents += this

    def FixName(s : String): String  ={
        s.replace('_', ' ')
    }

    def SetName(s : String): Unit = {
        val fixed = FixName(s)
        name = fixed
        val words = fixed.split(' ')
        if(words.length > 1) aliases.addAll(words)
    }


    def desc(desc : StringExpression) : this.type = {
        description = desc
        this
    }

    def is(prop: Property) : this.type = {
        properties += prop
        this
    }

    def are(prop: Property) : this.type = {
        properties += prop
        this
    }


    def named(name : String) : this.type = {
        SetName(name)
        this
    }

    def has(prop: Property) : this.type = {
        properties += prop
        this
    }

    def aka(s : String) : this.type  = {
        aliases.addOne(s)
        this
    }

    def amount(nounAmount: NounAmount):  this.type ={
        if nounAmount == NounAmount.plural then pluralized = true
        if (nounAmount == NounAmount.some) {
            pluralized = true
            indefiniteArticle = "some"
        }
        this
    }
}

class Supporter(using c : Container) extends thing with Container


