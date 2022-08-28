package Zext

import Zext.Direction.*
import Zext.Interpreter.*
import Zext.Parser.*
import Zext.QueryPrecedence.*
import Zext.Rule.*
import Zext.World.*
import Zext.thing.NounAmount

import java.lang.reflect.Constructor
import scala.collection.mutable.ArrayBuffer
import scala.language.{implicitConversions, postfixOps}
import Condition.*

import scala.reflect.TypeTest

trait Property


object holdable extends Property
object fixed extends Property
object scenery extends Property
object wet extends Property
case class initialDescription(desc : StringExpression) extends Property



object exports{
    export Interpreter.*
    export Rule.*
    export Condition.*
    export World.*
    export Direction.*
    export ZextObject.*
    export Actions.*
    export Inflector.*
    export Zext.thing.*
    export Zext.thing.NounAmount.*
    export ZextObject.device.*
}


trait Container {
    given c : Container = this
    var contents : ArrayBuffer[ZextObject] = ArrayBuffer[ZextObject]()
    var open = true
    var transparent = true

    def ContentsString : String = {
        var s = ""
        for (i <- c.contents) {
            s += i.indefinite + ", "
        }
        s.stripSuffix(", ")
    }

    def has(zextObject : => ZextObject) = Condition( zextObject.parentContainer == this, QueryPrecedence.Containment)
    def had(zextObject : => ZextObject) = Condition( zextObject.parentContainer == this, QueryPrecedence.Containment, true)
    def lacks(zextObject : => ZextObject) = Condition( zextObject.parentContainer != this, QueryPrecedence.Containment)
    def lacked(zextObject : => ZextObject) = Condition( zextObject.parentContainer != this, QueryPrecedence.Containment, true)
}


object ZextObject{
    val all = ArrayBuffer[ZextObject]()

    object device {
        // hey idiot, don't put these definition in a class or they're get copied every time someone makes a device~!

        object turningOn extends Action(1,"turn on", "switch on", "activate")
        object turningOff extends Action(1,"turn off", "switch off", "deactivate")
        object switching extends Action(1,"switch", "toggle")

        inflict(turningOn, of[device]) {
            val d = noun[device]
            if (d.on) {
                Say(s"$noun is already on")
                false
            } else {
                d.on = true
                true
            }
        }

        report(turningOn, of[device]) {
            Say(s"I turned on $noun")
        }

        inflict(turningOff, of[device]) {
            val d = noun[device]
            if (d.off) {
                Say(s"$noun is already off")
                false
            } else {
                d.on = false
                true
            }
        }

        report(turningOff, of[device]) {
            Say(s"I turned off $noun")
        }

        inflict(switching, of[device]){
            val d = noun[device]
            if(d.on) execute(turningOff, d)
            else execute(turningOn, d)
        }
    }

    class device(using Container) extends thing {

        var on = false
        def off = !on

        var offDesc : StringExpression = null
        var onDesc :  StringExpression = null
        description = s"${if(on) onDesc else offDesc}"
    }


    class Supporter(using Container) extends thing with Container
    class Box(using Container) extends thing with Container {
        transparent  = false
        open = false
    }
}

class ZextObject extends ParsableType(PartOfSpeech.noun) {

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

    ZextObject.all += this

    def transferTo(container: Container) = {
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
        val inInventory = parentContainer == player
        val connectedRoom = room.connections.values.exists(_ == this)

        // if this breaks, i deserve it
        val visibleTransitively = {
            var parent = parentContainer
            while(parent != null && (parent.transparent || parent.open) && parent != room && parent.isInstanceOf[ZextObject]){
                parent = parent.asInstanceOf[ZextObject].parentContainer
            }

            parent == room
        }



        global || sameRoom || room == this || inInventory || connectedRoom || visibleTransitively
    }

    def isAccessible(room: Room) : Boolean = {

        val accessibleTransitively = {
            var parent = parentContainer
            while (parent != null && parent.open && parent != room && parent.isInstanceOf[ZextObject]) {
                parent = parent.asInstanceOf[ZextObject].parentContainer
            }

            parent == room
        }


        isVisible(room) && accessibleTransitively
    }


    override def toString: String = definite

    def asSecondNoun : Condition = {
        Condition(secondNoun == this, QueryPrecedence.SecondObject)
    }

    def get[T](using TypeTest[Property, T]) : Option[T] = {
        // if a zextobject has more than one property of the same "type" then it will give ?? one
        properties.find( canBecome[Property,T]).map( _.asInstanceOf[T])
    }

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

        /*
        if(noun.pluralized){
            words = words.concat(words.map(Inflector.singularize))
        }
        */
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






