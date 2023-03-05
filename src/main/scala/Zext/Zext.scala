package Zext

import Zext.Direction.*
import Zext.Interpreter.*
import Zext.Parser.*
import Zext.QueryPrecedence.*
import Zext.Rule.*
import Zext.World.*
import Zext.Thing.NounAmount

import java.lang.reflect.Constructor
import scala.collection.mutable.ArrayBuffer
import scala.language.{implicitConversions, postfixOps}
import Condition.*
import Zext.Actions.does

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
    export Zext.Thing.*
    export Zext.Thing.NounAmount.*
    export Device.*
}


def ListNamesNicely(stuff: Seq[ZextObject]): Option[String] = {
    if (stuff.isEmpty)
        return None

    if (stuff.length == 1) {
        return Some(stuff.head.indefinite)
    }

    if (stuff.length == 2) {
        return Some(stuff.head.indefinite + " and " + stuff(1).indefinite)
    }

    var s = ""
    for (i <- 0 until stuff.length - 1)
        s += stuff(i).indefinite + ", "

    Some(s + "and " + stuff.last.indefinite)
}


trait Container {
    given c : Container = this
    var contents : ArrayBuffer[ZextObject] = ArrayBuffer[ZextObject]()
    var open = true
    var transparent = true
    var automaticallyListContents = true

    def ContentsString : Option[String] = {
       ListNamesNicely(contents.toSeq)
    }

    def has(zextObject : => ZextObject) = Condition( zextObject.parentContainer == this, QueryPrecedence.Containment)
    def had(zextObject : => ZextObject) = Condition( zextObject.parentContainer == this, QueryPrecedence.Containment, true)
    def lacks(zextObject : => ZextObject) = Condition( zextObject.parentContainer != this, QueryPrecedence.Containment)
    def lacked(zextObject : => ZextObject) = Condition( zextObject.parentContainer != this, QueryPrecedence.Containment, true)
}


object ZextObject{
    val globals = ArrayBuffer[ZextObject]()
    def Destroy(zextObject: ZextObject) = {
        zextObject.parentContainer.contents.remove(zextObject.parentContainer.contents.indexOf(zextObject))
        zextObject.parentContainer = null
        // anything else?
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


    //@todo eventually fix zextobject/thing confusion
    var parentContainer : Container = null
    val parts = ArrayBuffer[ZextObject]()
    var compositeObject : ZextObject = null

    // ZextObject.all += this

    def transferTo(container: Container) : Unit = {
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



    def isComposite = compositeObject != null

    def isAccessible(room: Room) : Boolean = {

        if(parentContainer == room){
            return true
        }

        if(isComposite) {
            return compositeObject.isAccessible(room)
        }

        if(parentContainer != null && parentContainer.open && parentContainer.isInstanceOf[ZextObject]){
            return parentContainer.asInstanceOf[ZextObject].isAccessible(room)
        }

        false
    }


    override def toString: String = definite

    def asSecondNoun : Condition = {
        Condition(secondNoun == this, QueryPrecedence.SecondObject)
    }

    def get[T](using TypeTest[Property, T]) : Option[T] = {
        // if a zextobject has more than one property of the same "type" then it will give ?? one
        properties.find( canBecome[Property,T]).map( _.asInstanceOf[T])
    }

    def composes(zextObject: ZextObject): this.type = {
        this transferTo nowhere
        compositeObject = zextObject
        zextObject.parts.addOne(this)
        this
    }

    def isType[T](using TypeTest[ZextObject, T]) = canBecome[ZextObject, T](this)

}



object Thing {
    extension(d: StringExpression)  {
        inline def unary_~(using c : Container) : Thing = {
            Thing() named Macros.variableName desc d
        }
    }

    enum NounAmount {
        case singular, plural, some
    }
}


class Thing(using c : Container) extends ZextObject{

    var valueInLucre = 0

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

    def worth(value : Int) = {
        valueInLucre = value
        this
    }
}


object Device {
    // hey idiot, don't put these definition in a class or they're get copied every time someone makes a device~!
    object turningOn extends Action(1, "turn on", "switch on", "activate")
    object turningOff extends Action(1, "turn off", "switch off", "deactivate")
    object switching extends Action(1, "switch", "toggle")

    inflict(turningOn, of[Device]) {
        val d = noun[Device]
        if (d.on) {
            Say(s"$noun is already on")
            stop
        }

        d.on = true
    }

    report(turningOn, of[Device]) {
        Say(s"I turned on $noun")
    }

    inflict(turningOff, of[Device]) {
        val d = noun[Device]
        if (d.off) {
            Say(s"$noun is already off")
            stop
        }

        d.on = false
    }

    report(turningOff, of[Device]) {
        Say(s"I turned off $noun")
    }

    inflict(switching, of[Device]) {
        val d = noun[Device]
        if (d.on) execute(turningOff, d)
        else execute(turningOn, d)
    }
}

class Device(using Container) extends Thing {

    var on = false

    def off = !on

    var offDesc: StringExpression = null
    var onDesc: StringExpression = null
    description = s"${if (on) onDesc else offDesc}"
}


class Supporter(using Container) extends Thing with Container

class Box(open_and_transparent : Boolean = false) (using Container) extends Thing with Container {
    transparent = open_and_transparent
    open = open_and_transparent
}

