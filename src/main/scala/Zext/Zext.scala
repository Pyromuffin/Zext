package Zext

import Zext.Direction.*
import Zext.Interpreter.*
import Zext.Parser.*
import Zext.QueryPrecedence.*
import Zext.Rule.*
import Zext.World.*
import Zext.Thing.NounAmount
import Zext.RuleContext.*

import java.lang.reflect.{Constructor, Modifier}
import scala.collection.mutable.ArrayBuffer
import scala.language.{implicitConversions, postfixOps}
import Condition.*
import Zext.Actions.*
import Zext.ZextObject.allObjects
import org.apache.commons.lang3.reflect.FieldUtils
import zobjectifier.Macros

import java.io.{IOException, ObjectInputStream, ObjectOutputStream}
import scala.reflect.TypeTest

trait Property


object holdable extends Property
object fixed extends Property
object scenery extends Property
object wet extends Property

object exports{
    export Interpreter.*
    export Rule.*
    export Condition.*
    export World.*
    export Direction.*
    export ZextObject.*
    export Actions.*
    export Zext.Thing.*
    export Zext.Thing.NounAmount.*
    export Zext.Device.*
    export Zext.StringExpression.*
    export Zext.RuleContext.*
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





object ZextObject{

    def Destroy(zextObject: ZextObject) = {
        zextObject.parentContainer.contents.remove(zextObject.parentContainer.contents.indexOf(zextObject))
        zextObject.parentContainer = null
        // anything else?
    }

    val allObjects = ArrayBuffer[ZextObject]()
}


case class ZextObjectSerializationProxy(index : Int){
    def readResolve(): java.lang.Object = {
        allObjects(index)
    }
}


object determiningAccessibility extends Action(2) {

 inflict(determiningAccessibility) {

     // determining if noun is accessible to secondNoun

     // in the same room, or part of their inventory/contents
     if (noun.parentContainer == secondNoun.parentContainer || noun.parentContainer == secondNoun || secondNoun.parentContainer == noun || noun == secondNoun) {
         continue
     }

     // if noun is composite, check if the composite object is accessible to secondNoun
     if (noun.isComposite) {
         result(execute(determiningAccessibility, noun.compositeObject, secondNoun))
     }

     // if noun is in a container, and that container is open, check if the parent container is accessible to secondNoun
     if (noun.parentContainer != null && noun.parentContainer.open && noun.parentContainer.isInstanceOf[ZextObject]) {
         result(execute(determiningAccessibility, noun.parentContainer.asInstanceOf[ZextObject], secondNoun))
     }

     // if not, fail
     stop
 }

}

object determiningVisibility extends Action(2) {


    /*
    def isTransitivelyVisibleWithin(z : ZextObject, container : ZextObject) : Boolean = {
        if(z == container)
            return true

        for(part <- container.parts) {
            if (isTransitivelyVisibleWithin(z, part))
                return true
        }

        container.get[Container] does { c =>

            if (c.open || c.transparent) {
                for (cc <- c.contents) {
                    if (isTransitivelyVisibleWithin(z, cc))
                        return true
                }
            }
        }

        false
    }
    */

    // this no longer has a bearing on what are parsable words

    // determining if noun is visible to second noun
    inflict(determiningVisibility){

        /*
        conditions for object visibility:
        1) in the same container
        2) in the object's inventory
        3) in a transparent container contained (transitively) within the room
        4) the current room or an adjacent room (? maybe ?),
        5) a part of a visible object
        6) a backdrop in the current room
        7) a backdrop in the current region
        8) a backdrop that is in the everywhere region

        */

        //@todo figure out if we want backdrops themsevles to be visible.

        val room = secondNoun.findRoom()
        val regionBackdrops = World.currentWorld.regions.filter(region => region.rooms.contains(room)).flatMap(_.backdrops)
        val backdrops = regionBackdrops.addAll(room.backdrops).addAll(everywhere.backdrops)

        // direct visibility for same container or inventory, or room, or self, or is in the set of backdrops visible backdrops
        if (noun.parentContainer == secondNoun.parentContainer || noun.parentContainer == secondNoun || secondNoun.parentContainer == noun || noun == secondNoun || backdrops.contains(noun)) {
            continue
        }

        // if noun is composite, check if the composite object is visible to secondNoun
        if (noun.isComposite) {
            result(execute(determiningVisibility, noun.compositeObject, secondNoun))
        }

        // if noun is in a container, and that container is open or transparent, check if the parent container is visible to secondNoun
        if (noun.parentContainer != null && (noun.parentContainer.open || noun.parentContainer.transparent) && noun.parentContainer.isInstanceOf[ZextObject]) {
            result(execute(determiningVisibility, noun.parentContainer.asInstanceOf[ZextObject], secondNoun))
        }

        stop
        /*
        visibleSet.addAll(FindAllTransitivelyVisible(zextObject.parentContainer))
        visibleSet.addAll(zextObject.contents) // this doesn't transitively give you the contents of the player or the globals.
        visibleSet.addAll(World.currentWorld.globals)
        visibleSet.addAll(playerLocation.backdrops.flatMap(_.contents))

        for (region <- World.currentWorld.regions) {
            if (region.rooms.contains(playerLocation)) {
                visibleSet.addAll(region.backdrops.flatMap(_.contents))
            }
        }

        visibleSet.addAll(everywhere.backdrops.flatMap(_.contents))

        visibleSet
        */
    }

}

@SerialVersionUID(100L)
abstract class ZextObject extends ParsableType(PartOfSpeech.noun) with Serializable with reflect.Selectable {

    val dynamic = false

    var objectID = allObjects.length
    allObjects.addOne(this)

    var definiteArticle: String = "the"
    var indefiniteArticle: String = "a"
    val name: StringExpression
    var aliases = ArrayBuffer[StringExpression]()
    val description: StringExpression
    var properties: ArrayBuffer[Property] = ArrayBuffer[Property]()
    var pluralized : Option[Boolean] = None
    var autoexplode = true
    var proper = false


    def findRoom(): Room = {
        parentContainer match {
            case r: Room => r
            case playerClass: PlayerClass => playerLocation
            case backdrop: Backdrop => nowhere
            case zextObject: ZextObject => zextObject.findRoom()
            case null => throw new Exception(s"ZextObject $this has null parent container, and we tried to find its room")
            //case _ => nowhere
        }
    }

    //@todo eventually fix zextobject/thing confusion
    var parentContainer: Container = null
    val parts = ArrayBuffer[ZextObject]()
    var compositeObject: ZextObject = null // turn composite into a property


    override def equals(obj: Any) = {
        obj match {
            case zextObjectProxy: ZextObjectProxy[_] => objectID == zextObjectProxy.objectID
            case zextObject: ZextObject => objectID == zextObject.objectID
            case null => false
        }
    }

    infix def transferTo(container: Container): Unit = {
        parentContainer.contents.remove(parentContainer.contents.indexOf(this))
        parentContainer = container
        container.contents.addOne(this)
    }


    def definite: String = {
        if (proper)
            return name.toString

        definiteArticle + " " + name
    }

    def indefinite: String = {
        if (proper)
            return name.toString

        indefiniteArticle + " " + name
    }

    def be: String = {
        if (pluralized.isDefined && pluralized.get)
             "are"
        else if(pluralized.isEmpty && this.isInstanceOf[Thing] && this[Thing].isAutomaticallyPlural)
             "are"
        else
            "is"
    }

    infix def is(rhs : String): String = {
        toString + " " + be + " " + rhs
    }

    def isComposite = compositeObject != null


    def isVisibleTo(other: ZextObject): Boolean = {
        execute(determiningVisibility, this, other)
    }

    def isAccessibleTo(other: ZextObject): Boolean = {
        execute(determiningAccessibility, this, other)
    }

    override def toString: String = definite


    def get[T](using TypeTest[Property, T]): Option[T] = {
        // if a zextobject has more than one property of the same "type" then it will give ?? one
        properties.find(canBecome[Property, T]).map(_.asInstanceOf[T])
    }

    infix def composes(zextObject: ZextObject): this.type = {
        this transferTo nowhere
        compositeObject = zextObject
        zextObject.parts.addOne(this)
        this
    }

    infix def inside(container : Container): this.type = {
        this transferTo container
        this
    }

    def apply[T](using TypeTest[Property, T]): T = {
        val maybe = this.get[T]
        if (maybe.isDefined) maybe.get
        else this.asInstanceOf[T]
    }

    def isType[T](using TypeTest[ZextObject, T]) = canBecome[ZextObject, T](this)

    def SerializeMembers(oos: ObjectOutputStream): Unit = {
        val fields = FieldUtils.getAllFields(getClass)
        for (f <- fields) {
            val modifiers = f.getModifiers
            val isNotFinal = (modifiers & Modifier.FINAL) == 0
            if (isNotFinal) {
                f.setAccessible(true)
                oos.writeObject(f.get(this))
            }
        }
    }

    def DeserialzeMembers(ois: ObjectInputStream): Unit = {

        val fields = FieldUtils.getAllFields(getClass)
        for (f <- fields) {

            val modifiers = f.getModifiers
            val isNotFinal = (modifiers & Modifier.FINAL) == 0
            if (isNotFinal) {
                f.setAccessible(true)
                val obj = ois.readObject()
                f.set(this, obj)
            }
        }
    }

    def writeReplace() : java.lang.Object = {

        if(!dynamic){
            ZextObjectSerializationProxy(objectID)
        }
        else {
            this
        }
    }
}

object Thing {
    extension(d: StringExpression)  {
        inline def unary_~(using c : Container) : Thing = {
            SimpleThing(FixName(Macros.variableName), d)
        }

        inline def initially(desc: StringExpression)(using c: Container): Thing = {
            SimpleThing(FixName(Macros.variableName), desc) and RoomDescription(d)
        }
    }

    def FixName(s: String): String = {
        s.replace('_', ' ')
    }

    enum NounAmount {
        case singular, plural, some
    }
}

case class RoomDescription(desc: StringExpression) extends Property {
    var disturbed = false
}

case class SimpleThing(name : StringExpression, description: StringExpression)(using c : Container) extends Thing

abstract class Thing(using c : Container) extends ZextObject{

    var valueInLucre = 0

    parentContainer = c
    c.contents += this

    def isAutomaticallyPlural = {
        Inflector.pluralize(name.toString) == name.toString
    }

    infix def is(prop: Property) : this.type = {
        properties += prop
        this
    }

    infix def and(prop: Property): this.type = {
        properties += prop
        this
    }

    infix def are(prop: Property) : this.type = {
        properties += prop
        this
    }

    infix def aka(s : String) : this.type  = {
        aliases.addOne(s)
        this
    }

    infix def amount(nounAmount: NounAmount):  this.type ={
        if (nounAmount == NounAmount.plural) {
            pluralized = Some(true)
        }

        if (nounAmount == NounAmount.singular) {
            pluralized = Some(false)
        }

        if (nounAmount == NounAmount.some) {
            indefiniteArticle = "some"
        }

        this
    }

    infix def worth(value : Int) : this.type = {
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

abstract class Device(using Container) extends Thing {

    var on = false

    def off = !on

    var offDesc: StringExpression = null
    var onDesc: StringExpression = null
    val description = s"${if (on) onDesc else offDesc}"
}

