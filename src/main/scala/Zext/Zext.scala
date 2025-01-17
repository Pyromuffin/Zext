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
    var pluralized = false
    var proper = false



    //@todo eventually fix zextobject/thing confusion
    var parentContainer: Container = null
    val parts = ArrayBuffer[ZextObject]()
    var compositeObject: ZextObject = null // turn composite into a property


    override def equals(obj: Any) = {
        obj match {
            case zextObjectProxy: ZextObjectProxy[_] => objectID == zextObjectProxy.objectID
            case zextObject: ZextObject => objectID == zextObject.objectID
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
        if (pluralized)
            return "are"
        "is"
    }

    infix def is(rhs : String): String = {
        toString + " " + be + " " + rhs
    }

    def isComposite = compositeObject != null

    def isAccessible(room: Room): Boolean = {

        if (parentContainer == room) {
            return true
        }

        if (isComposite) {
            return compositeObject.isAccessible(room)
        }

        if (parentContainer != null && parentContainer.open && parentContainer.isInstanceOf[ZextObject]) {
            return parentContainer.asInstanceOf[ZextObject].isAccessible(room)
        }

        false
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

    if(isAutomaticallyPlural){
        pluralized = true
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
            pluralized = true
        }

        if (nounAmount == NounAmount.singular) {
            pluralized = false
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

