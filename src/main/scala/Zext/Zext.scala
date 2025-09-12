package Zext

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
import Zext.Idea.allIdeas
import Zext.Relations.*
import Zext.SetComprehension.AllOf
import Zext.ZextObject.allObjects
import org.apache.commons.lang3.reflect.FieldUtils
import zobjectifier.Macros

import java.io.{IOException, ObjectInputStream, ObjectOutputStream}
import scala.reflect.TypeTest

trait Property


object fixed extends Property
object scenery extends Property
object wet extends Property
object proper extends Property
object built_in extends Property // for built in ideas we dont really want to put in the idea list

object exports{
    export Interpreter.*
    export Rule.*
    export Condition.*
    export World.*
    export ZextObject.*
    export Actions.*
    export Zext.Thing.*
    export Zext.Thing.NounAmount.*
    export Zext.Device.*
    export Zext.StringExpression.*
    export Zext.RuleContext.*
    export Relations.*
    export Zext.Relations.RoomAdjacency.*
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


type TT[X] = TypeTest[Any, X]


object ZextObject{

    def Destroy(zextObject: ZextObject) = {
        // unrelate all relations.
        val relations = zextObject.listRelations()
        for(r <- relations)
            zextObject.removeRelation(r)

    }

    val allObjects = ArrayBuffer[ZextObject]()
    val globals = ArrayBuffer[ZextObject]() // always visible and accessible.

    def GetAll[T <: Relatable : TT as tt] : Seq[T] = {

        allObjects.filter { z =>
           tt.unapply(z).isDefined
        }.map(_.asInstanceOf[T]).toSeq

    }

    implicit def toComprehension[X <: ZextObject](x : X) : SetComprehension[X] = {
        AllOf(x)
    }

}


case class ZextObjectSerializationProxy(index : Int){
    def readResolve(): java.lang.Object = {
        allObjects(index)
    }
}


object determiningAccessibility extends Action(2) {

 inflict(determiningAccessibility) {

     // determining if noun is accessible to secondNoun

     val nounLocation = noun.resolve match {
         case t: Thing => t.location
         case _ => null
     }

     val secondNounLocation = secondNoun.resolve match {
         case t: Thing => t.location
         case _ => null
     }

     // in the same room, or part of their inventory/contents
     if (nounLocation == secondNounLocation || nounLocation == secondNoun || secondNounLocation == noun || noun == secondNoun) {
         continue
     }

     // if noun is composite, check if the composite object is accessible to secondNoun
     if (noun.isType[Thing] && noun[Thing].isComposite) {
         result(execute(determiningAccessibility, noun[Thing].compositeObject, secondNoun))
     }

     // if noun is in a container, and that container is open, check if the parent container is accessible to secondNoun
     if (nounLocation != null && nounLocation.open) {
         result(execute(determiningAccessibility, nounLocation, secondNoun))
     }

     // if not, fail
     stop
 }

}

object determiningVisibility extends Action(2) {

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
        9) a global object.
        10) a known idea
        */

        val nounLocation = noun.resolve match {
            case t: Thing => t.location
            case _ => null
        }

        val secondNounLocation = secondNoun.resolve match {
            case t: Thing => t.location
            case _ => null
        }

        if(ZextObject.globals.contains(noun)){
            continue
        }

        //@todo figure out if we want backdrops themsevles to be visible.
        val room = secondNoun[Thing].room
        val regionBackdrops = World.currentWorld.regions.filter(region => region.rooms.contains(room)).flatMap(_.parents(Backdropping))
        val backdrops = regionBackdrops.addAll(room.parents(Backdropping)).addAll(everywhere.parents(Backdropping))
        //val backdropContents = backdrops.flatMap(_.contents)

        // direct visibility for same container or inventory, or room, or self, or is in the set of visible backdrops
        if (nounLocation == secondNounLocation || nounLocation == secondNoun || secondNounLocation == noun || noun == secondNoun || backdrops.contains(noun) ) {
            continue
        }

        // if noun is composite, check if the composite object is visible to secondNounbackdrops = {ArrayBuffer@3175} size = 2
        if (noun.isType[Thing] && noun[Thing].isComposite) {
            result(execute(determiningVisibility, noun[Thing].compositeObject, secondNoun))
        }

        // if noun is in a container, and that container is open or transparent, check if the parent container is visible to secondNoun
        if (nounLocation != null && (nounLocation.open || nounLocation.transparent)) {
            result(execute(determiningVisibility, nounLocation, secondNoun))
        }

        stop
    }

}

@SerialVersionUID(100L)
abstract class ZextObject extends ParsableType(PartOfSpeech.noun) with Serializable with reflect.Selectable with Relatable {

    val dynamic = false

    var objectID = allObjects.length
    allObjects.addOne(this)

    var definiteArticle: String = "the"
    val name: StringExpression
    val aliases = ArrayBuffer[StringExpression]()
    val description: StringExpression
    var properties: ArrayBuffer[Property] = ArrayBuffer[Property]()
    var pluralized : Option[Boolean] = None
    var autoexplode = true
    var mass = false

    override def equals(obj: Any) = {
        obj match {
            case zextObjectProxy: ZextObjectProxy[?] => objectID == zextObjectProxy.resolve.objectID
            case zextObject: ZextObject => objectID == zextObject.objectID
            case null => false
        }
    }

     def indefiniteArticle: String = {
        val firstLetter = name.toString(0).toLower
        if(mass)
             "some"
        else if(firstLetter == 'a' || firstLetter == 'e' || firstLetter == 'i' || firstLetter == 'o' || firstLetter == 'u' )
            "an"
        else
            "a"
    }

    def definite: String = {
        if (properties.contains(proper))
            return name.toString

        definiteArticle + " " + name
    }

    def indefinite: String = {
        if (properties.contains(proper))
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

        inline infix def initially(desc: StringExpression)(using c: ZContainer): Thing = {
            SimpleThing(desc) and RoomDescription(d)
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

case class SimpleThing(description: StringExpression)(using c : Container & ZextObject) extends Thing


abstract class Thing (using c : Container & ZextObject) extends ZextObject {

    c holds this

    var autoname: String = null

    // called by compiler plugin with valdef name
    def SetName(s: String): this.type = {
        if(autoname == null) autoname = Thing.FixName(s)
        this
    }

    def room : Room = {
        location match {
            case r: Room => r
            case backdrop: Backdrop => nowhere
            case z if (z == null) => throw new Exception(s"ZextObject $this has null parent container, and we tried to find its room")
            case thing: Thing => thing.room
            //case _ => nowhere
        }
    }

    def location = parent(Containment).get

    override val name = autoname

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
            mass = true
        }

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

abstract class Device(using ZContainer) extends Thing {

    var on = false

    def off = !on

    var offDesc: StringExpression = null
    var onDesc: StringExpression = null
    val description = s"${if (on) onDesc else offDesc}"
}

object Idea {
    val allIdeas = ArrayBuffer[Idea]()

    // known ideas are always visible.
    // visibility in this sense means that they can be parsed.
    inflict(determiningVisibility, noun[Idea].known) {
        replace
    }

    // @todo figure out how to stop this from leaking information by thinking about things you haven't seen.
    object thinking extends Action(1, "think", "think of", "imagine") {

        requiresVisibility = false

        before(thinking, noun[Idea].discoverable) {
            noun[Idea].known = true
            if(noun[Idea].discoverable) Say(s"A new thought about $noun occurs to you!")
            noun[Idea].discoverable = false
        }

        report(thinking, noun[Idea].known) {
            Say(s"Thinking of $noun reveals: ${noun.description}")
        }
    }

    object ideating extends Action(0, "ideas", "thoughts", "knowledge") {

        report(ideating) {
            val knownIdeas = Idea.allIdeas.filterNot(_.properties.contains(built_in)).filter(_.known)
            val ideasList = ListNamesNicely(knownIdeas.toSeq)
            if(ideasList.isEmpty){
                Say("Much is unknown.")
            } else {
                Say("The following ideas are known to you: " + ideasList.get)
            }
        }
    }
}

// innate ideas are initially known.
// discoverable ideas can be added to the ideas list just by thinking about them
class Idea(override val name : StringExpression, innate : Boolean = true, var discoverable : Boolean = false) extends ZextObject {
    override val description = "the idea of " + name
    properties += proper

    var known = innate
    if(discoverable) known = false

    allIdeas.addOne(this)
}