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
import Zext.Relation.{ManyToMany, OneToMany}
import Zext.Relations.*
import Zext.RuleControl.*
import Zext.SetComprehension.AllOf
import Zext.ZextObject.allObjects
import org.apache.commons.lang3.reflect.FieldUtils
import zobjectifier.Macros

import java.io.{IOException, ObjectInputStream, ObjectOutputStream}
import scala.reflect.TypeTest

trait Property extends Relatable

object fixed extends Property
object scenery extends Property
object wet extends Property
object proper extends Property
object unlisted extends Property // for things we dont want listed


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
    export Zext.Idea.*
    export Zext.ControlCodes.*
}


object listing extends Action(1) {
    instead(listing, unlisted) Stop
}

def ListNamesNicely(stuff: Seq[ZextObject]): Option[String] = {

    val filtered = stuff.filter(n => ExecuteAction(listing, subject = system, target = n))
    val sorted = filtered.sortBy(_.name.toString)

    if (sorted.isEmpty)
        return None

    if (sorted.length == 1) {
        return Some(sorted.head.indefinite)
    }

    if (sorted.length == 2) {
        return Some(sorted.head.indefinite + " and " + sorted(1).indefinite)
    }

    var s = ""
    for (i <- 0 until sorted.length - 1)
        s += sorted(i).indefinite + ", "

    Some(s + "and " + sorted.last.indefinite)
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


    implicit def setToComprehension[X <: Relatable](set: Set[X]): SetComprehension[X] = {
        AllOf(set.toSeq*)
    }

}


case class ZextObjectSerializationProxy(index : Int){
    def readResolve(): java.lang.Object = {
        allObjects(index)
    }
}


object determiningAccessibility extends Action(1) with Context[Action]{

 inflict(determiningAccessibility) {

     // determining if noun is accessible to subject

     val nounLocation = noun.resolve match {
         case t: Thing => t.location
         case _ => null
     }

     val secondNounLocation = subject.resolve match {
         case t: Thing => t.location
         case _ => null
     }

     // in the same room, or part of their inventory/contents
     if (nounLocation == secondNounLocation || nounLocation == subject || secondNounLocation == noun || noun == subject) {
         continue
     }

     // if noun is composite, check if the composite object is accessible to secondNoun
     if (noun.isType[Thing] && noun[Thing].isComposite) {
         ruleReturn(ExecuteAction(determiningAccessibility, target = noun[Thing].compositeObject))
     }

     // if noun is in a container, and that container is open, check if the parent container is accessible to secondNoun
     if (nounLocation != null && nounLocation.open) {
         ruleReturn(ExecuteAction(determiningAccessibility, target = nounLocation))
     }

     // if not, fail
     stop
 }

}

object determiningVisibility extends Action(1) with Context[Action]{

    // determining if noun is visible to subject
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

        // allow actions to control visibility rules for those particular actions.

        val subjectLocation = subject.resolve match {
            case t: Thing => t.location
            case _ => null
        }

        val targetLocation = noun.resolve match {
            case t: Thing => t.location
            case _ => null
        }

        if(ZextObject.globals.contains(noun)){
            continue
        }

        //@todo figure out if we want backdrops themsevles to be visible.
        val room = subject[Thing].room
        val regionBackdrops = World.currentWorld.regions.filter(region => region.rooms.contains(room)).flatMap(_.parents(Backdropping))
        val backdrops = regionBackdrops.addAll(room.parents(Backdropping)).addAll(everywhere.parents(Backdropping))

        // direct visibility for same container or inventory, or room, or self, or is in the set of visible backdrops
        if (subjectLocation == targetLocation || targetLocation == subject || subjectLocation == noun || subject == noun || backdrops.contains(noun) ) {
            continue
        }

        // if noun is composite, check if the composite object is visible to subject
        if (noun.isType[Thing] && noun[Thing].isComposite) {
            ruleReturn( subject.canSee(noun[Thing].compositeObject, GetActionContext()) )
        }

        // if noun is in a container, and that container is open or transparent, check if the parent container is visible to subject
        if (targetLocation != null && (targetLocation.open || targetLocation.transparent)) {
            ruleReturn( subject.canSee(targetLocation, GetActionContext()) )
        }

        stop
    }

}


implicit object property_having extends Relation[Relatable, Property] with ManyToMany {
    extension [X <: Source](subject: X)
        infix def is[Y <: Target](target: Y*): X = relates(subject, target)
}


@SerialVersionUID(100L)
abstract class ZextObject extends ParsableType(PartOfSpeech.noun) with Serializable with reflect.Selectable with Relatable {

    val dynamic = false

    allObjects.addOne(this)

    var definiteArticle: String = "the"
    val name: StringExpression
    val aliases = ArrayBuffer[StringExpression]()
    val description: StringExpression
    //var properties: ArrayBuffer[Property] = ArrayBuffer[Property]()
    var pluralized : Option[Boolean] = None
    var autoexplode = true
    var mass = false

    def GetName() : String = {
        ExecuteContextAction(printing_name(name.toString), subject = system, target = this).ret
    }

     def indefiniteArticle: String = {
        val firstLetter = GetName()(0).toLower
        if(mass)
             "some"
        else if(firstLetter == 'a' || firstLetter == 'e' || firstLetter == 'i' || firstLetter == 'o' || firstLetter == 'u' )
            "an"
        else
            "a"
    }

    def definite: String = {
        if (this is proper?)
            return GetName()

        definiteArticle + " " + GetName()
    }

    def indefinite: String = {
        if (this is proper?)
            return GetName()

        indefiniteArticle + " " + GetName()
    }

    def be: String = {
        if (pluralized.isDefined && pluralized.get)
             "are"
        else if(pluralized.isEmpty && this.isInstanceOf[Thing] && this[Thing].isAutomaticallyPlural)
             "are"
        else
            "is"
    }

    infix def iz(rhs : String): String = {
        toString + " " + be + " " + rhs
    }

    infix def and(prop: Property): this.type = {
        this is prop
    }

    infix def are(prop: Property): this.type = {
        this is prop
    }

    infix def aka(s: String): this.type = {
        aliases.addOne(s)
        this
    }

    override def location : ZContainer = nowhere

    def canSee(other: ZextObject, forAction : Action): Boolean = {
        ExecuteContextAction(determiningVisibility(forAction), subject = this, target = other).res
    }

    def canAccess(other: ZextObject, forAction : Action): Boolean = {
        ExecuteContextAction(determiningAccessibility(forAction), subject = this, target = other).res
    }


    override def toString: String = definite

    def get[T](using TypeTest[Property, T]): Option[T] = {
        // if a zextobject has more than one property of the same "type" then it will give ?? one
        val properties = relations(property_having)
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

    override def location = parent(Containment).get

    override val name = autoname

    def isAutomaticallyPlural = {
        Inflector.pluralize(name.toString) == name.toString
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
        if (d.on) ExecuteAction(turningOff, target = d)
        else ExecuteAction(turningOn, target = d)
    }
}

abstract class Device(using ZContainer) extends Thing {

    var on = false

    def off = !on

    var offDesc: StringExpression = null
    var onDesc: StringExpression = null
    val description = s"${if (on) onDesc else offDesc}"
}

