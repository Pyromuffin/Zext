package Zext

import Zext.Interpreter.*
import Zext.Parser.*
import Zext.Rule.*
import Zext.Thing.NounAmount
import Zext.RuleContext.*
import Zext.Infliction.*

import java.lang.reflect.{Constructor, Modifier}
import scala.collection.mutable.ArrayBuffer
import scala.language.{implicitConversions, postfixOps}
import Condition.*
import Zext.Actions.*
import Zext.Relation.{ManyToMany, OneToMany, relations}
import Zext.Relations.*
import Zext.SetComprehension.AllOf
import Zext.ZextObject.allObjects
import org.apache.commons.lang3.reflect.FieldUtils

import java.io.{IOException, ObjectInputStream, ObjectOutputStream}
import java.lang.management.ManagementFactory
import scala.collection.mutable
import scala.reflect.{ClassTag, TypeTest}



/*
@todo fix the printing of combinations of punctuation such as ... and ?! and "  ."
@todo fix the confusion of arg1 vs noun. Ideally we make noun a relatable proxy instead of a zextobject and then we delete arg1


*/


//@todo this property vs property with value stuff is all a bit weird, i think we need to fix this at some point.
case class PropertyValue[T](property: PropertyWithValue[T], value : T)

trait Property extends Relatable {

    val determining = new MetaAction[Relatable, Relatable, Nothing, Unit, Unit](1, s"normal determining $this") with SystemAction

    //@todo hack
      inflict(determining, Priority(-2)) {
          Break -> noun.getRelatedSetFromDictionaries(property_having).contains(this)
      }
}

trait PropertyWithValue[T] extends Property {

    val values = mutable.HashMap[Relatable, T]()

    def apply(value: T) = {
        PropertyValue(this, value)
    }

    // we used to be able to override the type of determining with this.
    val valueDetermining = new MetaAction[Relatable, Relatable, Nothing, Option[T], Option[T]](1, s"value determining $this") with SystemAction
    valueDetermining.arg = None


    inflict.returns(valueDetermining, Priority(-1)) { set =>
        set does (values(noun) = _)
        values.get(noun)
    }

}




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
    export Zext.Infliction.*
}


object listing extends Action(1) {
    instead(listing, unlisted) {
        stop
    }
}

def ListNamesNicely(stuff: Seq[ZextObject]): Option[String] = {

    val filtered = stuff.filter(n => listing.run.system.noun(n).result)
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
type CT[X] = ClassTag[X]


object ZextObject{

    def Destroy(zextObject: ZextObject) = {
        // unrelate all relations.
        val relations = zextObject.listRelations()
        for(r <- relations)
            zextObject.removeRelation(r)

    }

    val allObjects = ArrayBuffer[ZextObject]()
    val globals = ArrayBuffer[ZextObject]() // always visible and accessible.



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


object determiningAccessibility extends MetaAction[ZextObject, ZextObject, Nothing, AnyAction, Unit](1, "determining accessibility") with SystemAction {

 inflict(determiningAccessibility) {

     // determining if noun is accessible to subject

     val nounLocation = noun match {
         case t: Thing => t.location
         case _ => null
     }

     val secondNounLocation = subject match {
         case t: Thing => t.location
         case _ => null
     }

     // in the same room, or part of their inventory/contents
     if (nounLocation == secondNounLocation || nounLocation == subject || secondNounLocation == noun || noun == subject) {
         continue
     }

     // if noun is composite, check if the composite object is accessible to subject
     noun.get[Thing] does { t =>
        val ctx = RuleContext(determiningAccessibility, subject, Seq(t.compositeObject), silent, location)
        val result = ExecuteAction(ctx).res
        stop_unless(result)
     }

     // if noun is in a container, and that container is open, check if the parent container is accessible to subject
     if (nounLocation != null && nounLocation.open) {
         val ctx = RuleContext(determiningAccessibility, subject, Seq(nounLocation), silent, location)
         val result = ExecuteAction(ctx).res
         stop_unless(result)
     }

     // if not, fail
     fail
 }

}


object determiningVisibility extends MetaAction[ZextObject, ZextObject, Nothing, AnyAction, Unit](1, "determining visibility") with SystemAction{

    // determining if noun is visible to subject
    inflict.returns(determiningVisibility){ forAction =>

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

        val subjectLocation = subject match {
            case t: Thing => t.location
            case _ => null
        }

        val targetLocation = noun match {
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
            stop_unless( subject[Thing].canSee(noun[Thing].compositeObject, forAction) )
        }

        // if noun is in a container, and that container is open or transparent, check if the parent container is visible to subject
        if (targetLocation != null && (targetLocation.open || targetLocation.transparent)) {
            stop_unless( subject[Thing].canSee(targetLocation, forAction) )
        }

        fail
    }

}




implicit object property_having extends Relation[Relatable, Property] with ManyToMany {

    override val precedence = QueryPrecedence.Property

    extension [X <: Source](subject: X)
        infix def is[Y <: Target](target: Y*): X & PendingRelation[SourceT,TargetT,X,Y] = relates(subject, target)

        // enables object is property(whatever) syntax
        infix def is[ValueType](propertyValue: PropertyValue[ValueType]) : X = {

            propertyValue.property.values.update(subject.asInstanceOf[Relatable], propertyValue.value)
            val ret = relates(subject, propertyValue.property)
            ret
        }


    inflict (determining) {
        val result = noun.determining.run.system.noun(subject).execute()
        if(!result.res) fail
        //this might be important, i think this has to do with value determining.
        //if(result.ret == None) fail

        succeed
    }


}

extension (tt: TypeTest[Any, ?]) {
    inline def test(any: Any): Boolean = tt.unapply(any).isDefined
}


object Debugger {
    def attached: Boolean = {
        val jvmArguments = ManagementFactory.getRuntimeMXBean.getInputArguments
        jvmArguments.forEach{ arg =>
            if(arg.contains("-Xdebug") || arg.contains("-Xrunjdwp")) return true
        }
        false
    }
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
        ExecuteReturnAction(printing_name, RuleContext(printing_name, system, Seq(this), false, nowhere))(name.toString).ret
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
        if (this(proper))
            return GetName()

        definiteArticle + " " + GetName()
    }

    def indefinite: String = {
        if (this(proper))
            return GetName()

        indefiniteArticle + " " + GetName()
    }

    def be: String = {
        if (pluralized.isDefined && pluralized.get)
             "are"
        else if(pluralized.isEmpty && this.isInstanceOf[Thing] && this.asInstanceOf[Thing].isAutomaticallyPlural)
             "are"
        else
            "is"
    }

    infix def iz(rhs : String): String = {
        toString + " " + be + " " + rhs
    }

    infix def aka(s: String): this.type = {
        aliases.addOne(s)
        this
    }

    def location : ZContainer = nowhere

    def canSee(other: ZextObject, forAction : AnyAction): Boolean = {
        val ctx = RuleContext(determiningVisibility, this, Seq(other), false, this.location)
        ExecuteReturnAction(determiningVisibility, ctx)(forAction).res
    }

    def canAccess(other: ZextObject, forAction : AnyAction): Boolean = {
        val ctx = RuleContext(determiningAccessibility, this, Seq(other), false, this.location)
        ExecuteReturnAction(determiningAccessibility, ctx)(forAction).res
    }


    override def toString: String = {
        definite
    }




    def isType[T : TT as tt] = tt.test(this)

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
            SimpleThing(desc) is RoomDescription(d)
        }
    }

    def FixName(s: String): String = {
        s.replace('_', ' ')
    }

    enum NounAmount {
        case singular, plural, some
    }



}

object RoomDescription extends PropertyWithValue[StringExpression]
object disturbed extends Property

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
            fail
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
            fail
        }

        d.on = false
    }

    report(turningOff, of[Device]) {
        Say(s"I turned off $noun")
    }

    inflict(switching, of[Device]) {
        val d = noun[Device]
        if (d.on) turningOff.run.noun(d).result
        else turningOn.run.noun(d).result
    }
}

abstract class Device(using ZContainer) extends Thing {

    var on = false

    def off = !on

    var offDesc: StringExpression = null
    var onDesc: StringExpression = null
    val description = s"${if (on) onDesc else offDesc}"
}

