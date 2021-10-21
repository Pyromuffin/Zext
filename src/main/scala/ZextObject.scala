import Globals.*
import Query.Property
import Rule.*
import Direction.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.{implicitConversions, postfixOps}

given container : Container = world

object world extends Container{

    val start = bedRoom
    var playerName = "Zebra"
    var inventory = ArrayBuffer[ZextObject]()
    var location : Room = null


    def main(args: Array[String]): Unit = {
        contents.foreach { i =>
            println(i.indefinite)
        }

        contents.foreach(_ match {
            case r: Room =>
                r.connections.foreach(c => println(c.direction.toString + " of " + r.name + " is " + c.room.name))
        })
    }

}
import world._

object Interpreter{
    def Say(str: StringExpression): Unit = {
        println(str.toString.capitalize)
    }
}
import Interpreter._

trait Property {

}


object StringExpression{
    implicit def fromString(str : => String): StringExpression = {
        new StringExpression(str)
    }
}



class StringExpression(lazyStr : => String) {
    override def toString: String = {
        lazyStr
    }
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
}


case class Connection(room: Room, direction : Direction)

class Room(using container : Container) extends ZextObject with Container {
    given currentContainer : Container = this
    given room : Room = this

    val connections = ArrayBuffer[Connection]()

    def connect(direction: Direction)(implicit room : Room) = {
        room.connections += Connection(this, direction)
        this.connections += Connection(room, Direction.opposing(direction))
    }

}


class ZextObject(using container : Container) {
    var definiteArticle : String = "the"
    var indefiniteArticle : String = "a"
    var name : StringExpression = ""
    var description : StringExpression = ""
    var properties : ArrayBuffer[Property] = ArrayBuffer[Property]()
    var plural = false
    var proper = false

    var parentContainer : Container = container
    container.contents += this

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

object Rule {

    class ActionRuleSet {
        val beforeRules = ArrayBuffer[ActionRule]()
        val afterRules = ArrayBuffer[ActionRule]()
        val insteadRules = ArrayBuffer[ActionRule]()
    }

    val ruleSets = new mutable.HashMap[Action, ActionRuleSet]()
    val everyTurnRules = ArrayBuffer[Rule]()


    class Consequence(r : Action, conditions: Condition*) {
        def say(s : StringExpression): ActionRule = {
            instead(r, conditions:_*)( Say(s) )
        }
    }


    def before(r : Action, conditions: Condition*)(body : => Boolean) : ActionRule  = {
        val rule = new ActionRule(body)
        rule.conditions = conditions
        ruleSets(r).beforeRules += rule
        rule
    }

    def before(r : Action, conditions: Condition*)(body : => Unit) (implicit dummyImplicit: DummyImplicit) : ActionRule  = {
        before(r, conditions:_*){ body; true }
    }


    def instead(r : Action, conditions: Condition*)(body : => Boolean) : ActionRule  = {
        val rule = new ActionRule(body)
        rule.conditions = conditions
        ruleSets(r).insteadRules += rule
        rule
    }

    def instead(r : Action, conditions: Condition*)(body : => Unit) (implicit dummyImplicit: DummyImplicit) : ActionRule  = {
        instead(r, conditions:_*){ body; false }
    }

    def instead(r : Action, conditions: Condition*) : Consequence  = {
        new Consequence(r, conditions:_*)
    }



    def ResolveOverloads(rules : ArrayBuffer[ActionRule]): Option[ActionRule]  ={
        val possible = rules.filter( _.possible )
        if(possible.isEmpty)
            return Option.empty

        val maxPrecedence = possible.map(_.precedence).max
        val maxPrecedenceRules = possible.filter(_.precedence == maxPrecedence)
        val rule = maxPrecedenceRules.maxBy(_.specificity)
        Option(rule)
    }

    def execute(rule: Action, target: ZextObject): Unit = {
        val set = ruleSets(rule)
        noun = target

        val beforeRule = ResolveOverloads(set.beforeRules)
        if(beforeRule.isDefined){
            if(!beforeRule.get.evaluate)
                return
        }

        val insteadRule = ResolveOverloads(set.insteadRules)
        if(insteadRule.isDefined){
            if(!insteadRule.get.evaluate)
                return
        }

        if(target == null) {
            if(!rule.executeNone())
                return
        } else {
            if(!rule.executeOne(target.asInstanceOf[thing]))
                return
        }

        val afterRule = ResolveOverloads(set.afterRules)
        if(afterRule.isDefined){
            if(!afterRule.get.evaluate)
                return
        }
    }

}

abstract class Rule {
    val disabled = false
}

enum Query:
    case Generic, Class, Property, Object, Location

object Condition{
    // inform's precedence is something like
    // location > object > property > class > generic

    def CalculateTypeDepth(typeCondition : Class[_]) : Int = {
        var depth = 1
        val top = classOf[ZextObject]
        if(typeCondition == classOf[Nothing] || typeCondition == classOf[ZextObject])
            return 0

        var superClass = typeCondition.getSuperclass

        while(superClass != top){
            depth += 1
            superClass = superClass.getSuperclass
        }

        depth
    }

    implicit def fromBoolean(b : => Boolean) : Condition = new Condition(b, Query.Generic)
    implicit def fromObject(z : => ZextObject) : Condition = new Condition(z == noun, Query.Object)
    implicit def fromProperty(p : => Property) : Condition = new Condition(noun.properties.contains(p), Query.Property)
    implicit def fromLocation(r : => Room) : Condition = new Condition(r == location, Query.Location)
    implicit def fromClass(c : => Class[_]) : Condition = {
        val condition = new Condition(c.isAssignableFrom(noun.getClass), Query.Class)
        condition.specificity = CalculateTypeDepth(c)
        condition
    }
}



class Condition( condition : => Boolean, val queryType: Query ) {
    def evaluate = condition
    var specificity = 1

    def precedence = {
        queryType.ordinal
    }
}

class ActionRule(body : => Boolean) extends Rule{

    var conditions = Seq[Condition]()

    def specificity = {
        conditions.map( _.specificity ).sum
    }

    def precedence ={
        conditions.map(_.precedence).max
    }

    def possible = {
        conditions.forall( _.evaluate )
    }

    def evaluate = this.body
}


abstract class Action(verb : String*) extends Rule {
    def executeNone() : Boolean = false
    def executeOne(noun : ZextObject) : Boolean = false
    def executeTwo(first : ZextObject, second : ZextObject) : Boolean = false

    ruleSets(this) = new ActionRuleSet
}



object Globals {
    var noun : ZextObject = null

    object is {
        override def toString: String = Globals.noun.be
    }
}

object taking extends Action("take", "get") {

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
        Say(location.name)
        Say(location.description)
        true
    }

    override def executeOne(noun : ZextObject) : Boolean = {
        Say(noun.description)
        true
    }
}

object bedRoom extends Room {

    name = "bedroom"
    description = "The loft of the apartment is large enough for a bed and a desk. You have neither."
    val rock = new thing a "rock" desc "a fragment of the old mountain" has initialDescription(s"It's lodged in the floor. $playerName put it here")
    // val bob = new person named "Bob" desc "Bob is here for some reason."
    val table = new Supporter the "table" is fixed desc "a plywood mishap held up on the suggestion of four legs."
    val box = new Supporter the "box" desc "it's full of garbage."
    val chairs = new thing some "chairs" are scenery and fixed desc "A committee of seats"
    val sheet = new thing a "sheet of paper" is wet desc "incomprehensible scribbles litter the surface."
    var time = 10

    before(taking, rock) {
        Say(s"I might make $playerName mad.")
    }

    before(examining, rock){
        val init = noun.properties(0).asInstanceOf[initialDescription]
        Say(init.desc)
    }

    instead(taking, bathroom) {
        Say("I shouldn't take anything in the bathroom.")
    }


    instead(taking, bathroom, wet) say s"I might slip! The current time is $time."
    instead(taking, time == 10) say "It's too early for taking."
    instead(taking, classOf[Container]) say "It's too heavy."


    instead(taking, sheet){
        Say("I've seen enough.")
    }


    instead(taking, fixed) {
        Say(s"$noun $is stuck.")
    }

    instead(taking, chairs){
        Say("they're bolted to the floor.")
    }

    bathroom connect west


    def main(args: Array[String]): Unit = {
        location = bedRoom
        world.playerName = args(0)
        execute(taking, rock)
        execute(taking, box)
        time = 5
        execute(taking, rock)
        playerName = "Potato"
        execute(examining, rock)
        execute(taking, chairs)
        execute(taking, table)
        execute(taking, sheet)
        location = bathroom
        execute(taking, sheet)
        execute(taking, chairs)
        execute(taking, table)
        time = 7
        execute(taking, sheet)

    }
}


object bathroom extends Room {
    name = "bathroom"

}