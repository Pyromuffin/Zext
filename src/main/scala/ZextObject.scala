import Direction.{Direction, east, north, opposing, south, west}
import Globals._
import Rule._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.reflect.{ClassTag, classTag}

object world extends container{

    val start = bedRoom
    var playerName = "Zebra"
    var inventory = ArrayBuffer[ZextObject]()

    def main(args: Array[String]): Unit = {
        contents.foreach { i =>
            println(i.indefinite)
        }

        contents.foreach(_ match {
            case r: Room =>
                r.connections.foreach(c => println(c.direction + " of " + r.name + " is " + c.room.name))
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
    implicit def fromString(str: => String)  = {
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
case class initialDescription(desc : StringExpression) extends Property

trait container {
    var contents : ArrayBuffer[ZextObject] = ArrayBuffer[ZextObject]()
}

object Direction extends Enumeration {
    type Direction = Value
    val north, south, east, west = Value


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

class Room extends ZextObject with container {
    world.contents += this
    val connections = ArrayBuffer[Connection]()

    def connect(direction: Direction)(implicit room : Room) = {
        room.connections += Connection(this, direction)
        this.connections += Connection(room, opposing(direction))
    }

}


class ZextObject {
    var definiteArticle : String = "the"
    var indefiniteArticle : String = "a"
    var name : StringExpression = ""
    var description : StringExpression = ""
    var properties : ArrayBuffer[Property] = ArrayBuffer[Property]()
    var plural = false
    var proper = false

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

    override def toString: String = definite
}



class thing extends ZextObject{

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

class person extends thing

class supporter extends thing with container

object Rule {

    class ActionRuleSet {
        val beforeRules = ArrayBuffer[ActionRule]()
        val afterRules = ArrayBuffer[ActionRule]()
        val insteadRules = ArrayBuffer[ActionRule]()
    }


    val ruleSets = new mutable.HashMap[Action, ActionRuleSet]()
    val everyTurnRules = ArrayBuffer[Rule]()

/*
    def before[T : ClassTag](r : Action)(body : => Unit): Unit = {
        ruleSets(r).beforeRules += new ActionRule[T](body)
    }

    def before[T : ClassTag](r : Action, noun : ZextObject)(body : => Unit) : Unit = {
        val rule = new ActionRule[T](body)
        rule.objectCondition = Option(noun)
        ruleSets(r).beforeRules += rule
    }
*/

    def instead(r : Action, conditions: Condition*)(body : => Unit) = {
        val rule = new ActionRule(body)
        ruleSets(r).insteadRules += rule
        rule.conditions = conditions
        rule
    }

    def instead(r : Action, noun : ZextObject, conditions: Condition*)(body : => Unit)  = {
        val rule = new ActionRule(body)
        rule.objectCondition = Option(noun)
        rule.conditions = conditions
        ruleSets(r).insteadRules += rule
        rule
    }


    def instead(r : Action, typeCondition : Class[_], conditions: Condition* )(body : => Unit)  = {
        val rule = new ActionRule(body)
        rule.typeCondition = typeCondition
        rule.conditions = conditions
        rule.objectCondition = Option(noun)
        ruleSets(r).insteadRules += rule
        rule
    }




    var ruleResult = true

    def Fail() = {
        ruleResult = false
    }

    // we will likely need some kind of overload resolution here.
    // the inform book says they sort by specificity, in our case, we are applying ALL possible rules.

    def ResolveOverloads() ={

    }

    def execute(rule: Action, target: ZextObject): Unit = {
        val set = ruleSets(rule)
        noun = target
        ruleResult = true



        set.beforeRules.foreach { r =>
            if(r.possible && ruleResult) {
                r.execute
            }
        }

        set.insteadRules.foreach { r =>
            if(r.possible && ruleResult) {
                ruleResult = false
                r.execute
            }
        }

        if(ruleResult){
            if(target == null) {
                ruleResult = rule.executeNone()
            } else {
                ruleResult = rule.executeOne(target)
            }
        }

        set.afterRules.foreach { r =>
            if(r.possible && ruleResult) {
                r.execute
                ruleResult = false
            }
        }


    }

}

abstract class Rule {
    val disabled = false
}

object Condition{
    implicit def fromBool(bool: => Boolean)  = {
        new Condition(bool)
    }
}


class Condition( condition : => Boolean ) {
    def evaluate = condition
}

class ActionRule(body : => Unit) extends Rule{

    var typeCondition : Class[_] = classOf[ZextObject]
    var objectCondition : Option[ZextObject] = None
    var conditions = Seq[Condition]()
    val depth = typeDepth

    def typeDepth : Int = {
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

    def precedence = {
        var ret = 0

        if(typeCondition != classOf[Nothing])
            ret = 1

        if (objectCondition.isDefined)
            ret = 2;

        ret
    }


    def possible = {
        var success = true

        if (objectCondition.isDefined) {
            success = success && (objectCondition.get == noun)
        }
        else if(typeCondition != classOf[Nothing]){
            val nounClass = noun.getClass

        }

        conditions.foreach { c =>
            success = success && c.evaluate
        }

        success
    }

    def execute = this.body
}


abstract class Action(verb : String*) extends Rule {
    def executeNone() : Boolean = false
    def executeOne(noun : ZextObject) : Boolean = false
    def execute(nouns : ZextObject*) : Boolean = false
}



object Globals {
    var location : Room = null
    var noun : ZextObject = null

    /*
    object the {
        def noun = { SpecialStrings.noun.definite }
    }

    object a {
        def noun = { SpecialStrings.noun.indefinite }
    }
    */

    object is {
        override def toString: String = Globals.noun.be
    }
}
import Globals._

object taking extends Action("take", "get") {

    ruleSets(this) = new ActionRuleSet

    override def executeOne(noun : ZextObject) : Boolean = {
        if(noun ? fixed){
            Say(s"$noun $is stuck.")
            return false
        }

        Say(s"I took $noun.")
        true
    }
}



object examining extends Action("examine", "x", "look" ) {

    ruleSets(this) = new ActionRuleSet

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
    implicit val room = this

    name = "bedroom"
    description = "The loft of the apartment is large enough for a bed and a desk. You have neither."
    val rock = new thing a "rock" is fixed desc "a fragment of the old mountain" has initialDescription(s"It's lodged in the floor. $playerName put it here")
    val bob = new person named "Bob" desc "Bob is here for some reason."
    new supporter the "table" is fixed desc "a plywood mishap held up on the suggestion of four legs."
    val chairs = new thing some "chairs" are scenery and fixed desc "they're bolted to the floor."
    val sheet = new thing a "sheet of paper" desc "incomprehensible scribbles litter the surface."


    val a = instead(taking, location == bathroom) {
        Say("I shouldn't take anything in the bathroom.")
    }

    val b = instead(taking, sheet){
        Say("I've seen enough.")
    }


    val c = instead(taking, bob) {
        Say(s"I would think twice.")
    }

    instead(taking, classOf[person]) {
        Say(s"I should probably ask first.")
    }


    val e = instead(taking, classOf[thing]) {
        Say(s"taking of things is forbidden.")
    }


    bathroom connect west


    def main(args: Array[String]): Unit = {
        world.playerName = args(0)


        connections.foreach( c => println(c.direction.toString + " is " + c.room.toString) )
        location = bedRoom
        execute(examining, null)
        execute(examining, rock)
        execute(taking, sheet)
        execute(taking, rock)
        execute(taking, bob)
        location = bathroom
        execute(examining, null)
        execute(taking, bob)

    }
}


object bathroom extends Room {
    implicit val room = this
    name = "bathroom"

}