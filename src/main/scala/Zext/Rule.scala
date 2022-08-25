package Zext

import Zext.Interpreter.*
import Zext.Macros.depth
import Zext.Parser.*
import Zext.Rule.*
import Zext.World.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.quoted.*
import scala.reflect.{ClassTag, TypeTest}

object Rule {

    class ActionRuleSet {
        val beforeRules = ArrayBuffer[ActionRule]()
        val afterRules = ArrayBuffer[ActionRule]()
        val executeRules = ArrayBuffer[ActionRule]()
        val insteadRules = ArrayBuffer[ActionRule]()
        val reportRules = ArrayBuffer[ActionRule]()
    }

    val ruleSets = new mutable.HashMap[Action, ActionRuleSet]()
    val everyTurnRules = ArrayBuffer[PersistingRule]()


    class Consequence(r: Action, conditions: Condition*) {
        def say(s: StringExpression): ActionRule = {
            instead(r, conditions *)(Say(s))
        }
    }


    def before(r: Action, conditions: Condition*)(body: => Boolean): ActionRule = {
        val rule = new ActionRule(body, conditions*)
        ruleSets(r).beforeRules += rule
        rule
    }

    def before(r: Action, conditions: Condition*)(body: => Unit)(implicit dummyImplicit: DummyImplicit): ActionRule = {
        before(r, conditions *) {
            body; true
        }
    }

    inline def before[T <: ZextObject](r: Action, conditions: Condition*)(body: T => Boolean): ActionRule = {
        val condition = Condition.fromClass[T](QueryPrecedence.Class)
        val rule = new ActionRule({
            body(noun.asInstanceOf[T])
        }, (conditions :+ condition) *)
        ruleSets(r).beforeRules += rule
        rule
    }


    def instead(r: Action, conditions: Condition*)(body: => Boolean): ActionRule = {
        val rule = new ActionRule(body, conditions*)
        ruleSets(r).insteadRules += rule
        rule
    }

    def instead(r: Action, conditions: Condition*)(body: => Unit)(implicit dummyImplicit: DummyImplicit): ActionRule = {
        instead(r, conditions *) {
            body; false
        }
    }

    inline def instead[T <: ZextObject](r: Action, conditions: Condition*)(body: T => Unit)(using tag : ClassTag[T]): ActionRule = {
        val rule = new ActionRule( { body(noun.asInstanceOf[T]); false }, (conditions :+ Condition.fromClass[T](QueryPrecedence.Class))* )
        ruleSets(r).insteadRules += rule
        rule
    }

    def report(r: Action, conditions: Condition*)(body: => Unit): ActionRule = {
        val rule = new ActionRule( {body; true}, conditions* )
        ruleSets(r).reportRules += rule
        rule
    }

    inline def report[T <: ZextObject](r: Action, conditions: Condition*)(body: T => Unit)(using tag : ClassTag[T]): ActionRule = {
        val rule = new ActionRule( { body(noun.asInstanceOf[T]); true }, (conditions :+ Condition.fromClass[T](QueryPrecedence.Class))* )
        ruleSets(r).reportRules += rule
        rule
    }



    def instead(r: Action, conditions: Condition*): Consequence = {
        new Consequence(r, conditions *)
    }


    def after(r: Action, conditions: Condition*)(body: => Boolean): ActionRule = {
        val rule = new ActionRule(body, conditions*)
        ruleSets(r).afterRules += rule
        rule
    }

    def after(r: Action, conditions: Condition*)(body: => Unit)(implicit dummyImplicit: DummyImplicit): ActionRule = {
        after(r, conditions *) {
            body; true
        }
    }

    inline def after[T <: ZextObject](r: Action, conditions: Condition*)(body: T => Unit)(using tag: ClassTag[T]): ActionRule = {
        val rule = new ActionRule( {body(noun.asInstanceOf[T]); true}, (conditions :+ Condition.fromClass[T](QueryPrecedence.Class)) *)
        ruleSets(r).afterRules += rule
        rule
    }

    def inflict(r: Action, conditions: Condition*)(body: => Boolean) : ActionRule = {
        val rule = new ActionRule(body, conditions*)
        ruleSets(r).executeRules += rule
        rule
    }

    inline def inflict[T <: ZextObject](r: Action, conditions: Condition*)(body: T => Boolean): ActionRule = {
        val condition = Condition.fromClass[T](QueryPrecedence.Class)
        val rule = new ActionRule( { body(noun.asInstanceOf[T]) }, (conditions :+ condition)* )
        ruleSets(r).executeRules += rule
        rule
    }

    def inflict[T1 <: ZextObject, T2 <: ZextObject](r: Action, conditions: Condition*)(body: (T1, T2) => Boolean)(using TypeTest[ZextObject, T1], TypeTest[ZextObject, T2]): ActionRule = {
        val firstCondition = Condition.fromClass[T1](QueryPrecedence.Class)
        val secondCondition = Condition.fromClass[T2](QueryPrecedence.SecondClass)
        val addedConditions = conditions :+ firstCondition :+ secondCondition

        val rule = new ActionRule( { body(noun.asInstanceOf[T1], secondNoun.asInstanceOf[T2]) }, addedConditions* )
        ruleSets(r).executeRules += rule
        rule
    }

    def report[T1 <: ZextObject, T2 <: ZextObject](r: Action, conditions: Condition*)(body: (T1, T2) => Unit) (using TypeTest[ZextObject, T1], TypeTest[ZextObject, T2]): ActionRule = {
        val firstCondition = Condition.fromClass[T1](QueryPrecedence.Class)
        val secondCondition = Condition.fromClass[T2](QueryPrecedence.SecondClass)
        val addedConditions = conditions :+ firstCondition :+ secondCondition

        val rule = new ActionRule( { body(noun.asInstanceOf[T1], secondNoun.asInstanceOf[T2]); true }, addedConditions* )
        ruleSets(r).reportRules += rule
        rule
    }

    def ResolveOverloads(possible: ArrayBuffer[ActionRule], targets : Int): Option[ActionRule] = {
        if (possible.isEmpty)
            return Option.empty

        if(possible.length == 1)
            return possible.headOption

        val maxPrecedence = possible.map(_.precedence).max
        val maxPrecedenceRules = possible.filter(_.precedence == maxPrecedence)
        val rule = maxPrecedenceRules.maxBy(_.specificity)
        Option(rule)
    }


    def execute(rule: Action, target: Option[ZextObject] = None, target2: Option[ZextObject] = None): Boolean = {
        val set = ruleSets(rule)
        var targets = 0

        var stackNoun : ZextObject = null
        var stackNoun2 : ZextObject = null

        // pain
        if (target.isDefined) {
            stackNoun = target.get
            if(!stackNoun.isVisible(location)){
                //@todo maybe make this better
                Say(s"I can't see ${stackNoun.definite}")
                return false
            }

            targets += 1
        } else {
            stackNoun = null
        }

        if (target2.isDefined) {
           stackNoun2 = target2.get
            if(!stackNoun2.isVisible(location)){
                //@todo maybe make this better
                Say(s"I can't see ${stackNoun2.definite}")
                return false
            }

            targets += 1
        } else {
            stackNoun2 = null
        }



        def RunRule(noun1 : ZextObject, noun2 : ZextObject, rules : ArrayBuffer[ActionRule], targetCount : Int) : Boolean = {
            noun = noun1
            secondNoun = noun2
            val success = ResolveOverloads(rules, targets).forall(_.exec)
            success
        }

        noun = stackNoun
        secondNoun = stackNoun2
        // we need to evaluate any "previously" rules here... actually we want to run them before the **first** rule executes in any execution chain.
        // this will be difficult or impossible to do without serializing the game state, which we might want to do at some point for save games, but i am not going to worry about that just yet.
        val beforeRules = set.beforeRules.filter(r => r.targets == targets || r.generic).filter(r => r.previouslyPossible || r.possible )
        val previouslyInsteadPossible = set.insteadRules.filter(r => r.targets == targets || r.generic).filter(_.previouslyPossible)
        val previouslyPossibleExecute = set.executeRules.filter(r => r.targets == targets || r.generic).filter(_.previouslyPossible)
        val previouslyPossibleReport = set.reportRules.filter(r => r.targets == targets || r.generic).filter(_.previouslyPossible)
        val previouslyPossibleAfter = set.afterRules.filter(r => r.targets == targets || r.generic).filter(_.previouslyPossible)

        if(!RunRule(stackNoun, stackNoun2, beforeRules, targets)) return false

        noun = stackNoun
        secondNoun = stackNoun2
        val insteadPossible = set.insteadRules.filter(r => r.targets == targets || r.generic).filter(_.possible)
        if(!RunRule(stackNoun, stackNoun2, previouslyInsteadPossible concat insteadPossible , targets)) return false

        noun = stackNoun
        secondNoun = stackNoun2
        val executePossible = set.executeRules.filter(r => r.targets == targets || r.generic).filter(_.possible)
        if(!RunRule(stackNoun, stackNoun2, previouslyPossibleExecute concat executePossible, targets)) return false

        noun = stackNoun
        secondNoun = stackNoun2
        val reportPossible = set.reportRules.filter(r => r.targets == targets || r.generic).filter(_.possible)
        if(!RunRule(stackNoun, stackNoun2, previouslyPossibleReport concat reportPossible, targets)) return false

        noun = stackNoun
        secondNoun = stackNoun2
        val afterPossible = set.afterRules.filter(r => r.targets == targets || r.generic).filter(_.possible)
        if(!RunRule(stackNoun, stackNoun2, previouslyPossibleAfter concat afterPossible, targets)) return false


        true
    }

    def execute(rule: Action, target: ZextObject): Boolean = {
        execute(rule, Some(target))
    }
}

abstract class Rule {
    val disabled = false

}


object PersistingRule {
    implicit def fromUnit(body : => Unit) : PersistingRule = PersistingRule(body)
}

class PersistingRule( body : => Unit) extends Rule {
    def Execute() = body
}

enum QueryPrecedence:
    case Generic, Class, SecondClass, Property, Object, Containment, Location

object Condition{
    // inform's precedence is something like
    // location > object > property > class > generic


    implicit def fromBoolean(b : => Boolean) : Condition = new Condition(b, QueryPrecedence.Generic)
    implicit def fromObject(z : => ZextObject) : Condition = new Condition(z == noun, QueryPrecedence.Object)
    implicit def fromObjectArray(az : => Seq[ZextObject]) : Condition = new Condition( az.contains(noun), QueryPrecedence.Object)
    implicit def fromProperty(p : => Property) : Condition = new Condition(noun.properties.contains(p), QueryPrecedence.Property)
    implicit def fromLocation(r : => Room) : Condition = new Condition(r == location, QueryPrecedence.Location)
    inline def fromClass[T](queryType : QueryPrecedence = QueryPrecedence.Class)(using TypeTest[ZextObject, T]): Condition = {
        val condition = new Condition(
            {
                val target = if(queryType == QueryPrecedence.Class) noun else secondNoun
                val success = canBecome[ZextObject, T](target)
                //println(s"$target sucess: $success")
                success
            }
            , queryType)
        condition.specificity = depth[T]
        condition
    }

    def become[X, Y](x: X)(using tt: TypeTest[X, Y]): Option[Y] = x match
        case tt(x) => Some(x)
        case _ => None

    def canBecome[X, Y](x: X)(using tt: TypeTest[X, Y]): Boolean = {
        become[X,Y](x).isDefined
    }
}



class Condition( condition : => Boolean, val queryType: QueryPrecedence, val previously : Boolean = false ) {
    def evaluate = condition

    var specificity = 1

    def precedence = {
        queryType.ordinal
    }
}

class ActionRule(body : => Boolean, conditions : Condition*) extends Rule{

    var targets = 0
    var generic = false

    for(c <- conditions){
        if(c.queryType == QueryPrecedence.Object || c.queryType == QueryPrecedence.Property || c.queryType == QueryPrecedence.Class) {
            targets = 1
        }

        // maybe wrong
        if(c.queryType == QueryPrecedence.Generic)
            generic = true
    }

    if(conditions.exists(_.queryType == QueryPrecedence.SecondClass)){
        targets = 2
    }

    def specificity = {
        conditions.map( _.specificity ).sum
    }

    def precedence ={
        conditions.map(_.precedence).foldLeft(0)( _ max _ )
    }

    def previouslyPossible = {
        val previousConditions = conditions.filter(_.previously)
        if(previousConditions.isEmpty)
           false
        else
          previousConditions.forall( _.evaluate )
    }

    def possible = {
        conditions.filterNot(_.previously).forall( _.evaluate )
    }

    def exec = body
}





class Action(val verbs : String*) extends Rule with ParsableType(PartOfSpeech.verb) {

    Understand(this, verbs*)

    ruleSets(this) = new ActionRuleSet
    override def toString = verbs(0)
}

