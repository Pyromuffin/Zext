package Zext

import Zext.Interpreter.*
import Zext.Macros.depth
import Zext.Parser.Command
import Zext.Rule.*
import Zext.World.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.reflect.ClassTag

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
        val rule = new ActionRule( { body(noun.asInstanceOf[T]); false }, (conditions :+ Condition.fromClass[T](Query.Class))* )
        ruleSets(r).insteadRules += rule
        rule
    }

    def report(r: Action, conditions: Condition*)(body: => Unit): ActionRule = {
        val rule = new ActionRule( {body; true}, conditions* )
        ruleSets(r).reportRules += rule
        rule
    }

    inline def report[T <: ZextObject](r: Action, conditions: Condition*)(body: T => Unit)(using tag : ClassTag[T]): ActionRule = {
        val rule = new ActionRule( { body(noun.asInstanceOf[T]); true }, (conditions :+ Condition.fromClass[T](Query.Class))* )
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
        val rule = new ActionRule( {body(noun.asInstanceOf[T]); true}, (conditions :+ Condition.fromClass[T](Query.Class)) *)
        ruleSets(r).afterRules += rule
        rule
    }

    def inflict(r: Action, conditions: Condition*)(body: => Boolean) : ActionRule = {
        val rule = new ActionRule(body, conditions*)
        ruleSets(r).executeRules += rule
        rule
    }

    inline def inflict[T <: ZextObject](r: Action, conditions: Condition*)(body: T => Boolean)(using tag : ClassTag[T]): ActionRule = {
        val rule = new ActionRule( { body(noun.asInstanceOf[T]) }, (conditions :+ Condition.fromClass[T](Query.Class))* )
        ruleSets(r).executeRules += rule
        rule
    }

    def inflict[T1 <: ZextObject, T2 <: ZextObject](r: Action, conditions: Condition*)(body: (T1, T2) => Boolean) (using ClassTag[T1], ClassTag[T2] ): ActionRule = {
        val firstCondition = Condition.fromClass[T1](Query.Class)
        val secondCondition = Condition.fromClass[T2](Query.SecondClass)
        val addedConditions = conditions :+ firstCondition :+ secondCondition

        val rule = new ActionRule( { body(noun.asInstanceOf[T1], secondNoun.asInstanceOf[T2]) }, addedConditions* )
        ruleSets(r).executeRules += rule
        rule
    }

    def report[T1 <: ZextObject, T2 <: ZextObject](r: Action, conditions: Condition*)(body: (T1, T2) => Unit) (using ClassTag[T1], ClassTag[T2] ): ActionRule = {
        val firstCondition = Condition.fromClass[T1](Query.Class)
        val secondCondition = Condition.fromClass[T2](Query.SecondClass)
        val addedConditions = conditions :+ firstCondition :+ secondCondition

        val rule = new ActionRule( { body(noun.asInstanceOf[T1], secondNoun.asInstanceOf[T2]); true }, addedConditions* )
        ruleSets(r).reportRules += rule
        rule
    }

    def ResolveOverloads(rules: ArrayBuffer[ActionRule], targets : Int): Option[ActionRule] = {
        val possible = rules.filter(r => r.targets == targets || r.generic).filter(_.possible)
        if (possible.isEmpty)
            return Option.empty

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


        // this is pretty jank.
        noun = stackNoun
        secondNoun = stackNoun2
        val beforeRule = ResolveOverloads(set.beforeRules, targets)
        if (beforeRule.isDefined) {
            if (!beforeRule.get.evaluate)
                return false
        }

        noun = stackNoun
        secondNoun = stackNoun2
        val insteadRule = ResolveOverloads(set.insteadRules, targets)
        if (insteadRule.isDefined) {
            if (!insteadRule.get.evaluate)
                return false
        }

        noun = stackNoun
        secondNoun = stackNoun2
        val carryOutRule = ResolveOverloads(set.executeRules, targets)
        if (carryOutRule.isDefined) {
            if (!carryOutRule.get.evaluate)
                return false
        }

        noun = stackNoun
        secondNoun = stackNoun2
        val reportRule = ResolveOverloads(set.reportRules, targets)
        if (reportRule.isDefined) {
            reportRule.get.evaluate
        }

        noun = stackNoun
        secondNoun = stackNoun2
        val afterRule = ResolveOverloads(set.afterRules, targets)
        if (afterRule.isDefined) {
            if (!afterRule.get.evaluate)
                return false
        }

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

enum Query:
    case Generic, Class, SecondClass, Property, Object, Location

object Condition{
    // inform's precedence is something like
    // location > object > property > class > generic

    implicit def fromBoolean(b : => Boolean) : Condition = new Condition(b, Query.Generic)
    implicit def fromObject(z : => ZextObject) : Condition = new Condition(z == noun, Query.Object)
    implicit def fromObjectArray(az : => Seq[ZextObject]) : Condition = new Condition( az.contains(noun), Query.Object)
    implicit def fromProperty(p : => Property) : Condition = new Condition(noun.properties.contains(p), Query.Property)
    implicit def fromLocation(r : => Room) : Condition = new Condition(r == location, Query.Location)
    inline def fromClass[T <: ZextObject](queryType : Query = Query.Class)(implicit classTag: ClassTag[T]) : Condition = {
        val condition = new Condition(classTag.runtimeClass.isAssignableFrom(noun.getClass), queryType)
        condition.specificity = depth[T]
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

class ActionRule(body : => Boolean, conditions : Condition*) extends Rule{

    var targets = 0
    var generic = false

    for(c <- conditions){
        if(c.queryType == Query.Object || c.queryType == Query.Property || c.queryType == Query.Class) {
            targets = 1
        }

        // maybe wrong
        if(c.queryType == Query.Generic)
            generic = true
    }

    if(conditions.exists(_.queryType == Query.SecondClass)){
        targets = 2
    }

    def specificity = {
        conditions.map( _.specificity ).sum
    }

    def precedence ={
        conditions.map(_.precedence).foldLeft(0)( _ max _ )
    }

    def possible = {
        conditions.forall( _.evaluate )
    }

    def evaluate = body
}





class Action(val verb : String*) extends Rule {

    ruleSets(this) = new ActionRuleSet
    override def toString = verb(0)
}

