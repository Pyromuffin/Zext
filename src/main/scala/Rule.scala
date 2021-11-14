package Zext

import Interpreter.*
import World.*
import Rule.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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
    val everyTurnRules = ArrayBuffer[Rule]()


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

    def report(r: Action, conditions: Condition*)(body: => Unit): ActionRule = {
        val rule = new ActionRule( {body; true}, conditions* )
        ruleSets(r).reportRules += rule
        rule
    }


    def report[T](r: Action, conditions: Condition*)(body: T => Unit)(using tag : ClassTag[T]): ActionRule = {
        val rule = new ActionRule( { body(noun.asInstanceOf[T]); true }, (conditions :+ tag.runtimeClass)* )
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


    def carryOut(r: Action, conditions: Condition*)(body: => Boolean) : ActionRule = {
        val rule = new ActionRule(body, conditions*)
        ruleSets(r).executeRules += rule
        rule
    }

    def carryOut[T](r: Action, conditions: Condition*)(body: T => Boolean)(using tag : ClassTag[T]): ActionRule = {
        val rule = new ActionRule( { body(noun.asInstanceOf[T]) }, (conditions :+ tag.runtimeClass)* )
        ruleSets(r).executeRules += rule
        rule
    }

    def carryOut[T1, T2](r: Action, conditions: Condition*)(body: (T1, T2) => Boolean) : ActionRule = {
        val rule = new ActionRule( { body(noun.asInstanceOf[T1], secondNoun.asInstanceOf[T2]) }, conditions* )
        ruleSets(r).executeRules += rule
        rule
    }


    def ResolveOverloads(rules: ArrayBuffer[ActionRule], targets : Int): Option[ActionRule] = {
        val possible = rules.filter(_.targets == targets).filter(_.possible)
        if (possible.isEmpty)
            return Option.empty

        val maxPrecedence = possible.map(_.precedence).max
        val maxPrecedenceRules = possible.filter(_.precedence == maxPrecedence)
        val rule = maxPrecedenceRules.maxBy(_.specificity)
        Option(rule)
    }


    def execute(rule: Action, target: Option[ZextObject] = None): Boolean = {
        val set = ruleSets(rule)
        var targets = 0

        if (target.isDefined) {
            noun = target.get
            targets = 1
        } else {
            noun = null
        }

        val beforeRule = ResolveOverloads(set.beforeRules, targets)
        if (beforeRule.isDefined) {
            if (!beforeRule.get.evaluate)
                return false
        }

        val insteadRule = ResolveOverloads(set.insteadRules, targets)
        if (insteadRule.isDefined) {
            if (!insteadRule.get.evaluate)
                return false
        }

        val carryOutRule = ResolveOverloads(set.executeRules, targets)
        if (carryOutRule.isDefined) {
            if (!carryOutRule.get.evaluate)
                return false
        }

        val afterRule = ResolveOverloads(set.afterRules, targets)
        if (afterRule.isDefined) {
            if (!afterRule.get.evaluate)
                return false
        }

        val reportRule = ResolveOverloads(set.reportRules, targets)
        if(reportRule.isDefined){
            reportRule.get.evaluate
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

class ActionRule(body : => Boolean, conditions : Condition*) extends Rule{

    var targets = 0

    for(c <- conditions){
        if(c.queryType == Query.Object || c.queryType == Query.Property || c.queryType == Query.Class) {
            targets = 1
        }
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

