package Zext

import Globals.*
import Interpreter.*
import World.*
import Rule.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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
            instead(r, conditions*)( Say(s) )
        }
    }


    def before(r : Action, conditions: Condition*)(body : => Boolean) : ActionRule  = {
        val rule = new ActionRule(body)
        rule.conditions = conditions
        ruleSets(r).beforeRules += rule
        rule
    }

    def before(r : Action, conditions: Condition*)(body : => Unit) (implicit dummyImplicit: DummyImplicit) : ActionRule  = {
        before(r, conditions*){ body; true }
    }


    def instead(r : Action, conditions: Condition*)(body : => Boolean) : ActionRule  = {
        val rule = new ActionRule(body)
        rule.conditions = conditions
        ruleSets(r).insteadRules += rule
        rule
    }

    def instead(r : Action, conditions: Condition*)(body : => Unit) (implicit dummyImplicit: DummyImplicit) : ActionRule  = {
        instead(r, conditions*){ body; false }
    }

    def instead(r : Action, conditions: Condition*) : Consequence  = {
        new Consequence(r, conditions*)
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


abstract class Action(val verb : String*) extends Rule {
    def executeNone() : Boolean = false
    def executeOne(noun : ZextObject) : Boolean = false
    def executeTwo(first : ZextObject, second : ZextObject) : Boolean = false

    def Register() ={
        ruleSets(this) = new ActionRuleSet
    }
}

