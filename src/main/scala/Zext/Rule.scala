package Zext

import Zext.Interpreter.*
import Zext.Macros.{CodePosition, depth}
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
        rule.definitionPosition = CodePosition()
        println(rule.definitionPosition)


        ruleSets(r).insteadRules += rule
        rule
    }

    inline def instead[T1 <: ZextObject, T2 <: ZextObject](r: Action, conditions: Condition*)(body: (T1, T2) => Unit)(using TypeTest[ZextObject, T1], TypeTest[ZextObject, T2]): ActionRule = {
        val firstCondition = Condition.fromClass[T1](QueryPrecedence.Class)
        val secondCondition = Condition.fromClass[T2](QueryPrecedence.SecondClass)
        val addedConditions = conditions :+ firstCondition :+ secondCondition

        val rule = new ActionRule( { body(noun.asInstanceOf[T1], secondNoun.asInstanceOf[T2]); false}, addedConditions* )
        rule.definitionPosition = CodePosition()
        ruleSets(r).insteadRules += rule
        rule
    }

    inline def report(r: Action, conditions: Condition*)(body: => Unit): ActionRule = {
        val rule = new ActionRule( {body; true}, conditions* )
        rule.definitionPosition = CodePosition()
        ruleSets(r).reportRules += rule
        rule
    }

    inline def report[T <: ZextObject](r: Action, conditions: Condition*)(body: T => Unit)(using tag : ClassTag[T]): ActionRule = {
        val rule = new ActionRule( { body(noun.asInstanceOf[T]); true }, (conditions :+ Condition.fromClass[T](QueryPrecedence.Class))* )
        rule.definitionPosition = CodePosition()
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
        rule.definitionPosition = CodePosition()
        ruleSets(r).afterRules += rule
        rule
    }

    inline def inflict(r: Action, conditions: Condition*)(body: => Boolean) : ActionRule = {
        val rule = new ActionRule(body, conditions*)
        rule.definitionPosition = CodePosition()
        ruleSets(r).executeRules += rule
        rule
    }

    inline def inflict[T <: ZextObject](r: Action, conditions: Condition*)(body: T => Boolean): ActionRule = {

        val condition = Condition.fromClass[T](QueryPrecedence.Class)
        val rule = new ActionRule( { body(noun.asInstanceOf[T]) }, (conditions :+ condition)* )
        rule.definitionPosition = CodePosition()
        ruleSets(r).executeRules += rule
        rule
    }

    inline def inflict[T1 <: ZextObject, T2 <: ZextObject](r: Action, conditions: Condition*)(body: (T1, T2) => Boolean)(using TypeTest[ZextObject, T1], TypeTest[ZextObject, T2]): ActionRule = {
        val firstCondition = Condition.fromClass[T1](QueryPrecedence.Class)
        val secondCondition = Condition.fromClass[T2](QueryPrecedence.SecondClass)
        val addedConditions = conditions :+ firstCondition :+ secondCondition

        val rule = new ActionRule( { body(noun.asInstanceOf[T1], secondNoun.asInstanceOf[T2]) }, addedConditions* )
        rule.definitionPosition = CodePosition()
        ruleSets(r).executeRules += rule
        rule
    }

    inline def report[T1 <: ZextObject, T2 <: ZextObject](r: Action, conditions: Condition*)(body: (T1, T2) => Unit) (using TypeTest[ZextObject, T1], TypeTest[ZextObject, T2]): ActionRule = {
        val firstCondition = Condition.fromClass[T1](QueryPrecedence.Class)
        val secondCondition = Condition.fromClass[T2](QueryPrecedence.SecondClass)
        val addedConditions = conditions :+ firstCondition :+ secondCondition

        val rule = new ActionRule( { body(noun.asInstanceOf[T1], secondNoun.asInstanceOf[T2]); true }, addedConditions* )
        rule.definitionPosition = CodePosition()
        ruleSets(r).reportRules += rule
        rule
    }

    def ResolveOverloads(possible: ArrayBuffer[ActionRule], targets : Int): Option[ActionRule] = {
        // the inform rules are something like this:
        // it seems like this is highest priority to lowest

        // number of aspects constrained
        /*
        scored zero to six, summing:
        the number of constraints are going clauses, location, things involved (actor, noun, second noun), prescence of others, time, scene
        */
        // when/while requirement
        // action requirement
        /*
        Law III.2.1 - Action/Where/Going In Exotic Ways. A more specific combination of "...pushing...", "... by ...", and "... through ..." clauses in a "going" action beats a less specific. (Placing conditions on all three of these clauses beats placing conditions on any two, which beats any one, which beats none at all.) In cases where X and Y each place, let's say, two such conditions, they are considered in the order "...pushing...", "...by..." and then "...through..." until one wins. (The idea here is that pushing something from room to room is rarer than travelling in a vehicle, which in turn is rarer than going through a door. The rarer action goes first, as more specific.)
        Law III.2.2 - Action/Where/Room Where Action Takes Place. A more specific combination of conditions on the room in which the action starts, and in which it ends, beats a less specific. For all actions other than "going", there is no combination to be considered, and what we do is to look at the specificity of the "... in ..." clause. (So "Before looking in the Taj Mahal" beats "Before looking".)
        For "going" actions, there are strictly speaking three possible room clauses: "... in ...", "... from ..." and "... to ...". However, "... in ..." and "... from ..." cannot both be present, so that in practice a "going" rule constraining two rooms beats a "going" rule constraining only one.
        If both the room gone from (the "...in..." or "...from..." room, whichever is given) and the room gone to (the "... to..." room) are constrained, then the constraints are looked at in the order from-room followed by to-room, since an action which goes to room Z could start in many different places and thus is likely to be more general.
        Giving a place as a specific room beats giving only the name of a region; if region R is entirely within region S, then a rule applying in R beats a rule applying in S. (Note that regions can only overlap if one is contained in the other, so this does not lead to ambiguity.)
        Law III.2.3 - Action/Where/In The Presence Of. A more specific "...in the presence of..." clause beats a less specific one. (This is again a constraint on where the action can take place, but it's now a potentially a constraint which could be passed in many different places at different times, so it's the most likely to be achieved and therefore the last to be considered of the Laws on Where.)
        Law III.3.1 - Action/What/Second Thing Acted On. A more specific constraint on the second noun beats a less specific. Thus "putting something in the wooden box" beats "putting something in a container".
        Law III.3.2 - Action/What/Thing Acted On. A more specific constraint on the first noun beats a less specific. Thus "taking a container which is on a supporter" beats "taking a container".
        In the case of "going" actions, the first noun is a direction. The special constraint "going nowhere" (which means: a direction in which the actor's location has no map connection) is considered more general than any other constraint placed on the first noun, but more specific than having no constraint at all. Thus "Instead of going north" beats "Instead of going nowhere" which beats "Instead of going".
        Law III.3.3 - Action/What/Actor Performing Action. A more specific constraint on the actor beats a less specific.
        Law III.4.1 - Action/How/What Happens. A more specific set of actions beats a less specific. For instance, "taking" beats "taking or dropping" beats "doing something other than looking" beats "doing something". A named kind of action (such as "behaving badly") is more specific than "doing something", but considered less specific than any explicitly spelled out list of actions.
        Law III.5.1 - Action/When/Duration. An action with a constraint on its history ("for the fifth time", say, or "for the fifth turn") beats one without. If both rules place constraints on history, then the one occurring on the smaller number of possible turns wins (thus "for the third to seventh time" - 5 possible turns of applicability - beats "for less than the tenth turn" - 9 possible turns).
        Law III.5.2 - Action/When/Circumstances. A more specific condition under "...when..." beats a less specific one. These conditions could potentially be complex: Inform judges how specific they are by counting the clauses found in them. The more clauses, the more specific the condition, it is assumed.
        Law III.6.1 - Action/Name/Is This Named. A rule with a name ("the apple blossom rule", say) beats a rule without.
        */
        // scene requirement



        if (possible.isEmpty)
            return Option.empty

        if(possible.length == 1)
            return possible.headOption

        val maxPrecedence = possible.map(_.precedence).max
        val maxPrecedenceRules = possible.filter(_.precedence == maxPrecedence)
        val rule = maxPrecedenceRules.maxBy(_.specificity)
        Option(rule)
    }


    val previouslySucceeded = mutable.HashSet[Condition]()

    def EvaluatePreviouslyConditions(rule: Action, target: Option[ZextObject] = None, target2: Option[ZextObject]): Unit = {
        previouslySucceeded.clear()

        var targets = 0
        if(target.isDefined) {
            targets += 1
            noun = target.get
        }

        if(target2.isDefined) {
            targets += 1
            secondNoun = target2.get
        }

        // forbid using 'noun' in conditions when previously

        val set = ruleSets(rule)
        set.beforeRules.filter(r => r.targets == targets || r.generic).foreach( _.storePreviousPossibilities() )
        set.insteadRules.filter(r => r.targets == targets || r.generic).foreach( _.storePreviousPossibilities() )
        set.executeRules.filter(r => r.targets == targets || r.generic).foreach( _.storePreviousPossibilities() )
        set.reportRules.filter(r => r.targets == targets || r.generic).foreach( _.storePreviousPossibilities() )
        set.afterRules.filter(r => r.targets == targets || r.generic).foreach( _.storePreviousPossibilities() )
    }

    def execute(rule: Action, target: Option[ZextObject] = None, target2: Option[ZextObject] = None): Boolean = {
        val set = ruleSets(rule)
        var targets = 0

        var stackNoun : ZextObject = null
        var stackNoun2 : ZextObject = null

        // pain
        if (target.isDefined) {
            stackNoun = target.get
            if(!stackNoun.isVisible(currentLocation)){
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
            if(!stackNoun2.isVisible(currentLocation)){
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
        val insteadPossible = set.insteadRules.filter(r => r.targets == targets || r.generic).filter(_.possible)
        if(!RunRule(stackNoun, stackNoun2, insteadPossible , targets)) return false

        noun = stackNoun
        secondNoun = stackNoun2
        val executePossible = set.executeRules.filter(r => r.targets == targets || r.generic).filter(_.possible)
        if(!RunRule(stackNoun, stackNoun2, executePossible, targets)) return false

        noun = stackNoun
        secondNoun = stackNoun2
        val reportPossible = set.reportRules.filter(r => r.targets == targets || r.generic).filter(_.possible)
        if(!RunRule(stackNoun, stackNoun2, reportPossible, targets)) return false

        noun = stackNoun
        secondNoun = stackNoun2
        val afterPossible = set.afterRules.filter(r => r.targets == targets || r.generic).filter(_.possible)
        if(!RunRule(stackNoun, stackNoun2, afterPossible, targets)) return false


        true
    }

    def execute(rule: Action, target: ZextObject): Boolean = {
        execute(rule, Some(target))
    }
}

abstract class Rule {
    val disabled = false
    var definitionPosition : String = null
}


object PersistingRule {
    implicit def fromUnit(body : => Unit) : PersistingRule = PersistingRule(body)
}

class PersistingRule( body : => Unit) extends Rule {
    def Execute() = body
}

enum QueryPrecedence:
    case Generic, Class, SecondClass, Property, Containment, Object, SecondObject, Location

object Condition{
    // inform's precedence is something like
    // location > object > property > class > generic


    implicit def fromBoolean(b : => Boolean) : Condition = new Condition(b, QueryPrecedence.Generic)
    implicit def fromObject(z : => ZextObject) : Condition = new Condition(z == noun, QueryPrecedence.Object)
    implicit def fromObjectArray(az : => Seq[ZextObject]) : Condition = new Condition( az.contains(noun), QueryPrecedence.Object)
    implicit def fromProperty(p : => Property) : Condition = new Condition(noun.properties.contains(p), QueryPrecedence.Property)
    implicit def fromLocation(r : => Room) : Condition = new Condition(r == currentLocation, QueryPrecedence.Location)
    inline def fromClass[T](queryType : QueryPrecedence = QueryPrecedence.Class)(using TypeTest[ZextObject, T]): Condition = {
        val condition = new Condition(
            {
                val target = if(queryType == QueryPrecedence.Class) noun else secondNoun
                val success = canBecome[ZextObject, T](target)
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

    def storePreviousPossibilities(): Unit = {
        conditions.filter(_.previously).filter(_.evaluate).foreach(previouslySucceeded.add)
    }

    def possible : Boolean = {
        try{
            conditions.filterNot(_.previously).forall( _.evaluate ) && conditions.filter(_.previously).forall( previouslySucceeded(_) )
        } catch {
            case e => {
                System.err.println("Error from condition at: .(" + definitionPosition + ")")
                throw e
            }
        }

    }

    def exec = body
}





class Action(val verbs : String*) extends Rule with ParsableType(PartOfSpeech.verb) {

    Understand(this, verbs*)

    ruleSets(this) = new ActionRuleSet
    override def toString = verbs(0)
}

