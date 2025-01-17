package Zext

import Zext.Actions.{UnderstandAlias, allActions, examining}
import Zext.Interpreter.*
import Zext.Macros.{CodePosition, depth}
import Zext.Parser.*
import Zext.Rule.*
import Zext.RuleContext.*
import Zext.RuleControl.{Continue, Replace, Stop}
import Zext.StringExpression.str
import Zext.World.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.quoted.*
import scala.reflect.{ClassTag, TypeTest}
import scala.util.control.{Breaks, ControlThrowable}


object ZextObjectProxy {
    implicit def toZext[T <: ZextObject](z : ZextObjectProxy[T]): T = z.resolve
}

abstract class ZextObjectProxy[T <: ZextObject]  {
    override def toString = resolve.toString
    def resolve : T

    override def equals(obj: Any) = {
         obj match {
             case zextObjectProxy: ZextObjectProxy[_] => resolve.objectID == zextObjectProxy.objectID
             case zextObject: ZextObject => resolve.objectID == zextObject.objectID
        }
    }

    infix def has(zextObject: => ZextObject) = Condition(zextObject.parentContainer == resolve, QueryPrecedence.Containment)
    infix def lacks(zextObject: => ZextObject) = Condition(zextObject.parentContainer != resolve, QueryPrecedence.Containment)
}

object noun extends ZextObjectProxy[ZextObject] {
    override def resolve = RuleContext._noun
}

object secondNoun extends ZextObjectProxy[ZextObject] {
    override def resolve = RuleContext._secondNoun
}

object RuleContext {

    private[Zext] var _noun: ZextObject = null
    private[Zext] var _secondNoun: ZextObject = null
    private[Zext] var _first: Boolean = false
    private[Zext] var _silent: Boolean = false

    private[Zext] def SetContext(ctx : RuleContext) : Unit = {
        _noun = ctx.z1.orNull
        _secondNoun = ctx.z2.orNull
        _silent = ctx.silent
    }

    def first : Boolean =  _first
    def silent : Boolean = _silent
}

case class RuleContext(z1: Option[ZextObject], z2: Option[ZextObject], silent: Boolean)

object Rule {

    var blackboard : Any = null

    class ActionRuleSet {
        // this is the order they're executed in
        val beforeRules = ArrayBuffer[ActionRule]()
        val insteadRules = ArrayBuffer[ActionRule]()
        val checkRules = ArrayBuffer[ActionRule]()
        val reportRules = ArrayBuffer[ActionRule]()
        val executeRules = ArrayBuffer[ActionRule]()
        val afterRules = ArrayBuffer[ActionRule]()

        def GetAllRules() = {
            Array(beforeRules, insteadRules, checkRules, reportRules, executeRules, afterRules)
        }
    }

    val ruleSets = new mutable.HashMap[Action, ActionRuleSet]()


    inline def check(r: Action, conditions: Condition*)(body: => Unit): ActionRule = {
        val rule = new ActionRule({body; Continue}, conditions *)
        rule.definitionPosition = CodePosition()
        ruleSets(r).checkRules += rule
        rule
    }


    inline def before(r: Action, conditions: Condition*)(body: => Unit): ActionRule = {
        val rule = new ActionRule({body; Continue}, conditions*)
        rule.definitionPosition = CodePosition()
        ruleSets(r).beforeRules += rule
        rule
    }

    inline def instead(r: Action, conditions: Condition*)(body: => Unit): ActionRule = {
        val rule = new ActionRule({body; Stop}, conditions*)
        rule.definitionPosition = CodePosition()
        ruleSets(r).insteadRules += rule
        rule
    }

    inline def inflict(r: Action, conditions: Condition*)(body: => Unit) : ActionRule = {
        val rule = new ActionRule({body; Continue}, conditions*)
        rule.definitionPosition = CodePosition()
        ruleSets(r).executeRules += rule
        rule
    }

    inline def report(r: Action, conditions: Condition*)(body: => Unit): ActionRule = {
        val rule = new ActionRule( {body; Replace}, conditions* )
        rule.definitionPosition = CodePosition()
        ruleSets(r).reportRules += rule
        rule
    }

    inline def after(r: Action, conditions: Condition*)(body: => Unit): ActionRule = {
        val rule = new ActionRule({body; Continue}, conditions*)
        rule.definitionPosition = CodePosition()
        ruleSets(r).afterRules += rule
        rule
    }


    // ultra terse syntax
    class InsteadConsequence(r: Action, conditions: Condition*) {
        infix inline def Say(s: StringExpression): ActionRule = {
            instead(r, conditions *)(Interpreter.Say(s))
        }
    }

    class ReportConsequence(r: Action, conditions: Condition*) {
        infix inline def Say(s: StringExpression): ActionRule = {
            report(r, conditions *)(Interpreter.Say(s))
        }

        infix inline def Add(s: StringExpression): ActionRule = {
            report(r, conditions *){
                Interpreter.Say(s)
                continue
            }
        }
    }

    def report(r: Action, conditions: Condition*): ReportConsequence = {
        new ReportConsequence(r, conditions *)
    }

    def instead(r: Action, conditions: Condition*): InsteadConsequence = {
        new InsteadConsequence(r, conditions *)
    }


    def SortByPrecedence(possible: ArrayBuffer[ActionRule]): Seq[ActionRule] = {
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
        Law III.3.1 - Action/What/Second Thing Acted On. A more specific constraint on the second noun beats a less specific. Thus " putting something in the wooden box" beats "putting something in a container".
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
            return Seq()

        if(possible.length == 1)
            return Seq(possible.head)

        val precedenceSets = possible.groupBy(_.precedence).toSeq

        val sortedPrecedenceSets = precedenceSets.map ( kv => kv._1 -> kv._2.sortBy(-_.specificity))
        val flat = sortedPrecedenceSets.sortBy( kv => kv._1 ).reverse.flatten(kv => kv._2)

        flat
    }


    def ExecuteRules(sortedRules : Seq[ActionRule]) : Boolean = {
        for (rule <- sortedRules) {
            val result = rule.exec
            result match {
                case Continue =>
                case Stop => return false
                case Replace => return true
            }
        }
        true
    }

    def RunRule(context : RuleContext, rules: ArrayBuffer[ActionRule]): Boolean = {

        val previousContext = RuleContext(Option(noun), Option(secondNoun), silent)
        SetContext(context)

        val possibleRules = rules.filter(_.possible)
        val sorted = SortByPrecedence(possibleRules)
        val result = ExecuteRules(sorted)

        SetContext(previousContext)


        result
    }

     def ExecuteAction(rule: Action, target: Option[ZextObject] = None, target2: Option[ZextObject] = None, silent : Boolean = false): Boolean = {
        val set = ruleSets(rule)

         /*
            these are the rules for inform's rule execution, we are not following them, but it's useful to know anyway.
            one thing about before, instead, and after rules is that in inform, they're global. They are all checked every command.
            I believe this allows them to apply to sets of rules, or reason about rule logic in a powerful way. We don't do anything like that at the moment, but maybe!

            Before: by default, make no decision. If stopped, no further rulebooks are run.
            (some internal visibility/accessibility sanity checks run here)
            Instead: by default, stop the action. If stopped, no further rulebooks are run.
            Check: by default, make no decision. If stopped, no further rulebooks are run.
            Carry Out: by default, make no decision. If stopped, other rulebooks continue.
            After: by default, stop the action. If stopped, no further rulebooks are run.
            Report: by default, make no decision.
          */

         val context = RuleContext(target, target2, silent)

         val previousBlackboard = blackboard

         for(rules <- set.GetAllRules()) {
             if (!RunRule(context, rules)) {
                 blackboard = previousBlackboard
                 return false // cry about it.
             }
         }

         blackboard = previousBlackboard
         true
    }

    def execute(rule: Action, target: ZextObject, target2: ZextObject = null, silent : Boolean = false): Boolean = {
        ExecuteAction(rule, Some(target), Option(target2), silent)
    }
}

abstract class Rule {
    val disabled = false
    var definitionPosition : String = null
}


enum QueryPrecedence:
    case Generic, Class, SecondClass, Property, SecondProperty, Containment, Object, SecondObject, Location


class Condition(condition: => Boolean, var queryType: QueryPrecedence) {
    def evaluate = condition
    var specificity = 1
    def precedence = queryType.ordinal
    def unary_! : Condition = {
        val c = new Condition(!condition, queryType)
        c.specificity = specificity
        c
    }

    def &&(other: Condition): Condition = {
        // combine predicates
        val precedence = if(this.queryType.ordinal > other.queryType.ordinal) this.queryType else other.queryType
        val c = new Condition(this.evaluate && other.evaluate, precedence)
        c.specificity = this.specificity + other.specificity
        c
    }

}

object Condition {
    // inform's precedence is something like
    // location > object > property > class > generic

    implicit def fromBoolean(b: => Boolean): Condition = new Condition(b, QueryPrecedence.Generic)
    implicit def fromObject(z: => ZextObject): Condition = new Condition(z.objectID == noun.objectID, QueryPrecedence.Object)
    implicit def fromObjectArray(az: => Seq[ZextObject]): Condition = new Condition(az.contains(noun), QueryPrecedence.Object)
    implicit def fromProperty(p: => Property): Condition = new Condition(noun.properties.contains(p), QueryPrecedence.Property)
    implicit def fromLocation(r: => Room): Condition = new Condition(r.objectID == noun.objectID, QueryPrecedence.Location)
    implicit def fromClassHolder(ch: => ZextObjectClassHolder): Condition = ch.createCondition(QueryPrecedence.Class)
    implicit def fromPropHolder(ph: => ZextObjectPropHolder): Condition = ph.createCondition(QueryPrecedence.Property)


    implicit def fromTuple(t: => (ZextObject, ZextObject)): Condition = {

        val firstPredicate : Condition = t._1 match {
            case anythingFirst : ZextObject if anythingFirst.objectID == anything.objectID => { val c = Condition(true, QueryPrecedence.Generic); c.specificity = 0; c}
            case propHolder : ZextObjectPropHolder => propHolder.createCondition(QueryPrecedence.Property)
            case classHolder : ZextObjectClassHolder => classHolder.createCondition(QueryPrecedence.Class)
            case _ => fromObject(t._1)
        }

        val secondPredicate: Condition = t._2 match {
            case anythingFirst : ZextObject if anythingFirst.objectID == anything.objectID => { val c = Condition(true, QueryPrecedence.Generic); c.specificity = 0; c}
            case propHolder : ZextObjectPropHolder => propHolder.createCondition(QueryPrecedence.SecondProperty)
            case classHolder : ZextObjectClassHolder => classHolder.createCondition(QueryPrecedence.SecondClass)
            case _ => {val c = fromObject(t._2); c.queryType = QueryPrecedence.SecondObject; c}
        }

        firstPredicate && secondPredicate
    }


    // these have to be macros to get the proper depth for T
     inline def of[T <: ZextObject | Container](using tt: TypeTest[ZextObject | Container, T]) : ZextObjectClassHolder = {
         val depth = Macros.depth[T]
         val typeName = Macros.typeName[T]
         new ZextObjectClassHolder(tt, depth, typeName)
    }

    inline def of[T <: Property](using tt: TypeTest[Property, T], dummy: DummyImplicit): ZextObjectPropHolder = {
        val typeName = Macros.typeName[T]
        new ZextObjectPropHolder(tt, 1, typeName)
    }

    inline def ofDebug[T <: ZextObject | Container](name : String)(using tt: TypeTest[ZextObject | Container, T]) : ZextObjectClassHolder = {
        val depth = Macros.depth[T] // depth of container is -1, which is maybe not expected
        val typeName = Macros.typeName[T]
        println(s"making of $typeName with name $name with depth $depth")
        new ZextObjectClassHolder(tt, depth, name)
    }

    inline def ofDebug[T <: Property](name : String)(using tt: TypeTest[Property, T], dummy: DummyImplicit): ZextObjectPropHolder = {
        val typeName = Macros.typeName[T]
        println(s"making of $typeName with name $name with depth $depth")
        new ZextObjectPropHolder(tt, 1, name)
    }

    // this is for querying whether a specific object has a type
    def isZextObjectOf[T](target : => ZextObject, queryType: QueryPrecedence = QueryPrecedence.Class)(using TypeTest[ZextObject, T]): Condition = {
        val condition = new Condition(
            {
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
        become[X, Y](x).isDefined
    }
}

class ReplaceException extends ControlThrowable
class ContinueException extends ControlThrowable
class StopException extends ControlThrowable
def continue: Unit = throw new ContinueException
def stop: Unit = throw new StopException
def replace: Unit = throw new ReplaceException

enum RuleControl {
    case Stop, Continue, Replace
}


def result(res : Boolean) : Unit = {
    if(res) continue else stop
}

class ActionRule(body : => RuleControl, conditions : Condition*) extends Rule{
    var first = true

    def specificity = {
        conditions.map( _.specificity ).sum
    }

    def precedence ={
        conditions.map(_.precedence).foldLeft(0)( _ max _ )
    }


    def possible : Boolean = {
        try{
            conditions.forall( _.evaluate )
        } catch {
            case cast : ClassCastException => {
                // if we're trying to cast to something it's not, then that means it's not possible.
                false
            }
            case e: Throwable => {
                System.err.println("Error from condition at: .(" + definitionPosition + ")")
                throw e
            }
        }
    }

    def exec : RuleControl = {
        val previous = _first
        _first = this.first
        this.first = false

        val ret = try{
            body
        } catch {
            case ex : ContinueException => RuleControl.Continue
            case ex : StopException => RuleControl.Stop
            case ex : ReplaceException => RuleControl.Replace
        }

        _first = previous
        ret
    }
}





class Action(val targets : Int, val verbs : String*) extends Rule with ParsableType(PartOfSpeech.verb) {

    allActions.addOne(this)

    var implicitTargetSelector : ZextObject => Boolean = null
    var implicitSubjectSelector : ZextObject => Boolean = null
    var disambiguationHint : ZextObject => Boolean = null

    ruleSets(this) = new ActionRuleSet
    override def toString = verbs(0)

}

