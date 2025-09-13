package Zext

import Zext.Actions.{UnderstandAlias, allActions, does, examining}
import Zext.EverythingParser.ParseResult
import Zext.Parser.*
import Zext.Relation.RelationQuery
import Zext.Rule.*
import Zext.RuleContext.*
import Zext.RuleControl.{Continue, Replace, Stop}
import Zext.World.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.reflect.{ClassTag, TypeTest}
import scala.util.control.{Breaks, ControlThrowable}
import zobjectifier.Macros
import zobjectifier.Macros.CodePosition


object ZextObjectProxy {
    implicit def toZext[T <: ZextObject](z : ZextObjectProxy[T]): T = z.resolve

}

abstract class ZextObjectProxy[+T <: ZextObject] extends SetComprehension[Nothing]{

    override def getSet() = Seq(resolve).asInstanceOf[Seq[Nothing]]

    override def toString = resolve.toString
    def resolve : T

    override def equals(obj: Any) = {

         obj match {
             case zextObjectProxy: ZextObjectProxy[?] => this.objectID == zextObjectProxy.objectID
             case zextObject: ZextObject => this.objectID == zextObject.objectID
             case null => false
        }
    }

}

object noun extends ZextObjectProxy[ZextObject] {
    override def resolve = RuleContext._noun
}

def GetNouns() : Array[ZextObject] = {
    RuleContext._nouns
}

object subject extends ZextObjectProxy[ZextObject]{
    override def resolve = RuleContext._subject
}

object secondNoun extends ZextObjectProxy[ZextObject] {
    override def resolve = RuleContext._secondNoun
}

object RuleContext {

    private[Zext] var _noun: ZextObject = null
    private[Zext] var _secondNoun: ZextObject = null
    private[Zext] var _nouns : Array[ZextObject] = Array()
    private[Zext] var _subject : ZextObject = null
    private[Zext] var _first: Boolean = false
    private[Zext] var _silent: Boolean = false
    private[Zext] var _location: ZContainer = nowhere

    private[Zext] def SetContext(ctx : RuleContext) : Unit = {
        _noun = ctx.nouns.headOption.orNull
        _secondNoun = ctx.nouns.lift(1).orNull
        _nouns = ctx.nouns
        _silent = ctx.silent
        _location = ctx.location
        _subject = ctx.subject
    }


    def InheritContext(subject: ZextObject = null, target: ZextObject = null, target2: ZextObject = null, silent: Option[Boolean] = None, location: ZContainer = null): RuleContext = {
        val targets = ConsolidateTargets(target, target2)
        val currentLocation = if (location == null) GetCurrentRuleContext().location else location
        val currentSubject = if (subject == null) GetCurrentRuleContext().subject else subject
        val currentSilence = if (silent.isDefined) silent.get else GetCurrentRuleContext().silent
        assert(currentSubject != null, "you must specify a subject if there isn't a current rule context")
        assert(currentLocation != null, "you need to specify a location if there isn't a current rule context")
        RuleContext(currentSubject, targets, currentSilence, currentLocation)
    }

    def GetCurrentRuleContext() = RuleContext(_subject, _nouns, _silent, _location)

    def first : Boolean =  _first
    def silent : Boolean = _silent
    def location : ZContainer = _location
}


case class RuleContext(subject : ZextObject, nouns : Array[ZextObject], silent: Boolean, location : ZContainer)

object Rule {

    var blackboard : Any = null

    class ActionRuleSet {
        // this is the order they're executed in
        val applyingRules = ArrayBuffer[ActionRule]()
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


    inline def applying(r: Action | ActionWithContext[?], conditions: Condition*)(inline body: => Unit): ActionRule = {
        val (action, moreConditions) = r match {
            case ac: ActionWithContext[?] => (ac.action.asInstanceOf[Action], conditions appended ac.toCondition)
            case a: Action => (a, conditions)
        }
        val rule = new ActionRule({body; Continue}, moreConditions *)
        rule.definitionPosition = CodePosition()
        ruleSets(action).applyingRules += rule
        rule
    }


    inline def check(r: Action | ActionWithContext[?], conditions: Condition*)(inline body: => Unit): ActionRule = {
        val (action, moreConditions) = r match {
            case ac: ActionWithContext[?] => (ac.action.asInstanceOf[Action], conditions appended ac.toCondition)
            case a: Action => (a, conditions)
        }

        val rule = new ActionRule({body; Continue}, moreConditions *)
        rule.definitionPosition = CodePosition()
        ruleSets(action).checkRules += rule
        rule
    }


    inline def before(r: Action | ActionWithContext[?], conditions: Condition*)(inline body: => Unit): ActionRule = {
        val (action, moreConditions) = r match {
            case ac: ActionWithContext[?] => (ac.action.asInstanceOf[Action], conditions appended ac.toCondition)
            case a: Action => (a, conditions)
        }

        val rule = new ActionRule({body; Continue}, moreConditions*)
        rule.definitionPosition = CodePosition()
        ruleSets(action).beforeRules += rule
        rule
    }

    inline def instead(r: Action | ActionWithContext[?], conditions: Condition*)(inline body: => Unit): ActionRule = {
        val (action, moreConditions) = r match {
            case ac: ActionWithContext[?] => (ac.action.asInstanceOf[Action], conditions appended ac.toCondition)
            case a: Action => (a, conditions)
        }

        val rule = new ActionRule({body; Stop}, moreConditions*)
        rule.definitionPosition = CodePosition()
        ruleSets(action).insteadRules += rule
        rule
    }

    inline def inflict(r: Action | ActionWithContext[?], conditions: Condition*)(inline body: => Unit) : ActionRule = {
        val (action, moreConditions) = r match {
            case ac : ActionWithContext[?] => (ac.action.asInstanceOf[Action], conditions appended ac.toCondition)
            case a : Action => (a, conditions)
        }

        val rule = new ActionRule({body; Continue}, moreConditions*)
        rule.definitionPosition = CodePosition()
        ruleSets(action).executeRules += rule
        rule
    }

    inline def report(r: Action | ActionWithContext[?], conditions: Condition*)(inline body: => Unit): ActionRule = {
        val (action, moreConditions) = r match {
            case ac: ActionWithContext[?] => (ac.action.asInstanceOf[Action], conditions appended ac.toCondition)
            case a: Action => (a, conditions)
        }

        val rule = new ActionRule( {body; Replace}, moreConditions* )
        rule.definitionPosition = CodePosition()
        ruleSets(action).reportRules += rule
        rule
    }

    inline def after(r: Action | ActionWithContext[?], conditions: Condition*)(inline body: => Unit): ActionRule = {
        val (action, moreConditions) = r match {
            case ac: ActionWithContext[?] => (ac.action.asInstanceOf[Action], conditions appended ac.toCondition)
            case a: Action => (a, conditions)
        }

        val rule = new ActionRule({body; Continue}, moreConditions*)
        rule.definitionPosition = CodePosition()
        ruleSets(action).afterRules += rule
        rule
    }

    // ultra terse syntax
    class InsteadConsequence(r: Action | ActionWithContext[?], conditions: => Condition*) {
        infix inline def Say(s: StringExpression): ActionRule = {
            instead(r, conditions *)(Interpreter.Say(s))
        }
    }

    class ReportConsequence(r: Action | ActionWithContext[?], conditions: => Condition*) {
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

    def report(r: Action | ActionWithContext[?], conditions: => Condition*): ReportConsequence = {
        new ReportConsequence(r, conditions *)
    }

    def instead(r: Action | ActionWithContext[?], conditions: => Condition*): InsteadConsequence = {
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

        val previousContext = GetCurrentRuleContext()
        SetContext(context)

        val possibleRules = rules.filter(_.possible)
        val sorted = SortByPrecedence(possibleRules)
        val result = ExecuteRules(sorted)

        SetContext(previousContext)
        result
    }

    // this is different because applying rules only run if possible, while normal rules only don't run if impossible.
    def RunApplyingRule(context: RuleContext, rules: ArrayBuffer[ActionRule]): Boolean = {

        val previousContext = GetCurrentRuleContext()
        SetContext(context)

        val possibleRules = rules.filter(_.possible)
        val sorted = SortByPrecedence(possibleRules)
        val result = ExecuteRules(sorted)

        SetContext(previousContext)
        result && possibleRules.nonEmpty
    }


    def RunApplyingBeforeRules(userCommand : Command): Unit = {

    }

    def RunApplyingRules(userCommand : Command): Unit = {
        // get all actions with applying rules
        val applyingActions = Actions.allActions.filter(ruleSets(_).applyingRules.nonEmpty)

        for(action <- applyingActions){
            val applyingRules = ruleSets(action).applyingRules
            val allThings = ZextObject.allObjects.filter(_.isInstanceOf[Thing]).map(_.asInstanceOf[Thing])

            for(thing <- allThings){
                val thingLocation = thing.location
                val ruleContext = new RuleContext(nothing, Array(thing), false, thingLocation)
                if(RunApplyingRule(ruleContext, applyingRules))
                    ExecuteAction(action, RuleContext(nothing, Array(thing), false, thingLocation))
            }
        }
    }


    case class ExecutionResult[T](res : Boolean, ret : T)

    // convenience for not having to create an array.
    def ExecuteAction(action: Action, subject: ZextObject = null, target: ZextObject = null, target2: ZextObject = null, silent: Option[Boolean] = None, location: ZContainer = null): Boolean = {
          ExecuteAction(action, InheritContext(subject, target, target2, silent, location))
    }


    def ExecuteContextAction[T](rule: ActionWithContext[T], subject: ZextObject = null, target: ZextObject = null, target2: ZextObject = null, silent: Option[Boolean] = None, location: ZContainer = null): ExecutionResult[T] = {
        val previous = rule.action.GetActionContext()
        rule.action.SetActionContext(rule.context)
        val result = ExecuteAction(rule.action.asInstanceOf[Action], InheritContext(subject, target, target2, silent, location))
        val ctxValue = rule.action.GetActionContext()
        rule.action.SetActionContext(previous)
        ExecutionResult(result, ctxValue)
    }


    def ExecuteAction(rule: Action, context: RuleContext): Boolean = {

        rule match {
            case value: Context[?] =>
                assert(value.GetActionContext() != null, "Call context actions with the version that takes a context")
            case _ =>
        }

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

         val previousBlackboard = blackboard

         for(rules <- set.GetAllRules()) {
             if (!RunRule(context, rules)) {
                 blackboard = previousBlackboard
                 return false
             }
         }

         blackboard = previousBlackboard
         true
    }



     // for running a ruleset in another rule
     def ExecuteSubRules(rules : ArrayBuffer[ActionRule], ruleContext : RuleContext) : RuleControl = {

         def runRules(sortedRules: Seq[ActionRule]): RuleControl = {
             for (rule <- sortedRules) {
                 val result = rule.exec
                 result match {
                     case Continue =>
                     case Stop => return Stop
                     case Replace => return Replace
                 }
             }
             Continue
         }


         val previousContext = GetCurrentRuleContext()
         val previousBlackboard = blackboard

         SetContext(ruleContext)

         val possibleRules = rules.filter(_.possible)
         val sorted = SortByPrecedence(possibleRules)
         val result = runRules(sorted)

         SetContext(previousContext)
         blackboard = previousBlackboard

         result
     }


    private[Zext] inline def ConsolidateTargets(target: ZextObject, target2: ZextObject) : Array[ZextObject] = {
        // target2 must be null if target is null
        if (target == null)
            require(target2 == null)

        if (target != null && target2 != null)
            Array(target, target2)
        else if (target != null)
            Array(target)
        else
            Array[ZextObject]()
    }


}

abstract class Rule {
    val disabled = false
    var definitionPosition : String = null
}


enum QueryPrecedence:
    case Generic, Class, SecondClass, Property, SecondProperty, Content, Object, SecondObject, Location


class Condition(condition: => Boolean, var queryType: QueryPrecedence) {
    def evaluate = condition
    var specificity = 1
    def precedence = queryType.ordinal


    def &&(other: Condition): Condition = {
        // combine predicates
        val precedence = if(this.queryType.ordinal > other.queryType.ordinal) this.queryType else other.queryType
        val c = new Condition(this.evaluate && other.evaluate, precedence)
        c.specificity = this.specificity + other.specificity
        c
    }

}


abstract class ConditionHelper {
    def createCondition(queryPrecedence: QueryPrecedence) : Condition
}

case class Priority(amount : Int) extends ConditionHelper {
    override def createCondition(queryPrecedence: QueryPrecedence) = {
        val c = new Condition(true, queryPrecedence)
        c.specificity = amount
        c
    }
}

object Condition {
    // inform's precedence is something like
    // location > object > property > class > generic

    implicit def fromBoolean(b: => Boolean): Condition = new Condition(b, QueryPrecedence.Generic)
    implicit def fromObject(z: => ZextObject): Condition = new Condition(z == noun, QueryPrecedence.Object)
    def fromSecondObject(z: => ZextObject): Condition = new Condition(z == secondNoun, QueryPrecedence.SecondObject)
    implicit def fromObjectArray(az: => Seq[ZextObject]): Condition = new Condition(az.contains(noun), QueryPrecedence.Object)
    implicit def fromProperty(p: => Property): Condition = new Condition(noun.properties.contains(p), QueryPrecedence.Property)
    implicit def fromLocation(r: => Room): Condition = new Condition(r == noun, QueryPrecedence.Location)
    implicit def fromRegion(r: => RoomRegion): Condition = new Condition(r == noun, QueryPrecedence.Location)
    implicit def fromClassHolder(ch: => ZextObjectClassHolder): Condition = ch.createCondition(QueryPrecedence.Class)
    implicit def fromPropHolder(ph: => ZextObjectPropHolder): Condition = ph.createCondition(QueryPrecedence.Property)
    implicit def fromConditionHelper(helper: => ConditionHelper): Condition = helper.createCondition(QueryPrecedence.Generic)
    implicit def fromQuery(query: => RelationQuery[?,?]) : Condition = new Condition(query.evaluate(), query.relation.precedence)

    type ConditionTypes = ZextObject | ZextObjectProxy[?] | ConditionHelper

    implicit def fromTuple(t: => (ConditionTypes, ConditionTypes)): Condition = {

        val firstPredicate : Condition = t._1 match {
            case anythingFirst : ZextObject if anythingFirst == anything => { val c = Condition(true, QueryPrecedence.Generic); c.specificity = 0; c}
            case propHolder : ZextObjectPropHolder => propHolder.createCondition(QueryPrecedence.Property)
            case classHolder : ZextObjectClassHolder => classHolder.createCondition(QueryPrecedence.Class)
            case zextObjectProxy: ZextObjectProxy[?] => fromObject(zextObjectProxy.resolve.asInstanceOf[ZextObject])
            case zextObject: ZextObject => fromObject(zextObject)
            case helper : ConditionHelper => helper.createCondition(QueryPrecedence.Generic)
        }

        val secondPredicate: Condition = t._2 match {
            case anythingFirst : ZextObject if anythingFirst == anything => { val c = Condition(true, QueryPrecedence.Generic); c.specificity = 0; c}
            case propHolder : ZextObjectPropHolder => propHolder.createCondition(QueryPrecedence.SecondProperty)
            case classHolder : ZextObjectClassHolder => classHolder.createCondition(QueryPrecedence.SecondClass)
            case zextObjectProxy: ZextObjectProxy[?] => fromSecondObject(zextObjectProxy.resolve.asInstanceOf[ZextObject])
            case zextObject: ZextObject => fromSecondObject(zextObject)
            case helper : ConditionHelper => helper.createCondition(QueryPrecedence.Generic)
        }

        firstPredicate && secondPredicate
    }


    // these have to be macros to get the proper depth for T
     inline def of[T <: ZextObject | Container](using tt: TypeTest[ZextObject | Container, T]) : ZextObjectClassHolder = {
         val depth = Macros.depth[T, ZextObject, Container]
         val typeName = Macros.typeName[T]
         new ZextObjectClassHolder(tt, depth, typeName)
    }

    inline def of[T <: Property](using tt: TypeTest[Property, T], dummy: DummyImplicit): ZextObjectPropHolder = {
        val typeName = Macros.typeName[T]
        new ZextObjectPropHolder(tt, 1, typeName)
    }

    inline def ofDebug[T <: ZextObject | Container](name : String)(using tt: TypeTest[ZextObject | Container, T]) : ZextObjectClassHolder = {
        val depth = Macros.depth[T, ZextObject, Container] // depth of container is -1, which is maybe not expected
        val typeName = Macros.typeName[T]
        //println(s"making of $typeName with name $name with depth $depth")
        new ZextObjectClassHolder(tt, depth, name)
    }

    inline def ofDebug[T <: Property](name : String)(using tt: TypeTest[Property, T], dummy: DummyImplicit): ZextObjectPropHolder = {
        val typeName = Macros.typeName[T]
        //println(s"making of $typeName with name $name with depth $depth")
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
        condition.specificity = Macros.depth[T, ZextObject, Container]
        condition
    }

    def canBecome[X, Y](x: X)(using tt: TypeTest[X, Y]): Boolean = {
        tt.unapply(x).isDefined
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



// debug actions will have the entire set of objects in their scope
trait DebugAction {
    this : Action =>
}

case class ActionWithContext[T](action : Context[T], context : T) {
    def toCondition: Condition = {
        new Condition(action.GetActionContext() == context, QueryPrecedence.Object)
    }
}

trait Context[T] {
    this: Action =>

    private var _ctx : T = null.asInstanceOf[T]
    def GetActionContext(): T = _ctx
    def SetActionContext(context: T): Unit = _ctx = context
    def apply(context : T) = ActionWithContext(this, context)


}

class Action(val targets : Int, val verbs : String*) extends Rule with ParsableType(PartOfSpeech.verb) {

    allActions.addOne(this)

    def implicitTargetSelector : SetComprehension[ZextObject] = null
    def implicitSubjectSelector : SetComprehension[ZextObject] = null
    var disambiguationHint : ParsableType => Boolean = null

    ruleSets(this) = new ActionRuleSet
    override def toString = verbs(0)

    def execute(subject: ZextObject = null, target: ZextObject = null, target2: ZextObject = null, silent: Boolean = false, location: ZContainer = null) : Boolean = {
        val targets = ConsolidateTargets(target, target2)
        val currentLocation = if (location == null) GetCurrentRuleContext().location else location
        val currentSubject = if (subject == null) GetCurrentRuleContext().subject else subject
        assert(currentSubject != null, "you must specify a subject if there isn't a current rule context")
        assert(currentLocation != null, "you need to specify a location if there isn't a current rule context")
        RuleContext(currentSubject, targets, silent, currentLocation)
        ExecuteAction(this, RuleContext(currentSubject, targets, silent, currentLocation))
    }

}




abstract class CustomAction(targets: Int, verbs : String*) extends Action(targets, verbs*) {

    // when encountering a custom action, allow the user to intercept the raw text and parse results
    def intercept(rawInput : String, parseResult: ParseResult) : Command

}