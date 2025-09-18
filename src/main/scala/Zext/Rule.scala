package Zext

import Zext.Actions.{UnderstandAlias, allActions, allMetaActions, examining}
import Zext.EverythingParser.ParseResult
import Zext.Infliction.*
import Zext.Infliction.RuleControl.Default
import Zext.Interpreter.Say
import Zext.Parser.*
import Zext.QueryPrecedence.{Action, Context}
import Zext.Relation.RelationQuery
import Zext.Rule.*
import Zext.RuleContext.*
import Zext.World.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.{implicitConversions, postfixOps}
import scala.reflect.{ClassTag, TypeTest}
import scala.util.control.{Breaks, ControlThrowable}
import zobjectifier.Macros
import zobjectifier.Macros.CodePosition


object RelatableProxy {
    implicit def toRelatable[T <: Relatable](r : RelatableProxy[T]): T = r.resolve
}

abstract class RelatableProxy[+T <: Relatable] extends SetComprehension[Nothing]{

    override def getSet() = Seq(resolve).asInstanceOf[Seq[Nothing]]

    override def toString = resolve.toString
    def resolve : T

    //@todo: superstition
    override def equals(obj: Any) = {
         obj match {
             case relatableProxy: RelatableProxy[?] => this.objectID == relatableProxy.objectID
             case relatable: Relatable => this.objectID == relatable.objectID
             case null => false
        }
    }
}

object noun extends RelatableProxy[ZextObject] {
    override def resolve = RuleContext._noun.asInstanceOf[ZextObject]
}

def GetTargets() : Seq[Relatable] = {
    RuleContext._nouns
}


object subject extends RelatableProxy[Relatable]{
    override def resolve = RuleContext._subject
}

object secondNoun extends RelatableProxy[ZextObject] {
    override def resolve = RuleContext._secondNoun.asInstanceOf[ZextObject]
}

object act extends RelatableProxy[Action] {
    override def resolve = RuleContext._action
}

// use these when you're not assuming that the target of an action is a zext object
object arg1 extends RelatableProxy[Relatable] {
    override def resolve = RuleContext._noun
}

object arg2 extends RelatableProxy[Relatable] {
    override def resolve = RuleContext._secondNoun
}


object RuleContext {

    private[Zext] var _action : Action = null
    private[Zext] var _noun: Relatable = null
    private[Zext] var _secondNoun: Relatable = null
    private[Zext] var _nouns : Seq[Relatable] = Array[Relatable]()
    private[Zext] var _subject : Relatable = null
    private[Zext] var _first: Boolean = false
    private[Zext] var _silent: Boolean = false
    private[Zext] var _location: ZContainer = nowhere

    private[Zext] def SetContext(ctx : RuleContext) : Unit = {
        _action = ctx.action
        _noun = ctx.nouns.headOption.orNull
        _secondNoun = ctx.nouns.lift(1).orNull
        _nouns = ctx.nouns
        _silent = ctx.silent
        _location = ctx.location
        _subject = ctx.subject
    }


    def Redirect(t1 : ZextObject, t2 : ZextObject = null) : RuleContext = {
        InheritContext(target = t1, target2 = t2)
    }

    def InheritContext(action: Action = null, subject: Relatable = null, target: Relatable = null, target2: Relatable = null, silent: Option[Boolean] = None, location: ZContainer = null): RuleContext = {
        val currentContext = GetCurrentRuleContext()
        //val t1 = target.getOrElse(currentContext.nouns(0))
        //val t2 = target2.getOrElse(currentContext.nouns(1))

        val targets = ConsolidateTargets(target, target2)
        val currentAction = if(action == null) currentContext.action else action
        val currentLocation = if (location == null) currentContext.location else location
        val currentSubject = if (subject == null) currentContext.subject else subject
        val currentSilence = if (silent.isDefined) silent.get else currentContext.silent
        assert(currentSubject != null, "you must specify a subject if there isn't a current rule context")
        assert(currentLocation != null, "you need to specify a location if there isn't a current rule context")
        RuleContext(currentAction, currentSubject, targets, currentSilence, currentLocation)
    }

    def GetCurrentRuleContext() = RuleContext(_action, _subject, _nouns, _silent, _location)

    def first : Boolean =  _first
    def silent : Boolean = _silent
    def location : ZContainer = _location
}


case class RuleContext(action: Action, subject : Relatable, nouns : Seq[Relatable], silent: Boolean, location : ZContainer)

object Rule {

    var blackboard : Any = null

    class ActionRuleSet {
        // this is the order they're executed in
        val applyingRules = ArrayBuffer[ActionRule[?]]()
        val beforeRules = ArrayBuffer[ActionRule[?]]()
        val insteadRules = ArrayBuffer[ActionRule[?]]()
        val checkRules = ArrayBuffer[ActionRule[?]]()
        val reportRules = ArrayBuffer[ActionRule[?]]()
        val executeRules = ArrayBuffer[ActionRule[?]]()
        val afterRules = ArrayBuffer[ActionRule[?]]()

        def GetAllRules() = {
            Array(beforeRules, insteadRules, checkRules, reportRules, executeRules, afterRules)
        }
    }

    val ruleSets = new mutable.HashMap[MetaAction[?], ActionRuleSet]()
    val alwaysRuleSet = ActionRuleSet()




    def SortByPrecedence(possible: ArrayBuffer[ActionRule[?]]): Seq[ActionRule[?]] = {
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


    def ExecuteRuleControls(sortedRules : Seq[ActionRule[?]]) : ExecutionResult[?] = {
        var result : ResultAndControl[?] = null

        for (rule <- sortedRules) {
            result = rule.exec
            result.control match {
                case RuleControl.Continue =>
                case RuleControl.Stop => return ExecutionResult(false, result.returned)
                case RuleControl.Replace => return ExecutionResult(true, result.returned)
                case _ => ???
            }
        }

        val returned = if(result != null) result.returned else null
        ExecutionResult(true, returned)
    }


    def GetPossibleRules(context : RuleContext, rules: ArrayBuffer[ActionRule[?]]) : Seq[ActionRule[?]] = {
        val previousContext = GetCurrentRuleContext()
        val previousFirst = _first
        SetContext(context)


        val possibleRules = rules.filter{ rule =>
            _first = rule.first
            rule.possible
        }
        val sorted = SortByPrecedence(possibleRules)

        _first = previousFirst
        SetContext(previousContext)

        sorted
    }

    def RunRule(context : RuleContext, rules: ArrayBuffer[ActionRule[?]]): ExecutionResult[?] = {
        val possibleRules = GetPossibleRules(context, rules)
        val previousContext = GetCurrentRuleContext()

        SetContext(context)
        val result = ExecuteRuleControls(possibleRules)
        SetContext(previousContext)

        result
    }

    // this is different because applying rules only run if possible, while normal rules only don't run if impossible.
    def RunApplyingRule(context: RuleContext, rules: ArrayBuffer[ActionRule[?]]): ExecutionResult[?] = {
        val possibleRules = GetPossibleRules(context, rules)
        val previousContext = GetCurrentRuleContext()

        SetContext(context)
        val result = ExecuteRuleControls(possibleRules)
        SetContext(previousContext)

        ExecutionResult(result.res && possibleRules.nonEmpty, result.ret)
    }


    def RunApplyingBeforeRules(userCommand : Command): Unit = {

    }

    def RunApplyingRules(userCommand : Command): Unit = {
        // get all actions with applying rules
        val applyingActions = Actions.allActions.filter(ruleSets(_).applyingRules.nonEmpty)

        for(action <- applyingActions){
            val applyingRules = ruleSets(action).applyingRules
            val allThings = Relatable.GetAll[Thing]

            for(thing <- allThings){
                val thingLocation = thing.location
                val ruleContext = new RuleContext(action, nothing, Array(thing), false, thingLocation)
                if(RunApplyingRule(ruleContext, applyingRules).res)
                    ExecuteAction(RuleContext(action, nothing, Array(thing), false, thingLocation))
            }
        }
    }



    case class ExecutionResult[T](res : Boolean, ret : T)

    // convenience for not having to create an array.
    def ExecuteAction(action: Action, subject: Relatable = null, target: Relatable = null, target2: Relatable = null, silent: Option[Boolean] = None, location: ZContainer = null): Boolean = {
          ExecuteAction(InheritContext(action, subject, target, target2, silent, location)).res
    }

    def ReplaceAction(action: Action, ruleContext: RuleContext = GetCurrentRuleContext()): Unit = {
        val ctx = RuleContext(action, ruleContext.subject, ruleContext.nouns, ruleContext.silent, ruleContext.location)
        val result = ExecuteAction(ctx)
        Break -> result.res
    }

    def ExecuteReturnAction[T, R](action: Action & Returns[T,R], subject: Relatable = null, target: Relatable = null, target2: Relatable = null, silent: Option[Boolean] = None, location: ZContainer = null)(arg : T): ExecutionResult[R] = {
        val previousArg = action.arg

        action.arg = arg
        val result = ExecuteAction(InheritContext(action, subject, target, target2, silent, location))
        action.arg = previousArg

        result.asInstanceOf[ExecutionResult[R]]
    }


    def ExecuteAction(context: RuleContext): ExecutionResult[?] = {

        val rule = context.action

        val set = ruleSets(rule)
        var allRules = if(!rule.isInstanceOf[SystemAction]) {
            set.GetAllRules().zip(alwaysRuleSet.GetAllRules()).map((a, b) => a concat b)
        } else {
            set.GetAllRules()
        }

        allRules = allRules.filter(_.nonEmpty)

         /*
            these are the rules for inform's rule execution, we are not following them, but it's useful to know anyway.

            Before: by default, make no decision. If stopped, no further rulebooks are run.
            (some internal visibility/accessibility sanity checks run here)
            Instead: by default, stop the action. If stopped, no further rulebooks are run.
            Check: by default, make no decision. If stopped, no further rulebooks are run.
            Carry Out: by default, make no decision. If stopped, other rulebooks continue.
            After: by default, stop the action. If stopped, no further rulebooks are run.
            Report: by default, make no decision.
          */

         val previousBlackboard = blackboard

         var result : ExecutionResult[?] = ExecutionResult(true, null)
         for(rules <- allRules) {
             val setResult = RunRule(context, rules)
             // only assign next result if a rule executed
             // this logic could still be messed up i think
             if(setResult.ret != null)
                result = setResult

             if (!result.res) {
                 blackboard = previousBlackboard
                 return result
             }
         }

         blackboard = previousBlackboard
         result
    }


/*
     // for running a ruleset in another rule
     def ExecuteSubRules(rules : ArrayBuffer[ActionRule[?]], ruleContext : RuleContext) : RuleControl = {

         def runRules(sortedRules: Seq[ActionRule[?]]): RuleControl = {
             for (rule <- sortedRules) {
                 val result = rule.exec
                 result match {
                     case RuleControl.Continue =>
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
*/

    private[Zext] inline def ConsolidateTargets(target: Relatable, target2: Relatable) : Array[Relatable] = {
        // target2 must be null if target is null
        if (target == null)
            require(target2 == null)

        if (target != null && target2 != null)
            Array(target, target2)
        else if (target != null)
            Array(target)
        else
            Array[Relatable]()
    }


}

abstract class Rule {
    var disabled = false
    var definitionPosition : String = null
}


enum QueryPrecedence:
    case Generic, Class, SecondClass, Property, SecondProperty, Content, Object, SecondObject, Location, Action, Context


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

class ConditionWithAction(val action : Action, condition: => Boolean, queryType: QueryPrecedence) extends Condition(condition, queryType)

object Condition {
    // inform's precedence is something like
    // location > object > property > class > generic

    inline implicit def fromBoolean(inline b:  Boolean): Condition = new Condition(b, QueryPrecedence.Generic)
    inline implicit def fromObject(inline z:  Relatable): Condition = new Condition(z == noun, QueryPrecedence.Object)
    inline def fromSecondObject(inline z:  Relatable): Condition = new Condition(z == secondNoun, QueryPrecedence.SecondObject)
    inline implicit def fromObjectArray(inline az:  Seq[ZextObject]): Condition = new Condition(az.contains(noun), QueryPrecedence.Object)
    inline implicit def fromProperty(inline p: Property): Condition = new Condition(noun is p?, QueryPrecedence.Property)
    inline def fromSecondProperty(inline p: Property): Condition = new Condition(secondNoun is p?, QueryPrecedence.SecondProperty)
    inline implicit def fromLocation(inline r:  Room): Condition = new Condition(r == noun, QueryPrecedence.Location)
    inline implicit def fromRegion(inline r:  RoomRegion): Condition = new Condition(r == noun, QueryPrecedence.Location)
    inline implicit def fromClassHolder(inline ch:  ZextObjectClassHolder): Condition = ch.createCondition(QueryPrecedence.Class)
    inline implicit def fromConditionHelper(inline helper:  ConditionHelper): Condition = helper.createCondition(QueryPrecedence.Generic)
    inline implicit def fromQuery(inline query:  RelationQuery[?,?]) : Condition = new Condition(query.evaluate(), query.relation.precedence)

    inline implicit def fromAction(inline action: Action): Condition = new ConditionWithAction(action, ???, QueryPrecedence.Action)


    type ConditionTypes = Relatable | RelatableProxy[?] | ConditionHelper

    inline implicit def fromTuple(inline t: (ConditionTypes, ConditionTypes)): Condition = {

        val firstPredicate : Condition = t._1 match {
            case anythingFirst : ZextObject if anythingFirst == anything => { val c = Condition(true, QueryPrecedence.Generic); c.specificity = 0; c}
            case classHolder : ZextObjectClassHolder => classHolder.createCondition(QueryPrecedence.Class)
            case relatableProxy: RelatableProxy[?] => fromObject(relatableProxy.resolve)
            case property: Property => fromProperty(property)
            case relatable: Relatable => fromObject(relatable)
            case helper : ConditionHelper => helper.createCondition(QueryPrecedence.Generic)
        }

        val secondPredicate: Condition = t._2 match {
            case anythingFirst : ZextObject if anythingFirst == anything => { val c = Condition(true, QueryPrecedence.Generic); c.specificity = 0; c}
            case classHolder : ZextObjectClassHolder => classHolder.createCondition(QueryPrecedence.SecondClass)
            case relatableProxy: RelatableProxy[?] => fromSecondObject(relatableProxy.resolve)
            case property: Property => fromSecondProperty(property)
            case relatable: Relatable => fromSecondObject(relatable)
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


    inline def ofDebug[T <: ZextObject | Container](name : String)(using tt: TypeTest[ZextObject | Container, T]) : ZextObjectClassHolder = {
        val depth = Macros.depth[T, ZextObject, Container] // depth of container is -1, which is maybe not expected
        val typeName = Macros.typeName[T]
        //println(s"making of $typeName with name $name with depth $depth")
        new ZextObjectClassHolder(tt, depth, name)
    }


    // this is for querying whether a specific object has a type
    def isZextObjectOf[T : TT as tt](target : => ZextObject, queryType: QueryPrecedence = QueryPrecedence.Class): Condition = {
        val condition = new Condition(
            {
                val success = tt.test(target)
                success
            }
            , queryType)
        condition.specificity = Macros.depth[T, ZextObject, Container]
        condition
    }
}

def NarrowControls(ctrl : ResultAndControl[?] | Any) : ResultAndControl[?] = {
    ctrl match {
        case resultAndControl: ResultAndControl[?] => resultAndControl
        case any : Any => ResultAndControl(any, Default)
    }
}

class ActionRule[T](body : T => (ResultAndControl[?] | Any), val conditions : Array[Condition], defaultControl : RuleControl, hasReturns : Boolean) extends Rule{
    var first = true

    def specificity = {
        conditions.map( _.specificity ).sum
    }

    def precedence ={
        conditions.map(_.precedence).foldLeft(0)( _ max _ )
    }

    def possible : Boolean = {
        try {
            conditions.forall( _.evaluate )
        } catch {
            case e: Throwable =>
                System.err.println(s"Error $e from condition at: .(" + definitionPosition + ")")
                throw e
        }
    }

     def exec : ResultAndControl[?] = {
        val previous = _first
        _first = this.first
        this.first = false
         val action = GetCurrentRuleContext().action

         var ret : ResultAndControl[?] = try {

             action match {
                 case passthrough: Passthrough[?] if hasReturns =>
                     val returns = passthrough.asInstanceOf[Returns[T,T]]
                     val result = NarrowControls(body(returns.arg))
                     returns.arg = result.returned.asInstanceOf[T]
                     result

                 case returns : Returns[?, ?] if hasReturns =>
                     NarrowControls(body(returns.arg.asInstanceOf[T]))

                 case _ =>
                     NarrowControls(body(().asInstanceOf[T]))
             }
         } catch {
             case ex: ControlException[?] =>
                 action match {
                     case passthrough: Passthrough[?] if hasReturns =>
                         val returns = passthrough.asInstanceOf[Returns[T, T]]
                         val result = ex.resultAndControl
                         returns.arg = result.returned.asInstanceOf[T]
                         result
                     case _ => ex.resultAndControl
                 }
         }

         if(ret.control == RuleControl.Default)
             ret = ResultAndControl(ret.returned, defaultControl)

        _first = previous
        ret
    }
}



// debug actions will have the entire set of objects in their scope
trait DebugAction {
    this : Action =>
}


// this trait prevents running of always rules, ie things that query the 'act' object
trait SystemAction {
    this: Action =>
}

trait Passthrough[T] {
    this: Returns[T,T] =>
}

trait Returns[T, R] {
    this: Action =>
    var arg : T = null.asInstanceOf[T]
    def apply(context : T) = new ConditionWithAction(this,  arg == context, QueryPrecedence.Context)
    def base : Action = this.asInstanceOf[Action]
}


class MetaAction[NounType <: Relatable : TT as _tt](val targets : Int) extends Rule with Relatable {
    val nounTypeTest = _tt

    def implicitTargetSelector: SetComprehension[NounType] = null
    def implicitSubjectSelector: SetComprehension[NounType] = null
    var disambiguationHint: ParsableType => Boolean = null
    ruleSets(this) = new ActionRuleSet
    allMetaActions.addOne(this)

}

class Action(targets : Int, val verbs : String*) extends MetaAction[ZextObject](targets) with ParsableType(PartOfSpeech.verb) {
    allActions.addOne(this)
    override def toString = if(verbs.nonEmpty) verbs(0) else this.getClass.toString
}




abstract class CustomAction(targets: Int, verbs : String*) extends Action(targets, verbs*) {

    // when encountering a custom action, allow the user to intercept the raw text and parse results
    def intercept(rawInput : String, parseResult: ParseResult) : Command

}