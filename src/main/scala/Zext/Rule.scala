package Zext

import Zext.Actions.{UnderstandAlias, allMetaActions, examining}
import Zext.EverythingParser.ParseResult
import Zext.Infliction.*
import Zext.Infliction.RuleControl.Default
import Zext.Interpreter.Say
import Zext.Parser.*
import Zext.QueryPrecedence.{Action, ActionContext}
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

import scala.compiletime.summonFrom


object RelatableProxy {
    implicit def toRelatable[T <: Relatable](r : RelatableProxy[T]): T = r.resolve
}

@deprecated
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


object RuleContext {

    import scala.compiletime.summonInline


    private[Zext] var _first: Boolean = false

    // covariance is list of cat can be passed into list of animal

    inline def Redirect[S <: Relatable, N1 <: Relatable, N2 <: Relatable, A <: S, B <: N1, C <: N2](subject : A = null, target : B = null, secondTarget : C = null)
                                                                                (using RuleContext[S,N1,N2]) : RuleContext[S,N1,N2] = {
        InheritContext(subject = Option(subject), target = Option(target), target2 = Option(secondTarget))
    }

    def ReplaceActionContext[S <: Relatable, N1 <: Relatable, N2 <: Relatable](action: MetaAction[S,N1,N2,?,?] ,
                                                                              subject: Option[S] = None, target: Option[N1] = None, target2: Option[N2] = None, silent: Option[Boolean] = None, location: ZContainer = null)
                      (using currentContext: RuleContext[?, ?, ?]): RuleContext[S, N1, N2] = {

        val t1 = target.getOrElse(currentContext.nouns.lift(0).orNull)
        val t2 = target2.getOrElse(currentContext.nouns.lift(1).orNull)

        val targets = ConsolidateTargets(t1, t2)
        val currentLocation = if (location == null) currentContext.location else location
        val currentSubject = if (subject.isEmpty) currentContext.subject else subject.get
        val currentSilence = if (silent.isDefined) silent.get else currentContext.silent
        assert(currentSubject != null, "you must specify a subject if there isn't a current rule context")
        assert(currentLocation != null, "you need to specify a location if there isn't a current rule context")
        val ctx = RuleContext(action, currentSubject, targets, currentSilence, currentLocation)
        action.checkContext(ctx)
        ctx
    }


    def InheritContext[S <: Relatable, N1 <: Relatable, N2 <: Relatable]()(using currentContext: RuleContext[S, N1, N2]): RuleContext[S, N1, N2] = {
        currentContext
    }

    // //only changing the action changes the type of the rule context.
    def InheritContext[S <: Relatable, N1 <: Relatable, N2 <: Relatable](subject: Option[S] = None, target: Option[N1] = None, target2: Option[N2] = None, silent: Option[Boolean] = None, location: ZContainer = null)
                      (using currentContext: RuleContext[S,N1,N2]) : RuleContext[S,N1,N2] = {

        val t1 = target.getOrElse(currentContext.nouns.lift(0).orNull)
        val t2 = target2.getOrElse(currentContext.nouns.lift(1).orNull)
        val currentSubject = subject.getOrElse(currentContext.subject)

        val targets = ConsolidateTargets(t1, t2)
        val currentLocation = if (location == null) currentContext.location else location
        val currentSilence = if (silent.isDefined) silent.get else currentContext.silent
        assert(currentSubject != null, "you must specify a subject if there isn't a current rule context")
        assert(currentLocation != null, "you need to specify a location if there isn't a current rule context")
        RuleContext(currentContext.action, currentSubject, targets, currentSilence, currentLocation)
    }


    // it think this ruins everything.
    //val DefaultContext: RuleContext[Nothing, Nothing, Nothing] = ???
    //given RuleContext[Nothing, Nothing, Nothing] = DefaultContext

    inline def context[S,N1,N2](using ctx : RuleContext[S,N1,N2]) : RuleContext[S,N1,N2] = ctx

    inline def act(using ctx: RuleContext[?,?,?]) : MetaAction[Relatable,Relatable,Relatable,Unit,Unit] = {
        ctx.action.asInstanceOf[MetaAction[Relatable,Relatable,Relatable,Unit,Unit]]
    }

    // @todo improve this, we basically dont want to ever cast, and this should be a query in the conditions list that automatically constrains the rule context
    inline def noun[N](using ctx: RuleContext[?, ?, ?]): N & Zebra[N] = {
        val ret = ctx.nouns(0).asInstanceOf[N]
        inline ret match {
            case nothing: Nothing => scala.compiletime.error("Action doesn't have a noun")
        }
        ret.asInstanceOf[N & Zebra[N]]
    }


    inline def noun[S <: Relatable, N1 <: Relatable, N2 <: Relatable](using ctx: RuleContext[S, N1, N2] ): N1 & Zebra[N1] = {
      val ret = ctx.nouns(0).asInstanceOf[N1]
      inline ret match {
        case nothing: Nothing => scala.compiletime.error("Action doesn't have a noun")
      }
      ret.asInstanceOf[N1 & Zebra[N1]]
    }

    inline def secondNoun[S <: Relatable, N1 <: Relatable, N2 <: Relatable](using ctx: RuleContext[S, N1, N2]): N2 & Zebra[N2] = {
      val ret = ctx.nouns(1).asInstanceOf[N2]
      inline ret match {
        case nothing: Nothing => scala.compiletime.error("Action doesn't have a second noun")
      }
      ret.asInstanceOf[N2 & Zebra[N2]]
    }


    inline def secondNoun[N](using ctx: RuleContext[?, ?, ?]): N & Zebra[N] = {
        val ret = ctx.nouns(1).asInstanceOf[N]
        inline ret match {
            case nothing : Nothing => scala.compiletime.error("Action doesn't have second noun")
        }
        ret.asInstanceOf[N & Zebra[N]]
    }

    inline def subject[S](using ctx: RuleContext[?, ?, ?]): S & Zebra[S] = {
        ctx.subject.asInstanceOf[S & Zebra[S]]
    }

    inline def subject[S <: Relatable, N1 <: Relatable, N2 <: Relatable](using ctx: RuleContext[S, N1, N2]): S & Zebra[S] = {
      ctx.subject.asInstanceOf[S & Zebra[S]]
    }

    inline def arg(using ctx: RuleContext[?, ?, ?]): Any = {
        ctx.action.arg
    }


    inline def location(using ctx : RuleContext[?,?,?]) : ZContainer = ctx.location
    inline def silent(using ctx : RuleContext[?,?,?]) : Boolean = ctx.silent
    inline def nouns(using ctx: RuleContext[?, ?, ?]): Seq[Relatable] = ctx.nouns

    def first : Boolean =  _first
}



case class RuleContext[S <: Relatable, N1  <: Relatable, N2  <: Relatable](action: MetaAction[S,N1,N2, ?, ?], subject : Relatable, nouns : Seq[Relatable], silent: Boolean, location : ZContainer)



object Rule {

    var blackboard : Any = null

    enum RuleType {
        case  before, check, instead, report, inflict, after, applying,
    }

    val alwaysRuleSet = ActionRuleSet[Relatable, Relatable, Relatable, Unit, Unit]


    def SortByPrecedence(possible: Seq[AnyRule]): Seq[AnyRule] = {
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
        val flat = sortedPrecedenceSets.sortBy( kv => kv._1 ).reverse.flatten(using kv => kv._2)

        flat
    }


    def ExecuteRuleControls(context : RuleContext[?,?,?], sortedRules : Seq[AnyRule]) : ExecutionResult[?] = {
        var result : ResultAndControl[?] = null

        for (rule <- sortedRules) {
            result = rule.exec(context)
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


    def GetPossibleRules(context : RuleContext[?,?,?], rules: Seq[AnyRule]) : Seq[AnyRule] = {
        val previousFirst = _first

        val possibleRules = rules.filter{ rule =>
            _first = rule.first
            rule.possible(context)
        }
        val sorted = SortByPrecedence(possibleRules)

        _first = previousFirst

        sorted
    }

    def RunRule(context : RuleContext[?,?,?], rules: Seq[AnyRule]): ExecutionResult[?] = {
        val possibleRules = GetPossibleRules(context, rules)
        val result = ExecuteRuleControls(context, possibleRules)

        result
    }

    // this is different because applying rules only run if possible, while normal rules only don't run if impossible.
    def RunApplyingRule(context: RuleContext[?,?,?], rules: Seq[AnyRule]): ExecutionResult[?] = {
        val possibleRules = GetPossibleRules(context, rules)
        val result = ExecuteRuleControls(context, possibleRules)
        ExecutionResult(result.res && possibleRules.nonEmpty, result.ret)
    }

    def RunApplyingRules(userCommand : Command): Unit = {
        // get all actions with applying rules
        val applyingActions = Actions.allMetaActions.filter(_.ruleSet.applying.nonEmpty)

        for(action <- applyingActions){
            val applyingRules = action.ruleSet.applying
            val allThings = Relatable.GetAll[Thing]

            for(thing <- allThings){
                val thingLocation = thing.location
                val ruleContext = new RuleContext(action, nothing, Array(thing), false, thingLocation)
                if(RunApplyingRule(ruleContext, applyingRules.toSeq).res)
                    ExecuteAction(RuleContext(action, nothing, Array(thing), false, thingLocation))
            }
        }
    }



    case class ExecutionResult[T](res : Boolean, ret : T)


    // the reason this requires the action again is so we can do type inference on the argument and return type.
    // it is not required to specify the action twice for normal execution.
    def ExecuteReturnAction[S <: Relatable, N1 <: Relatable, N2 <: Relatable, T,R](action : MetaAction[S,N1,N2,T,R], context: RuleContext[S,N1,N2])(arg : T) :ExecutionResult[R] = {
        val previousArg = action.arg
        action.arg = arg
        val result = ExecuteAction(ReplaceActionContext(action)(using context))
        action.arg = previousArg

        result.asInstanceOf[ExecutionResult[R]]
    }


    def ExecuteAction(context: RuleContext[?,?,?]): ExecutionResult[?] = {

        val action = context.action

        val set = action.ruleSet
        var allRules : Array[ArrayBuffer[AnyRule]] = if(!action.isInstanceOf[SystemAction]) {
            set.GetAllRules().zip(alwaysRuleSet.GetAllRules()).map((a, b) => a concat b)
        } else {
            set.GetAllRules().asInstanceOf[Array[ArrayBuffer[AnyRule]]]
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
             val setResult = RunRule(context, rules.toSeq)
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
    var sourceCode : String = null
}


enum QueryPrecedence:
    case Generic, Class, SecondClass, Property, SecondProperty, Content, Object, SecondObject, Location, Action, ActionContext


class Condition[S <: Relatable, N1 <: Relatable, N2 <: Relatable](private val conditionBody: RuleContext[S,N1,N2] ?=> Boolean, var queryType: QueryPrecedence) {

    // evil casting probably wont work.
    def evaluate(using context: RuleContext[?,?,?]) = conditionBody(using context.asInstanceOf[RuleContext[S,N1,N2]])
    var specificity = 1
    def precedence = queryType.ordinal


    def &&(other: Condition[S,N1,N2]): Condition[S,N1,N2] = {
        // combine predicates
        val precedence = if(this.queryType.ordinal > other.queryType.ordinal) this.queryType else other.queryType
        val c = new Condition[S,N1,N2](this.conditionBody && other.conditionBody, precedence)
        c.specificity = this.specificity + other.specificity
        c
    }
}

abstract class ConditionHelper {
    def createCondition(queryPrecedence: QueryPrecedence) : AnyCondition
}

case class Priority(amount : Int) extends ConditionHelper {
    override def createCondition(queryPrecedence: QueryPrecedence) = {
        val c = new Condition(true, queryPrecedence)
        c.specificity = amount
        c
    }
}


object Condition {

    // condition needs a context so it can ask about noun context variables.
    inline implicit def fromBoolean(inline b:  Boolean): AnyCondition = new Condition(b, QueryPrecedence.Generic)
    inline implicit def fromObject(inline z:  ZextObject): AnyCondition = new Condition(z == noun, QueryPrecedence.Object)
    inline def fromSecondObject(inline z:  ZextObject): AnyCondition = new Condition(z == secondNoun, QueryPrecedence.SecondObject)
    inline implicit def fromObjectArray(inline az:  Seq[ZextObject]): AnyCondition = new Condition(az.contains(noun), QueryPrecedence.Object)
    @deprecated("just ask noun is p?") inline implicit def fromProperty(inline p: Property): AnyCondition = new Condition(Relatable.QueryRelation(property_having, noun, p), QueryPrecedence.Property)
    @deprecated("just ask secondNoun is p?") inline def fromSecondProperty(inline p: Property): AnyCondition = new Condition(Relatable.QueryRelation(property_having, secondNoun, p), QueryPrecedence.SecondProperty)
    inline implicit def fromLocation(inline r:  Room): AnyCondition = new Condition(r == noun, QueryPrecedence.Location)
    inline implicit def fromRegion(inline r:  RoomRegion): AnyCondition = new Condition(r == noun, QueryPrecedence.Location)
    inline implicit def fromClassHolder(inline ch:  ZextObjectClassHolder[?]): AnyCondition = ch.createCondition(QueryPrecedence.Class)
    inline implicit def fromConditionHelper(inline helper:  ConditionHelper): AnyCondition = helper.createCondition(QueryPrecedence.Generic)
    inline implicit def fromQuery(inline query:  RelationQuery[?,?]) : AnyCondition = new Condition(query.evaluate, query.relation.precedence) // act is loud should become a relation query


    type ConditionTypes = ZextObject | RelatableProxy[ZextObject] | ConditionHelper | Property

    inline implicit def fromTuple[X,Y,Z](inline t: (ConditionTypes, ConditionTypes)): AnyCondition = {

        val firstPredicate : AnyCondition = t._1 match {
            case anythingFirst : ZextObject if anythingFirst == anything => { val c = Condition(true, QueryPrecedence.Generic); c.specificity = 0; c}
            case classHolder : ZextObjectClassHolder[?] => classHolder.createCondition(QueryPrecedence.Class)
            case relatableProxy: RelatableProxy[ZextObject] => fromObject(relatableProxy.resolve)
            case property: Property => fromProperty(property)
            case relatable: ZextObject => fromObject(relatable)
            case helper : ConditionHelper => helper.createCondition(QueryPrecedence.Generic)
        }

        val secondPredicate: AnyCondition = t._2 match {
            case anythingFirst : ZextObject if anythingFirst == anything => { val c = Condition(true, QueryPrecedence.Generic); c.specificity = 0; c}
            case classHolder : ZextObjectClassHolder[?] => classHolder.createCondition(QueryPrecedence.SecondClass)
            case relatableProxy: RelatableProxy[ZextObject] => fromSecondObject(relatableProxy.resolve)
            case property: Property => fromSecondProperty(property)
            case relatable: ZextObject => fromSecondObject(relatable)
            case helper : ConditionHelper => helper.createCondition(QueryPrecedence.Generic)
        }

        firstPredicate && secondPredicate.asInstanceOf[firstPredicate.type]
    }



    // these have to be macros to get the proper depth for T
     inline def of[T <: ZextObject | Container](using tt: TypeTest[ZextObject | Container, T]) : ZextObjectClassHolder[T] = {
         val depth = Macros.depth[T, ZextObject, Container]
         val typeName = Macros.typeName[T]
         new ZextObjectClassHolder[T](tt, depth, typeName)
    }


    inline def ofDebug[T <: ZextObject | Container](name : String)(using tt: TypeTest[ZextObject | Container, T]) : ZextObjectClassHolder[T] = {
        val depth = Macros.depth[T, ZextObject, Container] // depth of container is -1, which is maybe not expected
        val typeName = Macros.typeName[T]
        //println(s"making of $typeName with name $name with depth $depth")
        new ZextObjectClassHolder[T](tt, depth, name)
    }


    // this is for querying whether a specific object has a type
    def isZextObjectOf[T : TT as tt](target : => ZextObject, queryType: QueryPrecedence = QueryPrecedence.Class): AnyCondition = {
        val condition = new Condition( tt.test(target), queryType)
        condition.specificity = Macros.depth[T, ZextObject, Container]
        condition
    }
}

def WrapDefault(any : Any) : ResultAndControl[?] = {
    ResultAndControl(any, Default)
}

type AnyRule = ActionRule[?,?,?,?,?]


class ActionRule[S, N1, N2, T, R](body : ReturnsBodyType[S,N1,N2,T,R], val conditions : Array[RuleQuestion[S,N1,N2]], defaultControl : RuleControl, hasReturns : Boolean) extends Rule {
    var first = true

    // should be ok to pass null here becuase we're not using the context?
    // i mean the context is required to generate the condition, which is annoying
    def specificity = {
        conditions.map( _(using null).specificity ).sum
    }

    def precedence = {
        conditions.map( _(using null).precedence).foldLeft(0)( _ max _ )
    }

    def possible(context : RuleContext[?,?,?]) : Boolean = {
        try {
            // this probably not going to work.
            // the context for the rule question provides the nouns, but it can be any context i think?
            val typedCtx = context.asInstanceOf[RuleContext[S,N1,N2]]
            for(ruleQ <- conditions){
                val condition = ruleQ(using typedCtx)
                if(!condition.evaluate(using context)) return false
            }
            true
        } catch {
            case e: Throwable =>
                System.err.println(s"Error $e from condition at: .(" + definitionPosition + s")\n with code: $sourceCode\n")
                throw e
        }
    }

    def exec(context: RuleContext[?, ?, ?]): ResultAndControl[?] = {

        val typedContext = context.asInstanceOf[RuleContext[S, N1, N2]]

        val previous = _first
        _first = this.first
        this.first = false

        var ret: ResultAndControl[?] = try {
            body match {
                case _ if hasReturns =>
                    val typedAction = context.action.asInstanceOf[MetaAction[?, ?, ?, T, ?]]
                    typedAction match {
                        case passthrough: Passthrough[?] =>
                            val result = body(typedAction.arg)(using typedContext)
                            typedAction.arg = result.asInstanceOf[T]
                            WrapDefault(result)
                        case _ =>
                            val result = body(typedAction.arg)(using typedContext)
                            WrapDefault(result)
                    }

                case unitBody : ReturnsBodyType[?,?,?,Unit,Unit] =>
                    unitBody(())(using typedContext)
                    WrapDefault(())
            }

        } catch {
            case ex: ControlException[?] =>
                body match {
                    case returnsBody: ReturnsBodyType[?, ?, ?, ?, ?] if hasReturns =>
                        val typedAction = context.action.asInstanceOf[MetaAction[?, ?, ?, T, ?]]
                        typedAction match {
                            case _: Passthrough[?] =>
                                typedAction.arg = ex.resultAndControl.returned.asInstanceOf[T]
                            case _ =>
                        }
                    case _ =>
                }

                ex.resultAndControl
        }

        if (ret.control == RuleControl.Default)
            ret = ResultAndControl(ret.returned, defaultControl)

        _first = previous
        ret
    }
}

type SelfAction[S] = MetaAction[S, Nothing, Nothing, Unit, Unit]
type SingleAction[T] = MetaAction[ZextObject, T, Nothing, Unit, Unit]
type DoubleAction[T1,T2] = MetaAction[ZextObject, T1, T2, Unit, Unit]
type AnyAction = MetaAction[?,?,?,?,?]
type AnyCondition = Condition[?,?,?]

// debug actions will have the entire set of objects in their scope
trait DebugAction {
    this : AnyAction =>
}


// this trait prevents running of always rules, ie things that query the 'act' object
// verbs for system actions are also not able to be understood in commands
trait SystemAction {
    this: AnyAction =>
}

// passthrough actions will pass through the argument from one rule to the next.
trait Passthrough[T] {
    this: MetaAction[?,?,?,T,T] =>
}

class ActionWithContextCondition[S,N1,N2,T,R](val action : MetaAction[S,N1,N2,T,R], condition : => Boolean,  queryType: QueryPrecedence) extends Condition(condition, queryType)

class ActionRuleSet[S <: Relatable, N1 <: Relatable, N2 <: Relatable, T, R] {
    val applying = ArrayBuffer[ActionRule[S, N1, N2, T, R]]()
    val before = ArrayBuffer[ActionRule[S, N1, N2, T, R]]()
    val instead = ArrayBuffer[ActionRule[S, N1, N2, T, R]]()
    val check = ArrayBuffer[ActionRule[S, N1, N2, T, R]]()
    val report = ArrayBuffer[ActionRule[S, N1, N2, T, R]]()
    val inflict = ArrayBuffer[ActionRule[S, N1, N2, T, R]]()
    val after = ArrayBuffer[ActionRule[S, N1, N2, T, R]]()


    def addRule(rule :AnyRule, ruleType : RuleType): Unit = {
        val set = GetRuleSet(ruleType)
        set.addOne(rule.asInstanceOf[ActionRule[S,N1,N2,T,R]])
    }

    def GetRuleSet(ruleType: RuleType, always: Boolean = false): ArrayBuffer[ActionRule[S, N1, N2, T, R]] = {
        ruleType match {
            case RuleType.before => before
            case RuleType.applying => applying
            case RuleType.instead => instead
            case RuleType.report => report
            case RuleType.inflict => inflict
            case RuleType.after => after
            case RuleType.check => check
        }
    }

    def GetAllRules() = {
        Array(before, instead, check, report, inflict, after)
    }
}


class PendingContext[S <: Relatable, N1 <: Relatable, N2 <: Relatable, Takes, Returns](val action : MetaAction[S, N1, N2, Takes, Returns]) {
    private var _subject : Option[S] = None
    private var _noun : Option[N1] = None
    private var _secondNoun: Option[N2] = None
    private var _silent: Option[Boolean] = Some(false)
    private var _location : Option[ZContainer] = None

    def subject(s : S) : this.type = { _subject = Option(s);  this }
    def noun(n : N1)  : this.type = { _noun = Option(n); this }
    def secondNoun(n : N2) : this.type = { _secondNoun = Option(n); this }
    def silent(b : Boolean) : this.type = { _silent = Option(b); this }
    def location(c : ZContainer) : this.type = { _location = Option(c); this }

    def system: this.type = {
        _subject = Some(system.asInstanceOf[S])
        _silent = Some(false)
        _location = Some(nowhere)
        this
    }

    inline def result : Boolean = {
        val ctx = summonFrom {
            case given RuleContext[S,N1,N2] => context
            case _                          => RuleContext[S,N1,N2](action, _subject.get, Seq(_noun.get, _secondNoun.get), _silent.get, _location.get)
        }

        val pendingSubject = _subject.getOrElse(ctx.subject)
        val pendingNouns = ConsolidateTargets(_noun.orNull, _secondNoun.orNull)
        val pendingSilence = _silent.getOrElse(ctx.silent)
        val pendingLocation = _location.getOrElse(ctx.location)
        val pendingCtx = RuleContext(action, pendingSubject, pendingNouns, pendingSilence, pendingLocation)
        ExecuteAction(pendingCtx).res
    }

    inline def returned(arg : Takes): Returns = {
        val ctx = summonFrom {
            case given RuleContext[S, N1, N2] => context
            case _ => RuleContext[S, N1, N2](action, _subject.get, Seq(_noun.get, _secondNoun.get), _silent.get, _location.get)
        }

        val pendingSubject = _subject.getOrElse(ctx.subject)
        val pendingNouns = ConsolidateTargets(_noun.orNull, _secondNoun.orNull)
        val pendingSilence = _silent.getOrElse(ctx.silent)
        val pendingLocation = _location.getOrElse(ctx.location)
        val pendingCtx = RuleContext(action, pendingSubject, pendingNouns, pendingSilence, pendingLocation)
        ExecuteReturnAction(action, pendingCtx)(arg).ret
    }

    inline def execute(arg: Takes = null.asInstanceOf[Takes]): ExecutionResult[Returns] = {
        val ctx = summonFrom {
            case given RuleContext[S, N1, N2] => context
            case _ => RuleContext[S, N1, N2](action, _subject.get, Seq(_noun.get, _secondNoun.get), _silent.get, _location.get)
        }

        val pendingSubject = _subject.getOrElse(ctx.subject)
        val pendingNouns = ConsolidateTargets(_noun.orNull, _secondNoun.orNull)
        val pendingSilence = _silent.getOrElse(ctx.silent)
        val pendingLocation = _location.getOrElse(ctx.location)
        val pendingCtx = RuleContext(action, pendingSubject, pendingNouns, pendingSilence, pendingLocation)

        if(arg != null) {
            ExecuteReturnAction(action, pendingCtx)(arg)
        } else {
            ExecuteAction(pendingCtx).asInstanceOf[ExecutionResult[Returns]]
        }
    }

}

// a metaaction of relatables is also a metacation of thing
class MetaAction[S <: Relatable, N1 <: Relatable, N2 <: Relatable, Takes, Returns](val targets : Int, val verbs : String*) extends Rule with Relatable with ParsableType(PartOfSpeech.verb){

    def checkContext(ruleContext : RuleContext[?,?,?]): Unit = {
       // require(_stt.test(ruleContext.subject), "Incompatible subject type")
       // require(_n1tt.test(ruleContext.nouns.lift(0).orNull), "Incompatible first noun type")
       // require(_n2tt.test(ruleContext.nouns.lift(1).orNull), "Incompatible second noun type")
    }

    private[Zext] var arg: Takes = null.asInstanceOf[Takes]
    def apply(context: Takes) = new ActionWithContextCondition(this, arg == context, QueryPrecedence.ActionContext)

    def implicitTargetSelector: SetComprehension[N1] = null
    def implicitSubjectSelector: SetComprehension[N2] = null
    var disambiguationHint: ParsableType => Boolean = null

    val ruleSet = new ActionRuleSet[S, N1, N2, Takes, Returns]

    // @todo add execute fuction with modifiable context chaining eg: execute.redirect.result or execute.inherit.returned, execute.targets().subject().noun().location()
    def run : PendingContext[S,N1,N2,Takes,Returns] = {
        PendingContext(this)
    }

    allMetaActions.addOne(this)

}


class Action(targets : Int, val verbs : String*) extends MetaAction[ZextObject,ZextObject,ZextObject, Unit, Unit](targets, verbs*)  {
    override def toString = if(verbs.nonEmpty) verbs(0) else this.getClass.toString
}

class ReturnsAction[T,R](targets: Int, val verbs: String*) extends MetaAction[ZextObject, ZextObject, ZextObject, T, R](targets, verbs*)  {
    override def toString = if (verbs.nonEmpty) verbs(0) else this.getClass.toString
}




abstract class CustomAction(targets: Int, verbs : String*) extends Action(targets, verbs*) {

    // when encountering a custom action, allow the user to intercept the raw text and parse results
    def intercept(rawInput : String, parseResult: ParseResult) : Command

}