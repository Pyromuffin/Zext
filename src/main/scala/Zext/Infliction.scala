package Zext

import Zext.Relation.RelationQuery
import Zext.Rule.*
import Zext.RuleContext.{secondNoun, subject}
import zobjectifier.Macros.*

import scala.annotation.targetName
import scala.compiletime.codeOf
import scala.language.postfixOps
import scala.util.control.ControlThrowable


object Infliction {

  enum RuleControl {
    case Stop, Continue, Replace, Default
  }

  case class ResultAndControl[T](returned: T, control: RuleControl)


  case class ControlException[T](resultAndControl: ResultAndControl[T]) extends ControlThrowable

  trait ControlObject(ruleControl: RuleControl) {
    infix def ->[T](arg: T): T = throw ControlException(ResultAndControl(arg, ruleControl))
  }

  object Succeed extends ControlObject(RuleControl.Replace)
  object Fail extends ControlObject(RuleControl.Stop)
  object Continue extends ControlObject(RuleControl.Continue)

  object Break {
    infix def ->(arg: Boolean): Unit =  if(arg) succeed else fail
  }

  def stop: Unit = throw ControlException(ResultAndControl((), RuleControl.Stop))
  def fail: Unit = throw ControlException(ResultAndControl((), RuleControl.Stop))
  def succeed: Unit = throw ControlException(ResultAndControl((), RuleControl.Replace))
  def continue: Unit = throw ControlException(ResultAndControl((), RuleControl.Continue))

  def stop_return(res: Boolean): Unit = {
    if (res) succeed else  fail
  }

  def stop_unless(res: Boolean): Unit = {
    if (res) continue else fail
  }


  type UnitBodyType[S <: Relatable, N1 <: Relatable, N2 <: Relatable] = RuleContext[S, N1, N2] ?=> Unit
  type ReturnsBodyType[S <: Relatable, N1 <: Relatable, N2 <: Relatable, T, R] = T => RuleContext[S, N1, N2] ?=> R


  inline def CreateAlwaysRule[S<: Relatable, N1<: Relatable, N2<: Relatable](inline body: ReturnsBodyType[S, N1, N2, Unit, Unit], control: RuleControl, ruleType: RuleType, conditions: RuleQuestion*): Unit = {

        val typedBody = body.asInstanceOf[ReturnsBodyType[Relatable, Relatable, Relatable, Unit, Unit]]
        val typedConditions = conditions.toArray
        val rule = new ActionRule(typedBody, typedConditions, control, false)
        rule.definitionPosition = CodePosition()
        rule.sourceCode = codeOf(body)
        alwaysRuleSet.addRule(rule, ruleType)
  }



  inline def CreateUnitRule[S<: Relatable, N1<: Relatable, N2<: Relatable, T , R](inline firstArg: FirstArg[S,N1,N2,T,R], inline body: ReturnsBodyType[S, N1, N2, Unit, Unit], control: RuleControl, ruleType: RuleType, conditions: RuleQuestion*): Unit = {

    var conds = conditions

    inline firstArg match {
      case action: AnyAction =>
        val typedConditions = conditions.toArray
        val rule = new ActionRule(body, typedConditions, control, false)
        rule.definitionPosition = CodePosition()
        rule.sourceCode = codeOf(body)
        action.ruleSet.addRule(rule, ruleType)

      case ac: ActionWithContextCondition[?, ?, ?, ?, ?] =>
        conds = conds.appended(ac)
        val typedConditions = conditions.toArray
        val rule = new ActionRule(body, typedConditions, control, false)
        rule.definitionPosition = CodePosition()
        rule.sourceCode = codeOf(body)
        ac.action.ruleSet.addRule(rule, ruleType)

      case _ =>
        println(firstArg.getClass.getName + " huh?")
        //scala.compiletime.error("huh?")
    }

  }


  inline def CreateReturnsRule[S  <: Relatable,N1  <: Relatable,N2  <: Relatable,T,R](firstArg: FirstArg[S,N1,N2,T,R], inline body : ReturnsBodyType[S,N1,N2, T, R], control : RuleControl, ruleType : RuleType, conditions : RuleQuestion*) : Unit = {

    var conds = conditions

    inline firstArg match {
      case action: AnyAction =>
        val typedConditions = conds.toArray
        val rule = new ActionRule(body, typedConditions, control, true)
        rule.definitionPosition = CodePosition()
        rule.sourceCode = codeOf(body)
        action.ruleSet.addRule(rule, ruleType)

      case ac: ActionWithContextCondition[?,?,?,?,?] =>
        conds = conds.appended(ac)
        val typedConditions = conds.toArray
        val rule = new ActionRule(body, typedConditions, control, true)
        rule.definitionPosition = CodePosition()
        rule.sourceCode = codeOf(body)
        ac.action.ruleSet.addRule(rule, ruleType)

    }

  }

  type SelfAction[S <: Relatable] = MetaAction[S, Nothing, Nothing, Unit, Unit]
  type SingleAction[T <: Relatable] = MetaAction[ZextObject, T, Nothing, Unit, Unit]
  type DoubleAction[T1 <: Relatable, T2 <: Relatable] = MetaAction[ZextObject, T1, T2, Unit, Unit]
  type AnyAction = MetaAction[?, ?, ?, ?, ?]
  type AnyCondition = Condition[?, ?, ?]

  type ActionQuestion = RuleContext[?,?,?] ?=> MetaAction[Relatable, Relatable, Relatable, Unit, Unit]
  type RuleQuestion =  AnyCondition
  type FirstArg[S <: Relatable,N1 <: Relatable,N2 <: Relatable,T,R] = MetaAction[S, N1, N2, T, R] | ActionWithContextCondition[S,N1,N2,T,R]
  trait N1Constraint[T <: Relatable]
  trait N2Constraint[T <: Relatable]
  trait SConstraint[T <: Relatable]
  trait ConstraintCondition[SC <: Relatable, N1C <: Relatable, N2C <: Relatable]

  object ThingsInContainers extends ConstraintCondition[ZextObject, Thing, ZContainer]

  trait Infliction(defaultControl : RuleControl, ruleType: RuleType) {


    /*
    inline def apply[S <: Relatable, N1 <: Relatable, N2 <: Relatable, T, R, NounType](action: FirstArg[S, N1, N2, T, R], inline ofCondition : ZextObjectClassHolder[NounType],
                                                                                       inline conditions: RuleQuestion[S, NounType, N2]*)(inline body: UnitBodyType[S, NounType, N2]): Unit = {
      CreateUnitRule(action, _ => body, RuleControl.Continue, RuleType.before, conditions *)
    }
    */

    inline def always(inline conditions: RuleQuestion *)(inline body: UnitBodyType[Relatable, Relatable, Relatable]): Unit = {
      CreateAlwaysRule(_ => body, defaultControl, ruleType, conditions*)
    }

    inline def apply[S <: Relatable, N1 <: Relatable, N2 <: Relatable, T, R](
        inline action: FirstArg[S,N1,N2,T,R],
        inline conditions: RuleQuestion*)(inline body: UnitBodyType[S, N1, N2]): Unit = {

      CreateUnitRule(action, _ => body, defaultControl, ruleType, conditions *)
    }

    inline def constrained[S <: Relatable, N1 <: Relatable, N2 <: Relatable, SC <: S, N1C <: N1, N2C <: N2, T, R](
                                                                                       action: FirstArg[S, N1, N2, T, R],
                                                                                       inline constraintCondition: ConstraintCondition[SC, N1C, N2C],
                                                                                       inline conditions: RuleQuestion*)(inline body: UnitBodyType[SC, N1C, N2C]): Unit = {
      ???
      //CreateUnitRule(action, _ => body, defaultControl, ruleType, conditions *)
    }




    inline def returns[S <: Relatable, N1 <: Relatable, N2 <: Relatable, T, R](action: FirstArg[S,N1,N2,T,R], inline conditions: RuleQuestion*)(inline body: ReturnsBodyType[S, N1, N2, T, R]): Unit = {
      CreateReturnsRule(action, body, defaultControl, ruleType, conditions *)
    }
  }

  object before extends Infliction(RuleControl.Continue, RuleType.before)
  object check extends Infliction(RuleControl.Continue, RuleType.check)
  object instead extends Infliction(RuleControl.Stop, RuleType.instead) {
    infix inline def quick[S <: Relatable, N1 <: Relatable, N2 <: Relatable, T, R](action: FirstArg[S, N1, N2, T, R], inline conditions: RuleQuestion*): InsteadConsequence[S,N1,N2,T,R] = {
      new InsteadConsequence(action, conditions*)
    }
  }
  object report extends Infliction(RuleControl.Replace, RuleType.report){
    infix inline def quick[S <: Relatable, N1 <: Relatable, N2 <: Relatable, T, R](action: FirstArg[S, N1, N2, T, R], inline conditions: RuleQuestion*): ReportConsequence[S,N1,N2,T,R] = {
      new ReportConsequence(action, conditions*)
    }
  }
  object inflict extends Infliction(RuleControl.Continue, RuleType.inflict)
  object after extends Infliction(RuleControl.Continue, RuleType.after)
  object applying extends Infliction(RuleControl.Continue, RuleType.applying)

  //inline def instead[S <: Relatable, N1 <: Relatable, N2 <: Relatable, T, R](action: FirstArg[S,N1,N2,T,R], conditions: RuleQuestion[S,N1,N2]*): InsteadConsequence = ??? // new InsteadConsequence(action, conditions *)

  //inline def report[S <: Relatable, N1 <: Relatable, N2 <: Relatable, T, R](action: FirstArg[S,N1,N2,T,R], conditions: RuleQuestion[S,N1,N2]*): ReportConsequence = ??? // new ReportConsequence(action, conditions *)

  // ultra terse syntax
  class InsteadConsequence[S <: Relatable, N1 <: Relatable, N2 <: Relatable, T, R](action: FirstArg[S, N1, N2, T, R], conditions: RuleQuestion*) {
    infix inline def Say(inline s: RuleContext[?,?,?] ?=> StringExpression) : Unit  = {
      CreateUnitRule(action, _ => Interpreter.Say(s), RuleControl.Stop, RuleType.instead, conditions *)
    }

    infix inline def Stop : Unit = {
      CreateUnitRule(action, _ => fail, RuleControl.Stop, RuleType.instead, conditions *)
    }
  }

  class ReportConsequence[S <: Relatable, N1 <: Relatable, N2 <: Relatable, T, R](action: FirstArg[S, N1, N2, T, R], conditions: RuleQuestion*) {
    infix inline def Say(inline s: RuleContext[?,?,?] ?=> StringExpression) : Unit  = {
      CreateUnitRule(action, _=> Interpreter.Say(s), RuleControl.Replace, RuleType.report, conditions *)
    }

    infix inline def Add(s: StringExpression) : Unit = {
      CreateUnitRule(action, _=> Interpreter.Say(s), RuleControl.Continue, RuleType.report, conditions *)
    }

  }

}













