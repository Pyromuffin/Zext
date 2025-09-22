package Zext

import Zext.Rule.*
import zobjectifier.Macros.*

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




  inline def CreateRule[T,R](action: Action & Returns[T, R] | Condition, inline body: T => R, control: RuleControl, ruleType: RuleType, conditions: Condition*) : ActionRule[T] = {

    var conds = conditions

    val ruleSet = action match {
      case returnsWrapper: ReturnsWrapper.type  =>
        GetRuleSet(returnsWrapper.wrapped, ruleType)
      case action: Action =>
        GetRuleSet(action, ruleType)
      case ac : ActionWithContextCondition =>
        conds = conds.appended(ac)
        GetRuleSet(ac.action, ruleType)
      case condition: Condition =>
        conds = conds.appended(condition)
        GetRuleSet(null, ruleType, true)
    }

    val rule = new ActionRule[T](body, conds.toArray, control, true)
    rule.definitionPosition = CodePosition()
    rule.sourceCode = SourceCode(body)
    ruleSet += rule

    rule
  }


  inline def CreateUnitRule(action:  Action & Returns[?, ?] | Condition, inline body : => Unit, control : RuleControl, ruleType : RuleType, conditions : Condition *) : ActionRule[Unit] = {

    var conds = conditions

    val ruleSet = action match {
      case returnsWrapper: ReturnsWrapper.type  =>
        GetRuleSet(returnsWrapper.wrapped, ruleType)
      case action: Action =>
        GetRuleSet(action, ruleType)
      case ac : ActionWithContextCondition =>
        conds = conds.appended(ac)
        GetRuleSet(ac.action, ruleType)
      case condition: Condition =>
        conds = conds.appended(condition)
        GetRuleSet(null, ruleType, true)
    }

    val rule = new ActionRule[Unit](_ => body, conds.toArray, control, false)
    rule.definitionPosition = CodePosition()
    rule.sourceCode = SourceCode( _ => body)
    ruleSet += rule

    rule
  }


  inline def before[T, R](action: Action & Returns[T, R] | Condition, conditions: Condition*)(inline body: => R)(using DummyImplicit): ActionRule[Unit] = {
    CreateUnitRule(action, body, RuleControl.Continue, RuleType.before, conditions *)
  }

  inline def before[T, R](action: Action & Returns[T, R] | Condition, conditions: Condition*)(inline body: T => R): ActionRule[T] = {
    CreateRule(action, body, RuleControl.Continue, RuleType.before, conditions *)
  }


  inline def check[T, R](action: Action & Returns[T, R] | Condition, conditions: Condition*)(inline body: => R)(using DummyImplicit): ActionRule[Unit] = {
    CreateUnitRule(action, body, RuleControl.Continue, RuleType.check, conditions *)
  }

  inline def check[T, R](action: Action & Returns[T, R] | Condition, conditions: Condition*)(inline body: T => R): ActionRule[T] = {
    CreateRule(action, body, RuleControl.Continue, RuleType.check, conditions *)
  }


  inline def inflict[T, R](action: Action & Returns[T, R] | Condition, conditions: Condition*)(inline body: => R)(using DummyImplicit): ActionRule[Unit] = {
    CreateUnitRule(action, body, RuleControl.Continue, RuleType.inflict, conditions *)
  }

  inline def inflict[T, R](action: Action & Returns[T, R] | Condition, conditions: Condition*)(inline body: T => R): ActionRule[T] = {
    CreateRule(action, body, RuleControl.Continue, RuleType.inflict, conditions *)
  }


  inline def instead[T, R](action: Action & Returns[T, R] | Condition, conditions: Condition*)(inline body: => R)(using DummyImplicit): ActionRule[Unit] = {
    CreateUnitRule(action, body, RuleControl.Stop, RuleType.instead, conditions *)
  }

  inline def instead[T, R](action: Action & Returns[T, R] | Condition, conditions: Condition*)(inline body: T => R): ActionRule[T] = {
    CreateRule(action, body, RuleControl.Stop, RuleType.instead, conditions *)
  }


  inline def report[T, R](action: Action & Returns[T, R] | Condition, conditions: Condition*)(inline body: => R)(using DummyImplicit): ActionRule[Unit] = {
    CreateUnitRule(action, body, RuleControl.Replace, RuleType.report, conditions *)
  }

  inline def report[T, R](action: Action & Returns[T, R] | Condition, conditions: Condition*)(inline body: T => R): ActionRule[T] = {
    CreateRule(action, body, RuleControl.Replace, RuleType.report, conditions *)
  }


  inline def after[T, R](action: Action & Returns[T, R] | Condition, conditions: Condition*)(inline body: => R)(using DummyImplicit): ActionRule[Unit] = {
    CreateUnitRule(action, body, RuleControl.Continue, RuleType.after, conditions *)
  }

  inline def after[T, R](action: Action & Returns[T, R] | Condition, conditions: Condition*)(inline body: T => R): ActionRule[T] = {
    CreateRule(action, body, RuleControl.Continue, RuleType.after, conditions *)
  }


  inline def applying[T, R](action: Action & Returns[T, R] | Condition, conditions: Condition*)(inline body: => R)(using DummyImplicit): ActionRule[Unit] = {
    CreateUnitRule(action, body, RuleControl.Continue, RuleType.applying, conditions *)
  }

  inline def applying[T, R](action: Action & Returns[T, R] | Condition, conditions: Condition*)(inline body: T => R): ActionRule[T] = {
    CreateRule(action, body, RuleControl.Continue, RuleType.applying, conditions *)
  }



  // ultra terse syntax
  class InsteadConsequence(action:  Action & Returns[?, ?] | Condition, conditions: Condition*) {

    infix inline def Say(inline s: StringExpression): ActionRule[Unit] = {
      CreateUnitRule(action, Interpreter.Say(s), RuleControl.Stop, RuleType.instead, conditions*)
    }

    infix inline def Stop: ActionRule[Unit] = {
      CreateUnitRule(action, fail, RuleControl.Stop, RuleType.instead, conditions *)
    }
  }

  class ReportConsequence(action:  Action & Returns[?, ?] | Condition, conditions: Condition*) {
    infix inline def Say(s: StringExpression): ActionRule[Unit] = {
      CreateUnitRule(action, Interpreter.Say(s), RuleControl.Replace, RuleType.report, conditions*)
    }

    infix inline def Add(s: StringExpression): ActionRule[Unit] = {
      CreateUnitRule(action, Interpreter.Say(s), RuleControl.Continue, RuleType.report, conditions *)
    }

  }

  // this is pretty stupid, but whatever.
  object ReturnsWrapper extends Action(0, "???") with SystemAction with Returns[Unit,Unit] {
    var wrapped : Action = null
  }

  inline def instead(action: Action & Returns[?, ?] | Condition, conditions: Condition*) : InsteadConsequence = new InsteadConsequence(action, conditions* )
  inline def report(action: Action & Returns[?, ?] | Condition, conditions: Condition*): ReportConsequence = new ReportConsequence(action, conditions* )



}
