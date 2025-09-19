package Zext

import Zext.Infliction.RuleControl.{Continue, Replace, Stop}
import Zext.Interpreter.Say
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

  def GetConditions(first: Condition, _conditions: Condition*): (ActionRuleSet, Array[Condition]) = {
    // do something to extract the action probably
    var conditions = _conditions
    var ruleSet = alwaysRuleSet
    first match {
      case ac: ConditionWithAction =>
        if (ac.queryType == QueryPrecedence.Action) {
          ruleSet = ruleSets(ac.action)
        } else {
          ruleSet = ruleSets(ac.action)
          conditions = Array(first) concat _conditions
        }
      case _ =>
        conditions = Array(first) concat _conditions
    }

    (ruleSet, conditions.toArray)
  }


  inline def before(first: Condition, conditions: Condition*)(inline body: => Unit): ActionRule[Unit] = {
    val (ruleSet, c) = GetConditions(first, conditions *)
    val rule = new ActionRule[Unit](_ => body, c, RuleControl.Continue, false)
    rule.definitionPosition = CodePosition()
    ruleSet.beforeRules += rule

    rule
  }


  inline def before[T, R](action: Action & Returns[T, R], conditions: Condition*)(inline body: T => R): ActionRule[T] = {
    val rule = new ActionRule(body, conditions.toArray, RuleControl.Continue, true)
    rule.definitionPosition = CodePosition()
    ruleSets(action).beforeRules += rule

    rule
  }


  inline def check(first: Condition, conditions: Condition*)(inline body: => Unit): ActionRule[Unit] = {
    val (ruleSet, c) = GetConditions(first, conditions *)
    val rule = new ActionRule[Unit](_ => body, c, RuleControl.Continue, false)
    rule.definitionPosition = CodePosition()
    ruleSet.checkRules += rule

    rule
  }


  inline def check[T, R](action: Action & Returns[T, R], conditions: Condition*)(inline body: T =>  R): ActionRule[T] = {
    val rule = new ActionRule(body, conditions.toArray, RuleControl.Continue, true)
    rule.definitionPosition = CodePosition()
    ruleSets(action).checkRules += rule

    rule
  }


  inline def instead(first: Condition, conditions: Condition*)(inline body: => Unit): ActionRule[Unit] = {
    val (ruleSet, c) = GetConditions(first, conditions *)
    val rule = new ActionRule[Unit](_ => body, c, RuleControl.Stop, false)
    rule.definitionPosition = CodePosition()
    ruleSet.insteadRules += rule

    rule
  }


  inline def instead[T, R](action: Action & Returns[T, R], conditions: Condition*)(inline body: T => R): ActionRule[T] = {
    val rule = new ActionRule(body, conditions.toArray, RuleControl.Stop, true)
    rule.definitionPosition = CodePosition()
    ruleSets(action).insteadRules += rule

    rule
  }


  inline def inflict(first: Condition, conditions: Condition*)(inline body: => Unit): ActionRule[Unit] = {
    val (ruleSet, c) = GetConditions(first, conditions *)
    val rule = new ActionRule[Unit](_ => body, c, RuleControl.Continue, false)
    rule.definitionPosition = CodePosition()
    ruleSet.executeRules += rule

    rule
  }

  type ReturnActions[T,R] = Action & Returns[T, R]

  inline def inflict[T, R](action: ReturnActions[T,R], conditions: Condition*)(inline body: T => R): ActionRule[T] = {
    val rule = new ActionRule(body, conditions.toArray, RuleControl.Continue, true)
    rule.definitionPosition = CodePosition()
    ruleSets(action).executeRules += rule

    rule
  }


  inline def report(first: Condition, conditions: Condition*)(inline body: => Unit): ActionRule[Unit] = {
    val (ruleSet, c) = GetConditions(first, conditions *)
    val rule = new ActionRule[Unit](_ => body, c, RuleControl.Replace, false)
    rule.definitionPosition = CodePosition()
    ruleSet.reportRules += rule

    rule
  }


  inline def report[T, R](action: Action & Returns[T, R], conditions: Condition*)(inline body: T => R): ActionRule[T] = {
    val rule = new ActionRule(body, conditions.toArray, RuleControl.Replace, true)
    rule.definitionPosition = CodePosition()
    ruleSets(action).reportRules += rule

    rule
  }


  inline def after(first: Condition, conditions: Condition*)(inline body: => Unit): ActionRule[Unit] = {
    val (ruleSet, c) = GetConditions(first, conditions *)
    val rule = new ActionRule[Unit](_ => body, c, RuleControl.Continue, false)
    rule.definitionPosition = CodePosition()
    ruleSet.afterRules += rule

    rule
  }


  inline def after[T, R](action: Action & Returns[T, R], conditions: Condition*)(inline body: T => R): ActionRule[T] = {
    val rule = new ActionRule(body, conditions.toArray, RuleControl.Continue, true)
    rule.definitionPosition = CodePosition()
    ruleSets(action).afterRules += rule

    rule
  }


  inline def applying(first: Condition, conditions: Condition*)(inline body: => Unit): ActionRule[Unit] = {
    val (ruleSet, c) = GetConditions(first, conditions *)
    val rule = new ActionRule[Unit](_ => body, c, RuleControl.Continue, false)
    rule.definitionPosition = CodePosition()
    ruleSet.applyingRules += rule

    rule
  }


  inline def applying[T, R](action: Action & Returns[T, R], conditions: Condition*)(inline body: T => R): ActionRule[T] = {
    val rule = new ActionRule(body, conditions.toArray, RuleControl.Continue, true)
    rule.definitionPosition = CodePosition()
    ruleSets(action).applyingRules += rule

    rule
  }


  object stuff extends Action(0)
  object string_process extends Action(0) with Returns[String, String]
  object string_say extends Action(0) with Returns[String, Unit]


  inflict(stuff, unlisted) {
    Say("nope!")
  }

  inflict(string_process, unlisted) { s =>
    if(s.length > 1){
      fail -> s
    }
      s
  }

  inflict(string_say) { s =>
    Say(s.toLowerCase())
  }



  // ultra terse syntax
  class InsteadConsequence(first: Condition, conditions: Condition*) {
    infix inline def Say(s: StringExpression): ActionRule[Unit] = {
      instead(first, conditions *)(Interpreter.Say(s))
    }

    infix inline def Stop: ActionRule[Unit] = {
      instead(first, conditions *)(fail)
    }
  }

  class ReportConsequence(first: Condition, conditions: Condition*) {
    infix inline def Say(s: StringExpression): ActionRule[Unit] = {
      report(first, conditions *)(Interpreter.Say(s))
    }

    infix inline def Add(s: StringExpression): ActionRule[Unit] = {
      report(first, conditions *) {
        Interpreter.Say(s)
        continue
      }
    }
  }

  def report(first: Condition, conditions: Condition*): ReportConsequence = {
    new ReportConsequence(first, conditions *)
  }

  def instead(first: Condition, conditions: Condition*): InsteadConsequence = {
    new InsteadConsequence(first, conditions *)
  }

}
