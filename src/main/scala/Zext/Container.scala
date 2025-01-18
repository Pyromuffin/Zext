package Zext

import Zext.*
import Zext.exports.*
import Zext.QueryPrecedence.Containment

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

import zobjectifier.Macros

object Container {
  implicit def z(container: Container) : ZextObject = container.asInstanceOf[ZextObject]

}

trait Container {
  this: ZextObject =>

  given c: Container = this

  var contents: ArrayBuffer[ZextObject] = ArrayBuffer[ZextObject]()
  var preposition = "inside"
  var openable = true
  var open = true
  var transparent = true
  var automaticallyListContents = true

  def ContentsString: Option[String] = {
    ListNamesNicely(contents.toSeq)
  }

  infix def contains(zextObject: ZextObject) = contents.contains(zextObject)

  // i think these containment conditions are not ?? dynamic. The haver is fixed, so if that changes then this wont work properly.
  infix def has(zextObject: => ZextObject) = Condition(zextObject.parentContainer == this, QueryPrecedence.Containment)
  infix def lacks(zextObject: => ZextObject) = Condition(zextObject.parentContainer != this, QueryPrecedence.Containment)
}


object Supporter {
  report(putting, anything -> ofDebug[Supporter]("report putting anything -> supporter")) Say s"You put $noun on to $secondNoun"
  report(taking, isZextObjectOf[Supporter](noun.parentContainer, Containment)) Say s"You take $noun off of ${noun.parentContainer}"
}


// tables, hooks, etc always open and cannot be closed.
abstract class Supporter(val name: StringExpression)(using Container) extends Thing with Container {
  open = true
  transparent = true
  openable = false
  preposition = "on"
}

abstract class Box(val name: StringExpression, open_and_transparent : Boolean = false) (using Container) extends Thing with Container {
  transparent = open_and_transparent
  open = open_and_transparent
}

case class SimpleBox(override val name : StringExpression, description: StringExpression)(using c : Container) extends Box(name)

case class SimpleSupporter(override val name: StringExpression, description: StringExpression)(using c: Container) extends Supporter(name)

inline def simpleBox(desc: StringExpression)(code: Container ?=> Unit)(using boxContainer: Container) = {
  val name = FixName(Macros.superVariableName)
  val box = SimpleBox(name, desc)(using boxContainer)
  code(using box)
  box
}

inline def simpleBox(desc: StringExpression)(using boxContainer: Container) = {
  val name = FixName(Macros.superVariableName)
  val box = SimpleBox(name, desc)(using boxContainer)
  box
}


inline def simpleSupporter(desc: StringExpression)(code: Container ?=> Unit)(using supporterContainer: Container) = {
  val name = FixName(Macros.superVariableName)
  val supporter = SimpleSupporter(name, desc)(using supporterContainer)
  code(using supporter)
  supporter
}

inline def simpleSupporter(desc: StringExpression)(using supporterContainer: Container) = {
  val name = FixName(Macros.superVariableName)
  val supporter = SimpleSupporter(name, desc)(using supporterContainer)
  supporter
}



