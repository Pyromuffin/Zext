package Zext

import Zext.*
import Zext.exports.*
import Zext.QueryPrecedence.Containment

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

object Container {
  implicit def z(container: Container) : ZextObject = container.asInstanceOf[ZextObject]

}

trait Container {
  this: ZextObject =>

  given c: Container = this

  var contents: ArrayBuffer[ZextObject] = ArrayBuffer[ZextObject]()
  var open = true
  var transparent = true
  var automaticallyListContents = true

  def ContentsString: Option[String] = {
    ListNamesNicely(contents.toSeq)
  }

  infix def contains(zextObject: ZextObject) = contents.contains(zextObject)

  // i think these containment conditions are not ?? dynamic. The haver is fixed, so if that changes then this wont work properly.
  infix def has(zextObject: => ZextObject) = Condition(zextObject.parentContainer == this, QueryPrecedence.Containment)
  infix def had(zextObject: => ZextObject) = Condition(zextObject.parentContainer == this, QueryPrecedence.Containment, true)
  infix def lacks(zextObject: => ZextObject) = Condition(zextObject.parentContainer != this, QueryPrecedence.Containment)
  infix def lacked(zextObject: => ZextObject) = Condition(zextObject.parentContainer != this, QueryPrecedence.Containment, true)

}


object Supporter {
  instead(closing, of[Supporter]) Say s"There's no way to close $noun"
  report(putting, ofSecond[Supporter]) Say s"You put $noun on to $secondNoun"
  report(taking, was[Supporter](noun.parentContainer, Containment)) Say s"You take $noun off of ${noun.parentContainer}"
}


// tables, hooks, etc always open and cannot be closed.
abstract class Supporter(using Container) extends Thing with Container {
  automaticallyListContents = false
  open = true
  transparent = true
}

abstract class Box(open_and_transparent : Boolean = false) (using Container) extends Thing with Container {
  transparent = open_and_transparent
  open = open_and_transparent
}

case class SimpleBox(name : String, description: StringExpression)(using c : Container) extends Box
case class SimpleSupporter(name: String, description: StringExpression)(using c: Container) extends Supporter

inline def box(desc: StringExpression)(code: Container ?=> Unit)(using boxContainer: Container) = {
  val name = Macros.superVariableName
  val box = SimpleBox(name, desc)(using boxContainer)
  code(using box)
  box
}

inline def box(desc: StringExpression)(using boxContainer: Container) = {
  val name = Macros.superVariableName
  val box = SimpleBox(name, desc)(using boxContainer)
  box
}


inline def supporter(desc: StringExpression)(code: Container ?=> Unit)(using supporterContainer: Container) = {
  val name = Macros.superVariableName
  val supporter = SimpleSupporter(name, desc)(using supporterContainer)
  code(using supporter)
  supporter
}

inline def supporter(desc: StringExpression)(using supporterContainer: Container) = {
  val name = Macros.superVariableName
  val supporter = SimpleSupporter(name, desc)(using supporterContainer)
  supporter
}


// not sure i like these string context extensions
/*
extension (sc: StringContext) {
  inline def box(args: Any*)(using c: Container): Box = {
    SimpleBox(FixName(Macros.variableName), sc.s())
  }

  inline def supporter(args: Any*)(using c: Container): Supporter = {
    SimpleSupporter(FixName(Macros.variableName), sc.toString)
  }
}
*/

