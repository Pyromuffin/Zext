package Zext

import Zext.*
import Zext.exports.*
import Zext.QueryPrecedence.Content
import Zext.Relatable.Containment

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import zobjectifier.Macros

object Container {
  implicit def z(container: Container) : ZextObject = container.asInstanceOf[ZextObject]

}

trait Container {
  this: ZextObject =>

  given c: Container = this

  def contents: Seq[Thing] = relations(Containment).toSeq
  var preposition = "inside"
  var openable = true
  var open = true
  var transparent = true
  var automaticallyListContents = true
  var enterable = false


  def ContentsString: Option[String] = {
    ListNamesNicely(contents)
  }

  // i think these containment conditions are not ?? dynamic. The haver is fixed, so if that changes then this wont work properly.
  //infix def has(zextObject: => ZextObject) = Condition(zextObject.parentContainer == this, QueryPrecedence.Content)
  //infix def lacks(zextObject: => ZextObject) = Condition(zextObject.parentContainer != this, QueryPrecedence.Content)
}


object Supporter {
  report(putting, anything -> ofDebug[Supporter]("report putting anything -> supporter")) Say s"You put $noun on to $secondNoun"
  report(taking, isZextObjectOf[Supporter](noun.parentContainer, Content)) Say s"You take $noun off of ${noun.parentContainer}"
}


// tables, hooks, etc always open and cannot be closed.
case class Supporter(override val description: StringExpression = "")(using Container) extends Thing with Container {
  open = true
  transparent = true
  openable = false
  preposition = "on"
}

case class Box(override val description: StringExpression = "")(using Container) extends Thing with Container {
  transparent = false
  open = false
}






