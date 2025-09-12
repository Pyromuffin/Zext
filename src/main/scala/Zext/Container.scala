package Zext

import Zext.*
import Zext.exports.*
import Zext.QueryPrecedence.Content

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import zobjectifier.Macros

object Container {
  implicit def z(container: Container) : ZextObject = container.asInstanceOf[ZextObject]

}

type ZContainer = ZextObject & Container

trait Container {
  this: ZextObject =>

  given c: ZContainer = this

  def contents: Seq[Thing] = relations(Containment).toSeq.sortBy(_.name.toString)
  var preposition = "inside"
  var openable = true
  var open = true
  var transparent = true
  var automaticallyListContents = true
  var enterable = false


  def ContentsString: Option[String] = {
    ListNamesNicely(contents)
  }
}


object Supporter {
  report(putting, anything -> of[Supporter]) Say s"You put $noun on to $secondNoun"
  report(taking, of[Thing], isZextObjectOf[Supporter](noun[Thing].location, Content)) Say s"You take $noun off of ${noun[Thing].location}"
}


// tables, hooks, etc always open and cannot be closed.
case class Supporter(override val description: StringExpression = "")(using c : Container & ZextObject) extends Thing with Container {
  open = true
  transparent = true
  openable = false
  preposition = "on"
}

case class Box(override val description: StringExpression = "")(using c : Container & ZextObject) extends Thing with Container {
  transparent = false
  open = false
}






