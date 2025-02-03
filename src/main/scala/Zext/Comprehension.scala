package Zext

import Zext.SetComprehension.CombinedComprehension

import scala.reflect.TypeTest


trait SetComprehension[+T] {
  def getSet(): Seq[T]
  var inverted = false
  def unary_! = {
    inverted = !inverted
    this
  }

}

object SetComprehension {

  case class FilterComprehension[T <: Relatable : TT](filter: T => Boolean) extends SetComprehension[T]{
    def getSet(): Seq[T] = {
      ZextObject.GetAll[T].filter(filter)
    }
  }

  case class SingleComprehension[R <: Relatable](r: R) extends SetComprehension[R]{
    def getSet(): Seq[R] = Seq(r)
  }

  case class ArrayComprehension[R <: Relatable](r: R*) extends SetComprehension[R] {
    def getSet(): Seq[R] = r
  }

  case class CombinedComprehension[R <: Relatable](setComprehensions: SetComprehension[R]*) extends SetComprehension[R] {
    def getSet(): Seq[R] = setComprehensions.flatMap(_.getSet())
  }


}



