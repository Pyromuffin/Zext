package Zext

import Zext.Relatable.TT
import scala.reflect.TypeTest

/*
trait SetCompable[Extended, Inner <: Relatable] {
  extension(e : Extended) {
    def getSet() : Seq[Inner]
  }
}

object SetCompable {
  /*
  implicit def fromComp[X, XI <: Relatable](s: X)(using SetCompable[X, XI]): SetComprehension[XI] = {
    () => s.getSet()
  }
  */

  given [T <: Thing] => SetCompable[T, T]:
    extension (thingy: T) def getSet(): Seq[T] = Seq(thingy)

}
*/

trait SetComprehension[+T] {
  def getSet(): Seq[T]
  var inverted = false
  def unary_! = {
    inverted = !inverted
    this
  }
}

object SetComprehension {

  /*
  given [I <: Relatable] =>SetCompable[SetComprehension[I], I]:
    extension (e: SetComprehension[I]) def getSet(): Seq[I] = e.getSet()
  */

  case class FilterComprehension[T <: Relatable : TT](filter: T => Boolean) {
    def getSet(): Seq[T] = {
      ZextObject.GetAll[T].filter(filter)
    }
  }

  case class SingleComprehension[R <: Relatable](r: R) {
    def getSet(): Seq[R] = Seq(r)
  }

  case class ArrayComprehension[R <: Relatable](r: R*) {
    def getSet(): Seq[R] = r
  }


}



