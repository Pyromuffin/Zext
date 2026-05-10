package Zext

import Zext.Relation.RelationQuery
import Zext.SetComprehension.narrow

import scala.collection.mutable.ArrayBuffer


extension [X <: Relatable](x: X ) {
  /*
  def unary_~ : SetComprehension[X] = {
    val narrowed = narrow(x)
    val cloned = narrowed.clone()
    cloned.inverted = !cloned.inverted
    cloned
  }
*/

  def unary_! : X = {
    //val narrowed = narrow(x)
    //val cloned = narrowed.clone()
    //cloned.not = !cloned.not
    //cloned
    ???

    // i think this being global for some reason doesnt work, but maybe.
    assert(!RelationQuery.negateNext)
    println("negated!")
    RelationQuery.negateNext = true
    x
    
  }

}

trait SetComprehension[+T] extends Cloneable {
  def getSet(): Seq[T]
  var inverted = false
  var any = false
  var not = false
  override def clone : this.type = super.clone().asInstanceOf[this.type]
}

object SetComprehension {


  /*
  extension (queryBlock: => SetComprehension[?]) {
    def ? : AnyCondition = {
      NotAQuery.stack.push(ArrayBuffer())
      queryBlock
      val queries = NotAQuery.stack.pop()
      assert(queries.length == 1)
      val q = queries.head
      fromQuery(q)
    }
  }
  */


  def narrow[X <: Relatable](x: X | SetComprehension[X]): SetComprehension[X] = {
    x match {
      case set: SetComprehension[?] => set.asInstanceOf[SetComprehension[X]]
      case relatable: Relatable => () => Seq(relatable.asInstanceOf[X])
    }
  }


  case class FilterComprehension[T <: Relatable : TT](filter: T => Boolean) extends SetComprehension[T]{
    def getSet(): Seq[T] = {
      Relatable.GetAll[T].filter(filter)
    }
  }

  case class AnyOf[R <: Relatable](r: R*) extends SetComprehension[R] {
    any = true
    def getSet(): Seq[R] = r
  }

  case class AllOf[R <: Relatable](r: R*) extends SetComprehension[R] {
    def getSet(): Seq[R] = r
  }

  case class CombinedComprehension[R <: Relatable](setComprehensions: SetComprehension[R]*) extends SetComprehension[R] {
    def getSet(): Seq[R] = setComprehensions.flatMap(_.getSet())
  }


}



