package Zext

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Graph {

  private def unwind(traversalOrder: ArrayBuffer[(Relatable, Int)], lastIndex : Int) : Seq[Relatable] = {
    val route = ArrayBuffer[Relatable]()
    val (destination, pi) = traversalOrder(lastIndex)
    route.addOne(destination)

    var (parentNode, parentIndex) = traversalOrder(pi)

    while(parentIndex != -1){
      route.addOne(parentNode)

      parentNode = traversalOrder(parentIndex)._1
      parentIndex = traversalOrder(parentIndex)._2
    }

    route.addOne(parentNode)
    route.reverse.toSeq
  }

  def findPath(start: Relatable, goal : Relatable, relation : Relation[?,?] ) : Seq[Relatable] = {

    val visited = mutable.HashSet[Relatable]()
    val traversalOrder = ArrayBuffer[(Relatable, Int)]()

    visited.add(start)
    traversalOrder.addOne((start, -1))

    val children = start.getRelated(relation).filter(!visited.contains(_))
    traversalOrder.addAll(children.map((_,0)))
    visited.addAll(children)

    var childIndex = 0

    while(childIndex < traversalOrder.length){
      val next = traversalOrder(childIndex)._1

      if(next == goal){
        return unwind(traversalOrder, childIndex)
      }

      val children = next.getRelated(relation).filter(!visited.contains(_))
      traversalOrder.addAll(children.map((_, childIndex)))
      visited.addAll(children)

      childIndex += 1
    }

    Nil
  }

}
