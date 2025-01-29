package Zext

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashSet}

class UnionFind[T] {

  // i think we just uhh, search linearly through all the sets every time!
  // yes it will be slow, but I don't really care.

  val sets = ArrayBuffer[HashSet[T]]()

  def find(node : T): HashSet[T] = {

    for(set <- sets) {
      if(set.contains(node))
        return set
    }

    val newSet = HashSet(node)
    sets.addOne(newSet)
    newSet
  }

  def makeUnion(a : T, b : T): Unit = {
    val a_set = find(a);
    val b_set = find(b);
    if(a_set == b_set)
      return

    if(a_set.size > b_set.size) {
      a_set.addAll(b_set)
      sets.remove(sets.indexOf(b_set))
    } else {
      b_set.addAll(a_set)
      sets.remove(sets.indexOf(a_set))
    }
  }

  def remove(a : T) = {
    val set = find(a)
    set.remove(a)
    if(set.isEmpty) {
      sets.remove(sets.indexOf(set))
    }
  }
}