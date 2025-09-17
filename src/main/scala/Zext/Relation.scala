package Zext

import Zext.*
import Zext.QueryPrecedence.Context
import Zext.Relatable.allRelatables
import Zext.Relation.*
import Zext.SetComprehension.{AllOf, AnyOf, CombinedComprehension}
import Zext.exports.*

import scala.collection.{immutable, mutable}
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.language.postfixOps
import scala.reflect.{TypeTest, Typeable}
/*
examples

containment relation - The coin is in the purse.
support relation - The coin is on the table.
incorporation relation - The coin is part of the sculpture.
carrying relation - The coin is carried by Peter.
wearing relation - The jacket is worn by Peter.
possession relation - if Mr Darcy has a rapier...
adjacency relation - The Study is east of the Hallway.
visibility relation - if Darcy can see Elizabeth...
touchability relation - if Darcy can touch Elizabeth...

people who can see the mouse
if Mr Darcy conceals a fob watch ...


(many to one)
Loving relates various people to one person.
Verenka loving relation Stankevich ok
Liubov loving relation Stankevich ok
Liubov loving relation Belinsky not ok! you can only love one person, but multiple people can love one person

Pet-ownership relates various animals to one person (called the owner). naming the target. dunno if we care about this
It would then make sense to talk about "the owner of Loulou", and we could have phrases like "now Flaubert is the owner of Loulou" or "if the owner of Loulou is a woman..." and so forth

symmetric
Meeting relates people to each other.

symmetric one to one
Marriage relates one person to another (called the spouse).



if the Hallway is adjacent to the Study ...
if somebody is in an adjacent room, ...

best route from the Drawbridge to the Keep through visited rooms
The description of the proximity gadget is "You are now [number of moves from the location to the Sundial] moves from the Sundial.";



Contact relates a thing (called X) to a thing (called Y) when X is part of Y or Y is part of X. The verb to be joined to means the contact relation.
Nearness relates a room (called A) to a room (called B) when the number of moves from B to A is less than 3. The verb to be near means the nearness relation.
Instead of listening when the location is near the Sundial: say "You hear a splashing of water."


Material is a kind of value. The materials are wood and metal. A thing has a material.
Materiality relates a thing (called X) to a material (called Y) when Y is the material of X. The verb to be made of means the materiality relation.

if the cube is made of wood, ...
say "The carpenter looks at [the list of things which are made of wood].";

Divisibility relates a number (called N) to a number (called M) when the remainder after dividing M by N is 0. The verb to divide means the divisibility relation. The verb to be a factor of means the divisibility relation.




Nationality relates people to each other in groups.

Transmutation relates things to each other in groups. The verb to become means the transmutation relation.
equivalence/group relation, they all become each other
A thing can be valuable. Something valuable called a bag of jewels is carried by the player. It becomes the bag of gunpowder and the bag of jelly beans.


Definition: a thing is transmutable if it becomes more than one thing. [* It always becomes itself.]

The can't insert into what's not a container rule does nothing when inserting something into the machine.
Check inserting something which is not transmutable into the machine:
    instead say "You can't transmute that."

To decide which thing is new form of (obj - edible thing): decide on a random valuable thing which becomes obj.
To decide which thing is new form of (obj - dangerous thing): decide on a random edible thing which becomes obj.
To decide which thing is new form of (obj - valuable thing): decide on a random dangerous thing which becomes obj.



>relations
Overlooking relates various rooms to various rooms:
    The Pub >=> the Garden
    The Garden >=> the Shrubbery
    The Shrubbery >=> the Sundial Plot
Friendship relates people to each other:
    Mr Wickham <=> Sophie
    Charlotte <=> Sophie
Marriage relates one person to another:
    Mr Wickham == Sophie



Connection relates one thing to another (called the other party).
The verb to reach means the connection relation.
Before calling something on something when the player reaches someone:

Check hanging up:
    if the noun is not a telephone, say "You can't hang up [the noun]." instead;
    if the player does not reach someone, say "You're not on the line with anyone." instead.

Carry out hanging up:
    now the player does not reach anyone.

Before misdialling when the player reaches someone:
    say "(first ending your conversation with [the other party of the player])[command clarification break]";
    end current conversation.

To end current conversation:
    let the current phone be a random telephone which can be touched by the player;
    silently try hanging up the current phone.

*/


// we want to be able to say things about the world
// if concealment relates bob and gun then, or if a person conceals a gun
// relations can be unidirectional, mutually reciprocal, and unique
// we want a way to query all things that satisfy a relation
// and a way to say something like [the list of rooms overlooked by the location]
// inform also contains a graph solver to find a route between objects via relations
// i guess relations express a directed graph. ->, <-, <->, -

// ok there are graph relations (relates) and conditional relations (relates when). no graph operations for conditional relations
// we need to be able to query all members to which a relation pertains

/*
add ability to verb a relation, likely how we define it.
x conceals y
could be an extension method, or a type class

 we can change the types accepted by the relation at runtime apparently!
if R relates X to Y, ...
now R relates X to Y;
now R does not relate X to Y;

"empty" - nothing relates to anything else
"symmetric" - by definition X relates to Y if and only if Y relates to X, straight lines between
"equivalence" - this is a relation "in groups", or an "equivalence relation"
"one-to-one" - it relates one K to one L, a single unidirectional arrow, at most one going in and one going out
"one-to-various" - similarly, each vertex has at most one coming in
"various-to-one" - similarly, each vertex has at most one coming out
"various-to-various" - similarly, no restriction

querying the set of relations...
if R is a symmetric one-to-one relation of texts, ...

Inform uses a different algorithm for finding routes ("the next step via R from A to B") in each of these cases, and internally it stores relations in different formats in the different cases, because it makes a big difference to the efficiency of Inform to minimise the storage required for a relation and the time taken to explore it.

All the cases are benign except for "various to various" - the most useful - and for its closely related symmetrical version, "relates... to each other".
Inform cannot afford to assume that the relation will be "sparse" (that is: that no vertex will have more than a certain number of arrows, or that the total number of arrows will be small), because it has no idea how the arrows will come and go during play.
It therefore uses 1 bit of storage for each pair of objects. This sounds harmless, but if there are 200 rooms, there are 40,000 pairs of rooms, which means a 5000-byte allocation of storage (plus a handful of bytes as overhead).
Gratuitous various-to-various relations are therefore not a good idea.

*/

class RelationQueryContext {
  val queries = ArrayBuffer[RelationQuery[?,?]]()
  def evaluate : Boolean = {
    if(queries.nonEmpty)
      queries.forall(_.evaluate())
    else false
  }
}

class RelationHolder[RequiredValence <: AllValence] {
  private[Zext] val nodes = mutable.HashMap[Relation[?,?], mutable.HashSet[Relatable]]()
  def apply[T <: Relatable](r : Relation[?,T] & RequiredValence) : Set[T] = nodes.getOrElseUpdate(r,mutable.HashSet()).asInstanceOf[mutable.HashSet[T]].toSet
}

class SingleRelationHolder[RequiredValence <: AllValence] {
  private[Zext] val nodes = mutable.HashMap[Relation[?, ?], Relatable]()
  def apply[T <: Relatable](relation: Relation[?, T] & RequiredValence): Option[T] = nodes.get(relation).map(_.asInstanceOf[T])
}

class ReverseSingleRelationHolder[RequiredValence <: AllValence] {
  private[Zext] val nodes = mutable.HashMap[Relation[?, ?], Relatable]()
  def apply[T <: Relatable, V <: RequiredValence](relation: Relation[T, ?] & RequiredValence): Option[T] = nodes.get(relation).map(_.asInstanceOf[T])
}

class ReverseRelationHolder[RequiredValence <: AllValence] {
  private[Zext] val nodes = mutable.HashMap[Relation[?, ?], mutable.HashSet[Relatable]]()
  def apply[S <: Relatable, V <: RequiredValence](r: Relation[S, ?] & RequiredValence): Set[S] = nodes.getOrElseUpdate(r, mutable.HashSet()).asInstanceOf[mutable.HashSet[S]].toSet
}


object Relatable {

  val allRelatables : ArrayBuffer[Relatable] = ArrayBuffer[Relatable]()

  def GetAll[T <: Relatable : TT as tt]: Seq[T] = {

    allRelatables.filter { z =>
      tt.unapply(z).isDefined
    }.map(_.asInstanceOf[T]).toSeq

  }

  extension (queryBlock: => Relatable) {
    def ? : RelationQuery[?,?] = {
      NotAQuery.stack.push(ArrayBuffer())
      queryBlock
      val queries = NotAQuery.stack.pop()
      assert(queries.length == 1)
      queries.head
    }

  }
}


object determiningRelation extends Action(1) with Context[Relation[?, ?]] with SystemAction {

  inflict(determiningRelation) {
    val relation = GetActionContext()
    val related = subject.getRelatedSetFromDictionaries(relation).asInstanceOf[Set[Relatable]]
    stop_unless(related.contains(arg1))
  }


  inflict(determiningRelation(property_having)) {
    val result = ExecuteContextAction(determiningProperty(arg1.resolve.asInstanceOf[Property]) )
    if(result.res) succeed
  }

}



object addingRelated extends Action(-1) with Context[Relation[?,?]] with SystemAction {

  inflict(addingRelated){
    val r = GetActionContext()
    val relatables = GetTargets()
    subject.addRelatedInner(r, relatables)
  }

}

object removingRelated extends Action(-1) with Context[Relation[?, ?]] with SystemAction {

  inflict(removingRelated) {
    val r = GetActionContext()
    val relatables = GetTargets()
    subject.innerRemove(r, relatables)
  }

}





trait Relatable {

  allRelatables.addOne(this)
  val objectID = allRelatables.length

  override def toString = this.getClass.toString


  def getProperty[T : {TT as tt, CT}]: Option[T] = {
    val storedProperties = getRelatedSetFromDictionaries(property_having)
    var maybe = storedProperties.find(tt.test).map(_.asInstanceOf[T])
    if(maybe.isEmpty) {
      // get generated property
      val generated = ExecuteContextAction(determiningProperty[T], InheritContext(action = determiningProperty, subject = this))
      if(generated.res) maybe = Some(generated.ret.asInstanceOf[T])
    }
    maybe
  }

/*
  def apply[T: {TT, CT}] : T = {
    val maybe = this.getProperty[T]
    if (maybe.isDefined) maybe.get
    else this.asInstanceOf[T]
  }
  */


  def apply[T: {TT, CT}] = {
    this match {
      case t: T => t
      case _ =>
        val maybe = this.getProperty[T]
        if (maybe.isDefined) maybe.get
        else throw new ClassCastException() // if we throw in the middle of a query then we never pop the query stack.
    }
  }


  override def equals(obj: Any) = {
    obj match {
      case relatableProxy: RelatableProxy[?] => objectID == relatableProxy.resolve.objectID
      case relatable: Relatable => objectID == relatable.objectID
      case null => false
    }
  }

  extension(h : mutable.HashMap[Relation[?,?], mutable.HashSet[Relatable]]) {
    def defaulted(r : Relation[?,?]) = h.getOrElseUpdate(r, mutable.HashSet())
  }

  // removes relation and all related from relatable
  def removeRelation(r: Relation[?, ?]): Unit = {
    val buddies = getRelatedSetFromDictionaries(r).iterator.toSeq
    removeRelated(r, buddies*)

    r match {
      case r: OneToMany => children.nodes.remove(r)
      case r: ManyToMany => children.nodes.remove(r)
      case r: OneToOne => child.nodes.remove(r)
      case r: ManyToOne => child.nodes.remove(r)
      case r: SingleSymmetric => partner.nodes.remove(r)
      case r: ManySymmetric => partners.nodes.remove(r)
      case r: Equivalence => groups.nodes.remove(r)
    }
  }

  private[Zext] def innerRemove(r: Relation[?, ?], relatables: Seq[Relatable]): Unit = {
    r match {
      case oneToMany: OneToMany =>
        for (child <- relatables) {
          children.nodes.defaulted(oneToMany).remove(child)
          child.parent.nodes.remove(oneToMany)
        }

      case manyToMany: ManyToMany =>
        for (child <- relatables) {
          children.nodes.defaulted(manyToMany).remove(child)
          child.parents.nodes.defaulted(manyToMany).remove(this)
        }

      case oneToOne: OneToOne =>
        for (c <- relatables) {
          child.nodes.remove(oneToOne)
          c.parent.nodes.remove(oneToOne)
        }

      case manyToOne: ManyToOne =>
        for (c <- relatables) {
          child.nodes.remove(manyToOne)
          c.parents.nodes(manyToOne).remove(this)
        }

      case symmetric: SingleSymmetric =>
        for (c <- relatables) {
          partner.nodes.remove(symmetric)
          c.partner.nodes.remove(symmetric)
        }

      case manySymmetric: ManySymmetric =>
        for (c <- relatables) {
          partners.nodes.defaulted(manySymmetric).remove(c)
          c.partners.nodes.defaulted(manySymmetric).remove(this)
        }

      case equivalent: Equivalence =>
        // well we're just saying that these nodes are not equivalent in this relation anymore.
        // this doesn't make too much sense, so i guess just remove this item from the shared set, and then remove the relation
        groups.nodes.defaulted(equivalent).remove(this)
        groups.nodes.remove(equivalent)
    }
  }

  def removeRelated(r: Relation[?, ?], relatables: Relatable*): Unit = {

    r match {
      // please dont call this with a conditional relation
      //case conditional: ConditionalRelation[?,?] => ???
      case reciprocal: ReciprocalRelation[?,?] =>
        for(relatable <- relatables){
          val subject = relatable.asInstanceOf[ZextObject]
          ExecuteContextAction(removingRelated(r), RuleContext(removingRelated, subject, Array(this), false, subject.location))
        }
      case _ =>
    }

    val location = this match {
      case zextObject: ZextObject => zextObject.location
      case _ => nowhere
    }

    ExecuteContextAction(removingRelated(r), RuleContext(removingRelated, this, relatables, false, location))
  }

  private[Zext] def addRelatedInner(r: Relation[?, ?], relatables: Seq[Relatable]) : Unit = {

    r match {
      // containment, we have to unparent the children and make it this
      // we also need to unchild the previous parent
      case oneToMany: OneToMany =>
        children.nodes.defaulted(oneToMany).addAll(relatables)
        for (child <- relatables) {
          val childsPreviousParent = child.parent.nodes.get(oneToMany)
          childsPreviousParent.does(_.children.nodes(oneToMany).remove(child))
          child.parent.nodes(oneToMany) = this
        }

      // anything goes.
      case manyToMany: ManyToMany =>
        children.nodes.defaulted(manyToMany).addAll(relatables)
        for (child <- relatables) {
          child.parents.nodes.defaulted(manyToMany).add(this)
        }

      case manySymmetric: ManySymmetric =>
        partners.nodes.defaulted(manySymmetric).addAll(relatables)
        for (child <- relatables) {
          child.partners.nodes.defaulted(manySymmetric).add(this)
        }


      //  maybe like wearing a hat? Can't wear more than one hat.
      case oneToOne: OneToOne =>
        // we have to do 6 things.
        require(relatables.length == 1)
        val nextChild = relatables.head

        // 1) go to the previous child and unparent this from it
        for (previousChild <- child.nodes.get(oneToOne)) {
          previousChild.parent.nodes.remove(oneToOne)
        }
        // 2) remove previous child, and 6) set next child
        child.nodes(oneToOne) = nextChild

        // 3) remove next child's previous parent's children
        for (childsPreviousParent <- nextChild.parent.nodes.get(oneToOne)) {
          childsPreviousParent.child.nodes.remove(oneToOne)
        }

        // 4) remove next child's previous parent and 5) set next child's parent
        nextChild.parent.nodes(oneToOne) = this



      // composition, many things can compose only one object
      // on change we nead to clear out current children to make sure we don't compose more than one object
      case manyToOne: ManyToOne =>
        require(relatables.length == 1)
        val nextChild = relatables.head
        // go to the previous child and unparent this from it
        for (previousChild <- child.nodes.get(manyToOne)) {
          previousChild.parents.nodes(manyToOne).remove(this)
        }

        // add next child
        child.nodes(manyToOne) = nextChild
        // add this to next child's parents
        nextChild.parents.nodes.defaulted(manyToOne).add(this)


      // almost the same as one to one
      case symmetric: SingleSymmetric =>
        require(relatables.length == 1)
        val newPartner = relatables.head
        // 1) remove old partner's partner and 2) remove old partner,
        // 3) add new partner and 4) new partner's partner
        // 5) remove new partner's old partner's partner
        // 6) remove new partner's old partner
        for (oldPartner <- partner.nodes.get(symmetric)) {
          oldPartner.partner.nodes.remove(symmetric) // 1
        }
        partner.nodes(symmetric) = newPartner // 2 and 3

        for (newPartnersOldPartner <- newPartner.partner.nodes.get(symmetric)) {
          newPartnersOldPartner.partner.nodes.remove(symmetric) // 5
        }
        newPartner.partner.nodes(symmetric) = this // 6 and 4


      // this is somewhat unlike the others. for each node, find the 'transitive closure'
      // which is all the nodes in the subgraph
      case equivalent: Equivalence =>
        val nodes = Seq(this) concat relatables
        val set = nodes.map(_.groups.nodes.get(equivalent))
          .filter(_.isDefined).map(_.get).distinct
          .sortBy(_.size)
          .foldLeft(mutable.HashSet[Relatable]())(_ addAll _)

        for (node <- nodes) {
          node.groups.nodes(equivalent) = set
        }
    }
  }

  def addRelated(r: Relation[?, ?], _relatables: Relatable*) = {

    // filter out already related
    val currentlyRelated = getRelatedSetFromDictionaries(r).asInstanceOf[Set[Relatable]]
    val relatables = _relatables.filterNot(a => currentlyRelated.contains(a))

    if (relatables.nonEmpty) {

      val location = this match {
        case zextObject: ZextObject => zextObject.location
        case _ => nowhere
      }

      ExecuteContextAction(addingRelated(r), RuleContext(addingRelated, this, relatables, false, location ))
    }

    this
  }

  private[Zext] def getRelatedSetFromDictionaries[T <: Relatable](relation: Relation[?,T]) : Set[T] = {
    relation match {
      case r : OneToMany => children(r)
      case r : ManyToMany => children(r)
      case r : Equivalence => groups(r)
      case r : OneToOne => child(r).toSet
      case r : ManyToOne => child(r).toSet
      case r : SingleSymmetric => partner(r).toSet
      case r : ManySymmetric => partners(r)
    }
  }

  // potentially costly, may iterate through all relatables.
  def queryRelatedSet[B <: Relatable](relation: Relation[?,B]) : Set[B] = {
    val candidates = Relatable.GetAll[B](using relation.ttTarget)
    candidates.filter(c => ExecuteContextAction(determiningRelation(relation), subject = this, c).res).toSet
  }

  def queryRelated[V <: OneToOne | ManyToOne | SingleSymmetric, T <: Relatable](relation: Relation[?, T] & V): Option[T] = {
    val set = queryRelatedSet(relation)
    assert(set.isEmpty || set.size == 1)
    set.headOption
  }

  def queryRelated[V <: OneToMany | ManyToMany | Equivalence | ManySymmetric, T <: Relatable](relation : Relation[?,T] & V)(using DummyImplicit) : Set[T] = {
    queryRelatedSet(relation)
  }

  def listRelations(): Seq[Relation[?,?]] = {
    val out = child.nodes.keys concat
      children.nodes.keys concat
      partners.nodes.keys concat
      partner.nodes.keys concat
      groups.nodes.keys

    out.toSeq
  }

  val child = SingleRelationHolder[OneToOne | ManyToOne]()
  val children = RelationHolder[OneToMany | ManyToMany]()

  // reversed types
  val parents = ReverseRelationHolder[ManyToOne | ManyToMany]()
  val parent = ReverseSingleRelationHolder[OneToOne | OneToMany]()

  val partners = RelationHolder[ManySymmetric]()
  val partner = SingleRelationHolder[SingleSymmetric]()
  val groups = RelationHolder[Equivalence]()


}

object Relation {

  val relations = ArrayBuffer[Relation[?,?]]()

  trait AllValence {
    this: Relation[?,?] =>
  }
  trait OneToOne extends AllValence{
    this: Relation[?,?] =>
  }
  trait ManyToOne extends AllValence{
    this: Relation[?,?] =>
  }
  trait OneToMany extends AllValence{
    this: Relation[?,?] =>
  }
  trait ManyToMany extends AllValence{
    this: Relation[?,?] =>
  }
  trait Equivalence extends AllValence{
    this: Relation[?,?] =>
  }
  trait SingleSymmetric extends AllValence{
    this: Relation[?,?] =>
  }
  trait ManySymmetric extends AllValence{
    this: Relation[?,?] =>
  }

  object RelationQuery {
    implicit def toBoolean(rq : RelationQuery[?,?]) : Boolean = rq.evaluate()
  }

  case class RelationQuery[A <: Relatable, B <: Relatable](ays : SetComprehension[A], bees : SetComprehension[B], relation : Relation[A,B]) {

     def evaluate() : Boolean = {
      val a_set = ays.getSet()
      val b_set = bees.getSet()

      val firstAll = !ays.any
      val secondAll = !bees.any
      val firstAny = ays.any
      val secondAny = bees.any

      val not = ays.not || bees.not

      val success = if(firstAll && secondAll) {
         a_set.forall { first =>
          b_set.forall { second =>
              relation.checkTypes(first,second) && ExecuteContextAction(determiningRelation(relation), subject = first.asInstanceOf[Relatable], target = second.asInstanceOf[Relatable]).res
          }
        }
      } else if (firstAny && secondAll) {
        a_set.exists { first =>
          b_set.forall { second =>
            relation.checkTypes(first,second) && ExecuteContextAction(determiningRelation(relation), subject = first.asInstanceOf[Relatable], target = second.asInstanceOf[Relatable]).res
          }
        }

      } else if (firstAny && secondAny) {
        a_set.exists { first =>
          b_set.exists { second =>
            relation.checkTypes(first,second) && ExecuteContextAction(determiningRelation(relation), subject = first.asInstanceOf[Relatable], target = second.asInstanceOf[Relatable]).res
          }
        }
      } else if (firstAll && secondAny) {
        a_set.forall { first =>
          b_set.exists { second =>
            relation.checkTypes(first,second) && ExecuteContextAction(determiningRelation(relation), subject = first.asInstanceOf[Relatable], target = second.asInstanceOf[Relatable]).res
          }
        }
      } else false

      if(not) !success else success
    }

  }
}

implicit object NotAQuery extends RelationQueryContext {
  val stack : mutable.Stack[ArrayBuffer[RelationQuery[?,?]]] = mutable.Stack()
}


abstract class ConditionalRelation[S <: Relatable, T <: Relatable] extends Relation[S,T] {
  def condition(s : S, t : T) : Boolean

  inflict(determiningRelation(this)) {
    if condition(subject.resolve.asInstanceOf[S], arg1.resolve.asInstanceOf[T]) then succeed
    fail
  }

}

class Relation[S <: Relatable : TT as _ttS, T <: Relatable : TT as _ttT]  {

  val ttSource = _ttS
  val ttTarget = _ttT


  def checkTypes(s : Relatable, t : Relatable) : Boolean = {
    ttSource.unapply(s).isDefined && ttTarget.unapply(t).isDefined
  }


  /*
  object gettingRelatedSet extends Action(1) with Context[mutable.HashSet[T]] with SystemAction

  // lower priority to make sure that default inflict statements execute before this one.
  inflict(gettingRelatedSet, Priority(-1)) {
    val currentSet = gettingRelatedSet.GetActionContext()
    currentSet.addAll(subject.getRelatedSetFromDictionaries(this))
  }

  */



  val precedence = QueryPrecedence.Generic

  type Source = S | SetComprehension[S]
  type Target = T | SetComprehension[T]

  relations.addOne(this)

  def narrowSource[X <: Source](x: X): SetComprehension[S] = {
    x match {
      case set: SetComprehension[?] => set.asInstanceOf[SetComprehension[S]]
      case relatable: Relatable => () => Seq(relatable.asInstanceOf[S])
    }
  }

  def narrowTarget[X <: Target](x: X): SetComprehension[T] = {
    x match {
      case set: SetComprehension[?] => set.asInstanceOf[SetComprehension[T]]
      case relatable: Relatable => () => Seq(relatable.asInstanceOf[T])
    }
  }


  def relates[A <: Source, B <: Target](_a: A, _b: Seq[B]) : A = {
    val scs = _b.map(narrowTarget)
    relates(_a, CombinedComprehension(scs*))
  }

  def reverseRelates[A <: Source, B <: Target](_b : B, _a: Seq[A]) : B = {
    val scs = _a.map(narrowSource)
    reverseRelates(_b, CombinedComprehension(scs*))
  }

  def relates[A <: Source, B <: Target](_a: A, _b: B): A = {
    makeRelation(narrowSource(_a), narrowTarget(_b))
    _a
  }

  def reverseRelates[A <: Source, B <: Target](_b: B, _a: A): B = {
    makeReverseRelation(narrowTarget(_b), narrowSource(_a))
    _b
  }


  private def makeRelation(a: SetComprehension[S], b : SetComprehension[T])(using ctx : RelationQueryContext) : Unit = {

    val not = a.not || b.not

    val secretMode = ctx == NotAQuery && NotAQuery.stack.nonEmpty
    if(secretMode){
      NotAQuery.stack.top.addOne(RelationQuery(a,b, this))
    }
    else if(ctx != NotAQuery) {
      ctx.queries.addOne(RelationQuery(a,b, this))
    } else {
      val a_filtered = a.getSet()
      val b_filtered = b.getSet()

      for(a_node <- a_filtered){
        if(!not)
          a_node.addRelated(this, b_filtered *)
        else
          a_node.removeRelated(this, b_filtered *)

      }
    }
  }

  private def makeReverseRelation(b : SetComprehension[T], a: SetComprehension[S])(using ctx : RelationQueryContext) : Unit = {

    val not = a.not || b.not

    val secretMode = ctx == NotAQuery && NotAQuery.stack.nonEmpty
    if(secretMode){
      NotAQuery.stack.top.addOne(RelationQuery(a,b, this))
    }
    else if(ctx != NotAQuery) {
      ctx.queries.addOne(RelationQuery(a,b, this))
    } else {
      val a_filtered = a.getSet()
      val b_filtered = b.getSet()
      for(a_node <- a_filtered){
        if (!not)
          a_node.addRelated(this, b_filtered *)
        else
          a_node.removeRelated(this, b_filtered *)
      }
    }
  }

}

abstract class ReciprocalRelation[S <: Relatable : TT, T <: Relatable : TT] extends Relation[S, T] {
  def getReciprocal : Relation[T,S]
  def reciprocates[A <: Source, B <: Target](_a: A, _b: B): A = {
    getReciprocal.relates(_b, _a)
    relates(_a, _b)
  }
}


object RelationsTest extends App {

  object House extends Room {

    override val name: StringExpression = "Relation Testing House"
    override val description: StringExpression = "This house is sure to test your relationships."


    val horse = ~"magnificent"
    val legs = ~"beefy butt holders"
    val statue = ~"it has arms and legs" made_from legs
    val arms = ~"pool noodles" makes statue
    val feet = Box("better left unmentioned")

    if(arms makes statue?){
      println("he got arms")
    }

    /*
    if( (statue made_from legs?) && !(feet makes statue?) ){
      println("and legs, but no feet")
    }

    feet makes statue

    if ((statue made_from legs?) && !(feet makes statue?)) {
      println("and legs, but no feet")
    }
  */

    val box = Box("may conceal something hidden")

    val hat = Box("not you again") holds horse holds statue

    val key = ~"what might this unlock?" made_from (horse, feet)



    val sc : SetComprehension[Nothing] = ???

    val notSc = !sc

    val what3 = hat holds key?

    val what = sc holds key
    val what2 = sc holds key?
    val what4 = !sc holds key?

    val what5 = !feet makes statue?



    println(statue.queryRelated(Composition))
  }

  House



}


