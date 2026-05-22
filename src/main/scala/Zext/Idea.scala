package Zext

import Zext.Actions.printing_name
import Zext.Idea.allIdeas
import Zext.Interpreter.Say
import Zext.Relation.{ManyToMany, OneToMany, RelationQuery}
import Zext.ControlCodes.*

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import Zext.Condition.*
import Zext.Infliction.*
import Zext.RuleContext.*
import Zext.idea_discovering.can_discover
import Zext.idea_knowing.knows
import scala.util.NotGiven



// this can be super psycho
// we have essentially 3 places where we will be using these relation statements
// definition context = val hat : Thing =~"hattable" is wet
// query context =  hat is wet? : RelationQuery[Thing, Property]
// rule context = hat is wet : PendingRelation, or just Thing, i suppose
// the question is that can we use givens to distinguish between query context and definition context
// we can distinguish between query and rule context by using a default context object
// maybe we can use something like the prescence of the implicit Zcontainer?

type RelationQuestion[S <: Relatable, T <: Relatable, A, B] = PendingRelation[S,T,A,B] ?=> Question[A]


def make[S <: Relatable, T <: Relatable, A, B](question : PendingRelation[S,T,A,B] ?=> Question[A]) : PendingRelation[S,T,A,B] = {
  val q = question(using null)
}

val x = make(noun knows subject?)
val y = noun knows subject

object idea_knowing extends Relation[Relatable, Idea] with ManyToMany {
  extension [X <: Source](subject: X) {
    infix def knows[Y <: Target](target: Y*)(using pr: PendingRelation[Relatable, Idea, X, Y] = null): X = relates(subject, target)

  }
}

object idea_discovering extends Relation[Relatable, Idea] with ManyToMany {
  extension [X <: Source](subject: X)
    infix def can_discover[Y <: Target](target: Y*): X = relates(subject, target)

}

object Idea {
  object innate extends Property // for ideas that everyone starts with
  object obvious extends Property // for ideas that appear in the idea list automatically

  val allIdeas = ArrayBuffer[Idea]()

  // knows requires a Relatable & Zext.idea_knowing.Target (Idea, in this case) for the second param,
  // but the context type passed for determiningVisibility is Relatable, Relatable.
  // i think this means that if we can't find a context then we should just assume failure?
  // essentially the type checker sees that noun is returning a relatable, and knows requires an Idea
  // this stupid context error is really just a typechecking error for knows requiring an Idea.
  // should we have a default context with everything set to nothing, so it can cast to any type?

  
  // known ideas are always visible.
  // this is so we can say stuff like go north (north, being an idea)
  inflict(determiningVisibility, subject knows noun?) {
      succeed
  }

  inflict(idea_knowing.determining, innate) {
    succeed
  }

  object thinking extends SingleAction[Idea](1, "think", "think of", "imagine", "think about") {

    // allow discoverable ideas to be thought of, but are otherwise not interactable.
    inflict(determiningVisibility(thinking), subject can_discover noun?) {
      succeed
    }

    before(thinking, subject can_discover noun? ) {
      subject knows noun
      subject.removeRelated(idea_discovering, noun)

      Say(s"A new thought about $noun occurs to you!")
    }

    report(thinking, subject knows noun?) {
      Say(s"Thinking of $noun reveals: ${noun.description}")
    }
  }

  object ideating extends SelfAction[Thing](0, "ideas", "thoughts", "knowledge") {

    before(ideating, first) {
      // make all innate ideas discoverable
      val known = subject.queryRelated(idea_knowing).filter( x => (x is innate?).evaluate )
      subject can_discover known
    }


    inflict.returns(printing_name, player can_discover noun[Idea]?) { name =>
      name.bold
    }


    report(ideating) {
      val knownIdeas = subject.queryRelated(idea_knowing)
      val obviousIdeas = subject.queryRelated(idea_discovering).filter(x => (x is obvious?).evaluate )
      val ideas = knownIdeas.concat(obviousIdeas)
      val ideasList = ListNamesNicely(ideas.toSeq)
      if(ideasList.isEmpty){
        Say("Much is unknown.")
      } else {
        Say("The following ideas are known to you: " + ideasList.get)
      }
    }
  }
}

// obvious ideas appear in the idea list as soon as they are discoverable.
// discoverable ideas can be added to the ideas list by thinking about them
// ideas with the property unlisted don't appear in the idea list.
class Idea(override val name : StringExpression) extends ZextObject {
  override val description = "the idea of " + name
  this is proper
  allIdeas.addOne(this)
}

