package Zext

import Zext.Actions.printing_name
import Zext.Idea.allIdeas
import Zext.Interpreter.Say
import Zext.Relation.OneToMany
import Zext.Rule.{ExecuteAction, before, inflict, instead, report}
import Zext.ControlCodes.*

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import Zext.Condition.*
import Zext.RuleContext.first

implicit object idea_knowing extends Relation[Thing, Idea] with OneToMany {
  extension [X <: Source](subject: X)
    infix def knows[Y <: Target](target: Y*): X = relates(subject, target)
}

implicit object idea_discovering extends Relation[Thing, Idea] with OneToMany {
  extension [X <: Source](subject: X)
    infix def can_discover[Y <: Target](target: Y*): X = relates(subject, target)
}




object Idea {
  object innate extends Property // for ideas that everyone starts with
  object obvious extends Property // for ideas that appear in the idea list automatically

  val allIdeas = ArrayBuffer[Idea]()

  // known ideas are always visible.
  // this is so we can say stuff like go north (north, being an idea)
  inflict(determiningVisibility, subject knows noun?) {
    replace
  }

  inflict(determiningRelation(idea_knowing), innate) {
    replace
  }

  object thinking extends Action(1, "think", "think of", "imagine", "think about") {

    // allow discoverable ideas to be thought of, but are otherwise not interactable.
    inflict(determiningVisibility(thinking), subject can_discover noun?) {
      replace
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

  object ideating extends Action(0, "ideas", "thoughts", "knowledge") {

    before(ideating, first) {
      // make all innate ideas discoverable
      val known = subject.relations(idea_knowing).filter(_.hasProp(innate))
      subject can_discover known
    }


    inflict(printing_name, player can_discover noun?) {
      val name = printing_name.GetActionContext()
      printing_name.SetActionContext(name.orange)
      replace
    }


    report(ideating) {
      val knownIdeas = subject.relations(idea_knowing)
      val obviousIdeas = subject.relations(idea_discovering).filter(_.hasProp(obvious))
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
// ideas with the property built_in don't appear in the idea list.
class Idea(override val name : StringExpression) extends ZextObject {
  override val description = "the idea of " + name
  properties += proper
  allIdeas.addOne(this)
}

