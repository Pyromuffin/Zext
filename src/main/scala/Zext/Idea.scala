package Zext

import Zext.Actions.printing_name
import Zext.Idea.allIdeas
import Zext.Interpreter.Say
import Zext.Relation.OneToMany
import Zext.Rule.{ExecuteAction, before, inflict, instead, report}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps


implicit object idea_knowing extends Relation[Thing, Idea] with OneToMany {
  extension [X <: Source](subject: X)
    infix def knows[Y <: Target](target: Y*): X = relates(subject, target)
}

implicit object idea_discovering extends Relation[Thing, Idea] with OneToMany {
  extension [X <: Source](subject: X)
    infix def can_discover[Y <: Target](target: Y*): X = relates(subject, target)
}


object Idea {
  val allIdeas = ArrayBuffer[Idea]()

  // known ideas are always visible.
  // visibility in this sense means that they can be parsed.
  inflict(determiningVisibility, (subject knows noun?) || noun[Idea].properties.contains(built_in)) {
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

    inflict(printing_name, player can_discover noun?) {
      val name = printing_name.GetActionContext()
      printing_name.SetActionContext( ControlCodes.orange + name + ControlCodes.normalControlCode)
      replace
    }


    report(ideating) {
      val knownIdeas = subject.relations(idea_knowing)
      val discoverableIdeas = subject.relations(idea_discovering).filter(_.obvious)
      val ideas = knownIdeas.concat(discoverableIdeas).filterNot(_.properties.contains(built_in))
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
class Idea(override val name : StringExpression, var obvious : Boolean = false) extends ZextObject {
  override val description = "the idea of " + name
  properties += proper
  allIdeas.addOne(this)
}

