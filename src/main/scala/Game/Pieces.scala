package Game

import Zext.*
import Zext.Actions.{dropping, putting}
import Zext.Actions.putting.Zontainer
import Zext.Rule.*
import Zext.Thing.NounAmount.*
import Zext.ZextObject.*
import Zext.Condition.*
import Zext.Inflector.pluralize
import Zext.Interpreter.*
import Zext.World.*

import javax.print.attribute.standard.Destination



object Pieces {

  inflict(putting, of[Pieces], ofSecond[Zontainer]) {

    val pieces = noun[Pieces]
    val name = pieces.name
    val amountToPut = NumberPrompt(s"Put how many $name?")

    if (amountToPut > pieces.quantity) {
      Say(s"You don't have that many $name")
      stop
    }

    pieces.split(amountToPut, secondNoun[Container])

    val unitWord = if (amountToPut == 1) pieces.unitName else pluralize(pieces.unitName)

    Say(s"You expel $amountToPut $unitWord of $name into the $secondNoun.")
    stop // suppress reporting
  }

  inflict(dropping, of[Pieces]) {
    val pieces = noun[Pieces]
    val name = pieces.name
    val amountToDrop = NumberPrompt(s"Drop how many $name?")

    if(amountToDrop > pieces.quantity){
      Say(s"You don't have that many $name")
      stop
    }

    pieces.split(amountToDrop, currentLocation)

    val unitWord = if(amountToDrop == 1) pieces.unitName else pluralize(pieces.unitName) 
    val verb = if(amountToDrop == 1) "spills" else "spill"

    Say(s"$amountToDrop $unitWord of $name $verb onto the floor.")
    stop // don't do the usual report for dropping.
  }

  // it may be tempting to create report rules for this action, but i don't have a way of changing the target of an action during the execution of a rule.
  // this may be a useful thing at some point.

}

abstract class Pieces(var quantity : Int)(using c : Container) extends Thing with Cloneable {

  amount(some)

  var unitName = "piece"

  def split(amount : Int, destination: Container) : Unit = {

    if(amount == quantity){
      this.transferTo(destination)
      return
    }

    val typeOfPiece = this.getClass
    val currentlyHeld = destination.contents.find(_.getClass == typeOfPiece)

    if (currentlyHeld.nonEmpty) {
      val pieces = currentlyHeld.get.asInstanceOf[Pieces]
      this.quantity -= amount
      pieces.quantity += amount
    } else {
      val other = this.clone().asInstanceOf[this.type]

      quantity -= amount
      other.quantity = amount
      other.parentContainer = destination
      destination.contents.append(other)
    }
  }

  override def definite = {
    super.definite + s" ($quantity)"
  }

  override def indefinite = {
    super.indefinite + s" ($quantity)"
  }

  override def transferTo(container: Container) : Unit = {

    val typeOfPiece = this.getClass
    val currentlyHeld = container.contents.find(_.getClass == typeOfPiece)
    if (currentlyHeld.nonEmpty) {
      val pieces = currentlyHeld.get.asInstanceOf[Pieces]
      pieces.quantity += quantity
      Destroy(this)
    }
    else {
      super.transferTo(container)
    }
  }

}
