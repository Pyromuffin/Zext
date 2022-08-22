package Game

import Zext.Parser.*
import Zext.*
import Zext.Interpreter.*
import Zext.thing.*
import Zext.Direction.*
import JunimoGame.*
import Zext.Actions.*
import Zext.Rule.*
import Zext.World.*

class device extends thing {

  var on = false
  def off = !on

  var offDesc : StringExpression = null
  var onDesc :  StringExpression = null
  description = s"${if(on) onDesc else offDesc}"

  object turningOn extends Action("turn on", "switch on", "activate")
  object turningOff extends Action("turn off", "switch off", "deactivate")
  object switching extends Action("switch", "toggle")

  carryOut[device](turningOn) { d =>
    if (d.on) {
      Say(s"$noun is already on")
      false
    } else {
      d.on = true
      true
    }
  }

  report[device](turningOn) { d =>
    Say(s"I turned on $noun")
  }

  carryOut[device](turningOff) { d =>
    if (d.off) {
      Say(s"$noun is already off")
      false
    } else {
      d.on = off
      true
    }
  }

  report[device](turningOff) { d =>
    Say(s"I turned off $noun")
  }

  carryOut[device](switching){ d =>
    if(d.on) execute(turningOff, d)
    else execute(turningOn, d)
  }
}


object FarmHouse extends Room {
  name = s"$farm farmhouse"
  description = s"$farm is where the magic happens. That place is outside of this place."
  OutsideWorld connect south

  val tv = new device {
    name = "tv"
    aliases.addOne("television")
    offDesc = "The tv lies dormant, you can turn it on."
    onDesc = "Light dances across the dome of the CRT."
    has(fixed)

    report(turningOn, this) {
      Say(s"$noun reluctantly flickers to life.")
    }

    report(turningOff, this) {
      Say(s"With a protest of static, $noun blinks off.")
    }

    everyTurnRules += {
      if (on && location == FarmHouse && Randomly(4)) Say("The tv crackles in the background")
    }
  }

  val bed = ~"The place where the real magic happens. Soft sheets, the smell of you, safety. Make sure you're here by 2 am or who *knows* what might happen to you." is fixed aka "love nest" aka "pile of sheets"

  val door = ~"This is a thing you really wish you could open and close, but you can't"

  object coffee_machine extends device {
    name = "coffee maker"
    aliases.addOne("unholy espresso machine").addOne("coffee monster").addOne("cm")

    var tamped = false

    proper = true

    offDesc = "Sleepy, just like you."
    onDesc = "Constant disturbing grinding and whining, the machine performs miracles before your eyes"
    has(fixed)

    report(turningOn, this) {
      Say(s"$noun awakens from its slumber.")
    }

    report(turningOff, this) {
      Say(s"Spent but angry, $noun comes to a stop.")
    }

    everyTurnRules += {
      if (on && location == FarmHouse && Randomly(4)) Say("The coffee machine looms")
    }
  }

  coffee_machine.tamped = false
  object tamping extends Action("tamp", "smack")


  carryOut(tamping, coffee_machine) { cm =>
    if (coffee_machine.tamped == true)
      Say ("It's as tamped as it's gonna get without your hoe")
    else
      Say ("You tamp dat ass")

    coffee_machine.tamped = true
    true
  }

  carryOut[ZextObject](tamping){ _=>
    Say (s"$noun doesn't give a heck. It's tamp-er proof.")
    false
  }


}

object OutsideWorld extends Room {
  ChickenCoop connect west
  name = "Porch"
  description = s"Ah, yes, the great outdoors. $farm lies before you. You feel the wood planks beneath your feet. Your chicken coop is west of here."

  val crops = ~"You have lovely little fwends growing in neat stupid fucking rows divided by pointless cobblestones."


}


object ChickenCoop extends Room {
  name = "Coop"
  description = "You are in a little wooden coop. So many fluffy feathery chicken friends surround you."
  // transitiontext = "You duck into the hatch, because doors are for losers. The chickens like it when you do things their way."

  val chickens = ~"There are some cute lil chickens waiting for your love." aka "chicks" aka "chicken" aka "fluffballs" aka "cuties"

  chickens.plural = true

  report(taking, chickens) {
    Say("You scoop up every chicken and shove them in your trousers. They purr contentedly, sending vibrations through your body")
  }




  object pet extends Action("pet", "hug", "pat", "love")

  chickens.petted = false

  carryOut(petting, chickens) { c =>
    if (chickens.petted == true)
      Say("You pet the chickens again, extra hard. They make little contented clucks but don't love you any harder.")
    else
      Say("You pet each and every chicken. They let our little <3's and love you even more now.")

    chickens.petted = true
    true
  }

  carryOut[ZextObject](tamping) { _ =>
    Say(s"$noun doesn't give a heck. It's tamp-er proof.")
    false
  }
}