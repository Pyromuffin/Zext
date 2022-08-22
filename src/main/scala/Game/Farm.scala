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

  val tv = new device {
    name = "tv"
    aliases.addOne("television")
    offDesc = "The tv lies dormant, you can turn it on."
    onDesc  = "Light dances across the dome of the CRT."
    has(fixed)

    report(turningOn, this){
      Say(s"$noun reluctantly flickers to life.")
    }

    report(turningOff, this) {
      Say(s"With a protest of static, $noun blinks off.")
    }

    everyTurnRules += {
      if(on && location == FarmHouse && Randomly(4)) Say("The tv crackles in the background")
    }
  }





}
