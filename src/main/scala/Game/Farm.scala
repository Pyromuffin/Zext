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
import Zext.thing.NounAmount.*


class device(using Container) extends thing {

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

  Connect(south, OutsideWorld)
  report(going, south, here) {
    Say("Closing the door behind, you emerge into the sunlight")
  }

  // probably put this somewhere better
  carryOut[Supporter](examining) { s =>
    Say(s.description)
    s.contents.foreach(z => Say(z.name))
    true
  }

  val drawer = new Supporter {
    val pencil = ~"graphite imprisoned with carved wood."
  } named "drawer" desc "debris collector"


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
  instead(taking, coffee_machine, this){
    Say( str = "Wow that's a lot heavier than it should be. Oh, right, you glued it down after that special night with the Wizard. With a grunt, you narrowly avoid dropping it on your foot as you put it back down.")
  }




}

object OutsideWorld extends Room {

  name = "Porch"
  description = s"Ah, yes, the great outdoors. $farm lies before you. You feel the wood planks beneath your feet. Your chicken coop is west of here."

  val crops = ~"You have lovely little fwends growing in neat stupid fucking rows divided by pointless cobblestones." aka "plants" amount plural

  val parsnip= ~"A single perfect parsnip, ripe and ready"
  // "Pop! You gently but firmly yank the parsnip, extricating it from its bed."
  //can u like hack their frikkin save file for the season or something and make it seasonally appropriate produce
  // yeah probably

  instead(taking, crops) {
    Say("They are just babies! Don't be a cradle robber. Wait until they're old enough to eat at least.")
  }

  Connect(west, ChickenCoop)
  report(going, west, here) {
    Say("You duck into the hatch, because doors are for losers. The chickens like it when you do things their way.")
  }

}


class Animal(using c : Container) extends thing {
  var petted = false
}
object ChickenCoop extends Room {
  name = "Coop"
  description = "You are in a little wooden coop. So many fluffy feathery chicken friends surround you."

  val void_mayo = ~"At last, the final piece of the puzzle, the icing on the cake, the cap on the marker, the bonnet on the bee, the kangaroo in the pouch. You've been waiting for so long to be able to present this mayo to the junimos. It's black, with red glistening spots. It's beautiful." aka "mayo" aka "mayonnaise"

  val mayo_machine = ~"You look hopefully at the mayo machine. You left some void egg in there last night."
  /* property empty vs full (full of mayo)
  "You reverently take the fresh void mayo out of the machine. It contains universes within, in a convenient colloidal suspension"
  "Emptiness. Like the wrong kind of void."
   */
  val chickens = new Animal named "chickens" desc "There are some cute lil chickens waiting for your love." aka "chicks" aka "chicken" aka "fluffballs" aka "cuties" amount plural

  report(taking, chickens) {
    Say("You scoop up every chicken and shove them in your trousers. They purr contentedly, sending vibrations through your body")
  }

  object petting extends Action("pet", "hug", "pat", "love")

  chickens.petted = false

  carryOut(petting, chickens) { c =>
    if (chickens.petted == true)
      Say("You pet the chickens again, extra hard. They make little contented clucks but don't love you any harder.")
    else
      Say("You pet each and every chicken. They let out little <3's and love you even more now.")

    chickens.petted = true
    true
  }

  val hay = ~"a mini bundle of soft yellow hay, great for eating and sleeping on" amount some
  /*container take text "you scoop up a nice armful of hay and stuff it in your sack for later."
   if inventory already has a hay, "Not so fast, Scarecrow Steve! Leave some for the chickens"
  * */

  after(taking, hay) {
    Say("You aren't really sure why you want that but you do.")
  }

  val hay_box = ~"tray of hay" is fixed aka "trough" aka "food" aka "feed"

  val odors = ~"The pleasant aroma of feathers and mayonnaise intertwine in this pleasant place" are scenery


  instead(going, east, this) {
    // if(no may in inventory
    Say("but, but, the mayo, the chickens!")
  }

  instead(going, north, this) {
    Say("You lift up one leg. The chickens look at you reproachfully. You change your mind about getting into the nest.")
  }

}
