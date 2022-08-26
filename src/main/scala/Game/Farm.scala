package Game

import Zext.Parser.*
import Zext.*
import Zext.Interpreter.*
import Zext.thing.*
import Zext.device.*
import Zext.Direction.*
import JunimoGame.*
import Zext.Actions.*
import Zext.Rule.*
import Zext.World.*
import Zext.thing.NounAmount.*
import Zext.Inflector.*
import Zext.device.{turningOff, turningOn}
import Condition.*
import reflect.Selectable.*


object FarmHouse extends Room {

  Macros.packageToucher(JunimoGame) // leave this here for now, maybe not the best place for it.

  name = s"$farm farmhouse"
  description = s"$farm is where the magic happens. That place is outside of this place."

  report(going, south, here) Say "Closing the door behind, you emerge into the sunlight"


  // probably put this somewhere better
  inflict(examining, of[Supporter]) {
    Say(noun.description)
    noun.as[Supporter].contents.foreach(z => Say(z.name))
    true
  }

  val drawer = new Supporter {
    val pencil = ~"graphite imprisoned with carved wood."
    name = "drawer"
    description = "debris collector"

    Understand(pencil, "prisoner"){
      pencil.parentContainer == this
    }

    report(taking, pencil, this had pencil) Say "The prisoner is free of their shackles"
    instead(putting, pencil, this.asSecondNoun) Say "the pencil squeals \"No! I will never go back!\""
    report(putting, this.asSecondNoun) Say s"the drawer eases open to accept $noun"
    
    

  }

  val tv = new device {
    name = "tv"
    aliases.addOne("television")
    offDesc = "The tv lies dormant, you can turn it on."
    onDesc = "Light dances across the dome of the CRT."
    has(fixed)

    report(turningOn, this) Say s"$noun reluctantly flickers to life."
    report(turningOff, this) Say s"With a protest of static, $noun blinks off."

    everyTurnRules += {
      if (on && currentLocation == FarmHouse && Randomly(4)) Say("The tv crackles in the background")
    }

  }

  val bed = ~"The place where the real magic happens. Soft sheets, the smell of you, safety. Make sure you're here by 2 am or who *knows* what might happen to you." is fixed aka "love nest" aka "pile of sheets"
  val door = ~"This is a thing you really wish you could open and close, but you can't"

  object coffee_machine extends device {
    name = "coffee maker"
    aliases.addOne("unholy espresso machine").addOne("coffee monster").addOne("cm")

    var tamped = false

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
      if (on && currentLocation == FarmHouse && Randomly(4)) Say("The coffee machine looms")
    }
  }

  coffee_machine.tamped = false
  object tamping extends Action(1, "tamp", "smack", "compress")


  inflict(tamping, coffee_machine) {
    if (coffee_machine.tamped)
      Say ("It's as tamped as it's gonna get without your hoe")
    else
      Say ("You tamp dat ass")

    coffee_machine.tamped = true
    true
  }

  inflict(tamping){
    Say (s"$noun doesn't give a heck. It's tamp-er proof.")
    false
  }

  instead(taking, coffee_machine) Say "Wow that's a lot heavier than it should be. Oh, right, you glued it down after that special night with the Wizard. With a grunt, you narrowly avoid dropping it on your foot as you put it back down."
  report(smelling, drawer.pencil) Say Randomly( "You fill your nostrils with pencil. It hurts but also smells like wood.", "Cedar, like you expected. But also, the enchanting smell of sandalwood, lingering from the desk drawer.", "You poked your sinuses with a pencil. Smells like pain.")
  report(dropping, drawer) Say "You drop your drawer."
  report(taking, drawer) Say Randomly("You take the drawer and balance it on your head. Maybe you can carry stuff like that?", "You took the drawer out of the desk and slid in into your pants. Now you can store things in a drawer in your pants.")

}


object Vegetable {

  before(taking, of[Vegetable] ) {
    val v = noun.as[Vegetable]
    if (v.ripe) {
      Say("Up you go!")
      true
    }
    else {
      Say("We do not harvest the young")
      false
    }
  }

  before(examining, of[Vegetable]) {
    val v = noun.as[Vegetable]
    if (v.wilted) {
      Say(s"A slow tear runs down your face, but even a river of tears could not bring $noun back to life")
    }
    if (!v.wilted) {
      Say(s"A slow tear of happiness runs down your cheek, adding one extra drop of moisture to your healthy $noun")
    }
    if (v.ripe) {
      Say("Yum yum!")
    }

    true
  }

}

class Vegetable(using c : Container) extends thing {
  var wilted = false
  var watered = false
  var ripe = false
}


object Animal{
  before(taking, of[Animal]){
    Say (s"$noun! eye you expectantly.")
    true
  }
}

class Animal(using c : Container) extends thing {
  var petted = false

}

object OutsideWorld extends Room {

  Connect(north, FarmHouse)

  name = "Porch"
  description = s"Ah, yes, the great outdoors. $farm lies before you. You feel the wood planks beneath your feet. Your chicken coop is west of here."

  val crops = ~"You have lovely little fwends growing in neat stupid fucking rows divided by pointless cobblestones." aka "plants" amount some

  val parsnip= ~"A single perfect parsnip, ripe and ready"

  report(taking, parsnip) {
    Say("Pop! You gently but firmly yank the parsnip, extricating it from its bed.")
  }

  val parsnips = new Vegetable named "baby parsnips" desc "These parsnips are young and unprepared to leave their homes." amount plural

  val seedlings = new Vegetable named "seedlings" desc "There guys look dry and sad" // ripe= false watered = false
  instead(taking, crops) {
    Say("They are just babies! Don't be a cradle robber. Wait until they're old enough to eat at least.")
  }

  report(going, west, here) {
    Say("You duck into the hatch, because doors are for losers. The chickens like it when you do things their way.")
  }

  Connect(west, ChickenCoop)

  Connect(east, Path)


}






object Path extends Room {

  name = "Path to Town"
  description = s"You are on the slow journey, at walking pace, from $farm to The Greater SDV Area"
  val three_wiggly_things_in_the_ground = ~"There are some creepy little periscopes. They dance and wriggle, begging you for a tamping. If only you still had your hoe." aka "strings" aka "three" aka "fingers" aka "eels" aka "wiggly things"

  report(going, west, here) {
    Say(Randomly("With one last glance over your shoulder, you sigh and turn towards home.", "You try to run home but you can only ever walk."))
  }

  report(going, east, here) {
    Say(Randomly("You gird your loins, take a deep breath, and ready yourself to face The Town.", "You enter the sprawling metropolis of Stardew Valley."))
  }


}

object ChickenCoop extends Room {
  name = "Coop"
  description = "You are in a little wooden coop. It smells nice. So many fluffy feathery chicken friends surround you."

  val void_mayo = ~"At last, the final piece of the puzzle, the icing on the cake, the cap on the marker, the bonnet on the bee, the kangaroo in the pouch. You've been waiting for so long to be able to present this mayo to the junimos. It's black, with red glistening spots. It's beautiful." aka "mayo" aka "mayonnaise"

  val mayo_machine = ~"You look hopefully at the mayo machine. You left some void egg in there last night." aka "mm" aka "machine"
  /* property empty vs full (full of mayo)
  "You reverently take the fresh void mayo out of the machine. It contains universes within, in a convenient colloidal suspension"
  "Emptiness. Like the wrong kind of void."
   */


  val chickens = new Animal named "chickens" desc "There are some cute lil chickens waiting for your love." aka "chicks" aka "fluffballs" aka "cuties" amount some

  report(taking, chickens) {
    Say("You scoop up every chicken and shove them in your trousers. They purr contentedly, sending vibrations through your body")
  }

  object petting extends Action(1,"pet", "hug", "pat", "love")

  chickens.petted = false

  inflict(petting, chickens) {
    if (chickens.petted)
      Say("You pet the chickens again, extra hard. They make little contented clucks but don't love you any harder.")
    else
      Say("You pet each and every chicken. They let out little <3's and love you even more now.")

    chickens.petted = true
    true
  }

  val hay_pile =  ~"a tangle of straw, bane of needle searchers." aka "hay trough" aka "trough" aka "feed"

  val hay = ~"a mini bundle of soft yellow hay, great for eating and sleeping on" amount some
  hay transferTo nowhere

  Understand(hay_pile, "hay"){
    hay.parentContainer == nowhere
  }

  instead(taking, hay_pile){
    if(hay.parentContainer == nowhere){
      Say("you scoop up a nice armful of hay and stuff it in your sack for later.")
      hay transferTo player
    } else {
      Say("Not so fast, Scarecrow Steve! Leave some for the chickens")
    }
  }


  after(taking, hay) {
    Say("You aren't really sure why you want that but you do.")
  }

  val odors = ~"The pleasant aroma of feathers and mayonnaise intertwine here" are scenery aka "odor" aka "scents" aka "aroma"


  instead(going, east, this) {
    // if(no may in inventory
    Say("But, but, the mayo, the chickens!")
  }

  instead(going, north, this) {
    Say("You lift up one leg. The chickens look at you reproachfully. You change your mind about getting into the nest.")
  }


  instead(going, west, here) {
    val dix =
      if (void_mayo.parentContainer == player) {
        Say("you got that sweet mayo, but there's no exit there anyways.")
        true
      }
      else {
        Say("you really want that mayo")
        false
      }
    if (dix) Say("The true dix are the friends you make along the way")
    else if (!chickens.petted) Say("you really should pet the chickens, idiot")

  }

}








