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





object FarmHouse extends Room {
  name = s"$farm farmhouse"
  description = s"$farm is where the magic happens. That place is outside of this place."



  Connect(south, OutsideWorld)
  report(going, south, here) {
    Say("Closing the door behind, you emerge into the sunlight")
  }

  // probably put this somewhere better
  inflict[Supporter](examining) { s =>
    Say(s.description)
    s.contents.foreach(z => Say(z.name))
    true
  }

  val drawer = new Supporter {
    val pencil = ~"graphite imprisoned with carved wood."

    Understand(pencil, "prisoner"){
      pencil.parentContainer == this
    }

    report[ZextObject](taking, pencil, this had pencil){ _ =>
      Say("The prisoner is free of their shackles")
    }

    instead[thing, thing](putting, pencil, this.asSecondNoun) { (_,_) =>
      Say("the pencil squeals \"No! I will never go back!\"")
    }

    report[thing, thing](putting, this.asSecondNoun) { (_,_) =>
      Say(s"the drawer eases open to accept $noun")
    }


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
  object tamping extends Action("tamp", "smack")


  inflict(tamping, coffee_machine) { cm =>
    if (coffee_machine.tamped == true)
      Say ("It's as tamped as it's gonna get without your hoe")
    else
      Say ("You tamp dat ass")

    coffee_machine.tamped = true
    true
  }

  inflict[ZextObject](tamping){ _=>
    Say (s"$noun doesn't give a heck. It's tamp-er proof.")
    false
  }
  instead(taking, coffee_machine, this){
    Say( str = "Wow that's a lot heavier than it should be. Oh, right, you glued it down after that special night with the Wizard. With a grunt, you narrowly avoid dropping it on your foot as you put it back down.")
  }

  report(smelling, drawer, this){  //want this to be pencil but cant yet
    Say(Randomly( "You fill your nostrils with pencil. It hurts but also smells like wood.", "Cedar, like you expected. But also, the enchanting smell of sandalwood, lingering from the desk drawer.", "You poked your sinuses with a pencil. Smells like pain."))
  }

  report(dropping, drawer, this) {
     Say(str = "You drop your drawer.")
  }

  report(taking, drawer, this) {
    Say(Randomly("You take the drawer and balance it on your head. Maybe you can carry stuff like that?", "You took the drawer out of the desk and slid in into your pants. Now you can store things in a drawer in your pants."))
  }


}


object Vegetable {

  before[Vegetable](taking) { v =>
    if (v.ripe) {
      Say("Up you go!")
      true
    }
    else {
      Say("We do not harvest the young")
      false
    }
  }

  before[Vegetable](examining) { v =>

    if (v.wilted) {
      Say(s"A slow tear runs down your face, but even a river of tears could not bring $noun back to life")
    }
    if (!v.wilted) {
      Say(s"A slow tear of happiness runs down your cheek, adding one extra drop of moisture to your healthy $noun")
    }
    if (v.ripe) {
      Say("yum yum!")
    }

    true
  }

}

class Vegetable(using c : Container) extends thing {
  var wilted = false
  var watered = false
  var ripe = false
}

object OutsideWorld extends Room {

  Vegetable

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



class Animal(using c : Container) extends thing {
  var petted = false
  before(taking, this){
    Say (s"$noun! eye you expectantly.")
}
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

  val mayo_machine = ~"You look hopefully at the mayo machine. You left some void egg in there last night." aka "mm"
  /* property empty vs full (full of mayo)
  "You reverently take the fresh void mayo out of the machine. It contains universes within, in a convenient colloidal suspension"
  "Emptiness. Like the wrong kind of void."
   */


  val chickens = new Animal named "chickens" desc "There are some cute lil chickens waiting for your love." aka "chicks" aka "fluffballs" aka "cuties" amount some

  report(taking, chickens) {
    Say("You scoop up every chicken and shove them in your trousers. They purr contentedly, sending vibrations through your body")
  }

  object petting extends Action("pet", "hug", "pat", "love")

  chickens.petted = false

  inflict(petting, chickens) { c =>
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

//
//  inflict(taking, hay){
//    if (hay.parentContainer == inventory){
//      Say("Not so fast, Scarecrow Steve! Leave some for the chickens")
//      false
//    }
//    else{
//      val new_hay = hay
//    }
//  }
  before(taking, hay){
    if (hay.parentContainer == inventory) {
      Say("Not so fast, Scarecrow Steve! Leave some for the chickens")
      false
    }
    else
      true
  }

  report(taking, hay, here) {
    Say("you scoop up a nice armful of hay and stuff it in your sack for later.")
  }

  after(taking, hay) {
    Say("You aren't really sure why you want that but you do.")
  }

  val hay_box = ~"tray of hay" is fixed aka "trough" aka "food" aka "feed"

  val odors = ~"The pleasant aroma of feathers and mayonnaise intertwine here" are scenery


  instead(going, east, this) {
    // if(no may in inventory
    Say("But, but, the mayo, the chickens!")
  }

  instead(going, north, this) {
    Say("You lift up one leg. The chickens look at you reproachfully. You change your mind about getting into the nest.")
  }


  instead(going, west, here) {
    val dix =
      if (void_mayo.parentContainer == inventory) {
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








