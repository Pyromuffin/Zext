package Game

import Game.smelling.scent
import Zext.Parser.*
import Zext.*
import Zext.exports.*
import JunimoGame.*


object smelling extends Action(1, "smell", "inhale", "snort", "vape", "endocytose", "flehm", "flehmen", "sniff", "nasalize") {

  case class scent(odor : StringExpression) extends Property


  report(smelling, reflexively) {
    Say(Randomly("You wrinkle your nose and lift your lips, giving' that vestigial Vomeronasal Organ another go", "Ah, the smells.", "Moved here for the grandpa, stayed for the smells."))
  }

  after(smelling, player has noun) {
    Say(s"The scent of $noun is soaked through your clothing now.")
  }

  report(smelling) Say s"You smell $noun."
  report(smelling, prop[scent]) Say s"$noun smells like ${noun[scent].odor}"

}


object FarmHouse extends Room {

  name = s"$farm Farmhouse"
  description = s"$farm is where the magic happens. That place is outside of this place."

  report(going, south, here) Say "Closing the door behind you, you emerge into the sunlight"


  val wine = ~Progressively("It is one second old", "it is two seconds old", "it is three seconds old", "you can't count any further")
  val jukebox = ~Shuffled("it's playing the theme to journey of the prairie king", "it's playing the theme to junimo kart", "it's playing the theme to the joja vending machine?" )
  val hat = ~"Hat"

  val butt = new Box {
    name = "butt"
    description = "debris expeller"
    report(putting, this.asSecondNoun) Say s"the butt eases open to accept $noun"
    this has scent("peaches. no cream")
    this has flavor("strangely flavorful")
    open = true
    transparent = false

    before(examining,this) {
      if (open == true) {
        Say("Is this a buttock that I see before me? The cheeks are loose")
      }
      else if (open == false) {
        Say("The cheeks are tightly pressed together")
        Say("It's lips are sealed, as they say")
      }
      else {
        Say("I dream of dix")
      }
    }
  }


  val drawer = new Box {
    val pencil = ~"graphite imprisoned with carved wood." worth 100
    name = "drawer"
    description = "debris collector"

    Understand(pencil, "prisoner"){
      pencil.parentContainer == this
    }

    pencil has flavor(Randomly("wood. Delicious, delicious wood", "a beaver delicacy", "sitting at your desk in kindergarten", "deep thoughts", "eraser flakes"))
    pencil has scent(Randomly( "You fill your nostrils with pencil. It hurts but also smells like wood.", "Cedar, like you expected. But also, the enchanting smell of sandalwood, lingering from the desk drawer.", "You poked your sinuses with a pencil. Smells like pain."))

    report(taking, pencil, this had pencil) Say "The prisoner is free of their shackles"
    instead(putting, pencil, this.asSecondNoun) Say "the pencil squeals \"No! I will never go back!\""
    report(putting, this.asSecondNoun) Say s"the drawer eases open to accept $noun"
  }

  object tv extends Device {
    name = "tv"
    aliases.addOne("television").addOne("TV")
    offDesc = "The tv lies dormant, ready to be turned on."
    onDesc = "Light dances across the dome of the CRT."
    has(fixed)
    this has flavor(Randomly("electrostatic shock", "static", "smooth glass", "dust. When was the last time you dusted this thing?"))

    report(turningOn, this) Say s"$noun reluctantly flickers to life."
    report(turningOff, this) Say s"With a protest of static, $noun blinks off."
    report(being, on, here, Randomly(4)) Say "The tv crackles in the background"
  }

  val bed = ~"The place where the real magic happens. Soft sheets, the smell of you, safety. Make sure you're here by 2 am or who *knows* what might happen to you." is fixed aka "love nest" aka "pile of sheets"
  val door = ~"This is a thing you really wish you could open and close, but you can't"

  val houseLucre = Lucre(100)

  object coffee_machine extends Device with Container {
    name = "coffee maker"
    aliases.addOne("unholy espresso machine").addOne("coffee monster").addOne("cm").addOne("coffee machine")

    var tamped = false

    open = false
    transparent = true

    offDesc = "Sleepy, just like you."
    onDesc = "Constant disturbing grinding and whining, the machine performs miracles before your eyes"
    has(fixed)
    this has flavor("coffee residue") has scent ("bitter regret")


    val coffee_grounds = ~"all that remains of yesterday's grind." amount some aka "grounds" aka "beans"
    coffee_grounds has scent("coffee") has flavor("bitter and a little moldy, but nice")

    report(turningOn, this) {
      Say(s"$noun awakens from its slumber.")
    }

    report(turningOff, this) {
      Say(s"Spent but angry, $noun comes to a stop.")
    }

    object tamping extends Action(1, "tamp", "smack", "compress")

    inflict(tamping, coffee_machine) {
      if (coffee_machine.tamped)
        Say("It's as tamped as it's gonna get without your hoe")
      else
        Say("You tamp dat ass")

      coffee_machine.tamped = true
    }

    inflict(tamping) {
      Say(s"$noun doesn't give a heck. It's tamp-er proof.")
    }

    instead(taking, coffee_machine) Say "Wow that's a lot heavier than it should be. Oh, right, you glued it down after that special night with the Wizard. With a grunt, you narrowly avoid dropping it on your foot as you put it back down."
  }



  report(dropping, drawer) Say "You drop your drawer."
  report(taking, drawer) Say Randomly("You take the drawer and balance it on your head. Maybe you can carry stuff like that?", "You took the drawer out of the desk and slid in into your pants. Now you can store things in a drawer in your pants.")

}


object Vegetable {

  before(taking, of[Vegetable] ) {
    val v = noun[Vegetable]
    if (v.ripe) {
      Say("Up you go!")
    }
    else {
      Say("We do not harvest the young")
      stop
    }
  }

  before(examining, of[Vegetable]) {
    val v = noun[Vegetable]
    if (v.wilted) {
      Say(s"A slow tear runs down your face, but even a river of tears could not bring $noun back to life")
    }
    if (!v.wilted) {
      Say(s"A slow tear of happiness runs down your cheek, adding one extra drop of moisture to your healthy $noun")
    }
    if (v.ripe) {
      Say("Yum yum!")
    }

    if(v.watered) {Say("Nice and moist, just like they like it")}
    else {Say("They thirst.")}

  }
}

class Vegetable(using c : Container) extends Thing {
  var wilted = false
  var watered = false
  var ripe = false
}


object Animal{
  before(taking, of[Animal]){
    Say (s"$noun eye you expectantly.")
  }
}

class Animal(using c : Container) extends Thing {
  var petted = false

}


object Pieces {

}

class Pieces(var quantity : Int)(using c : Container) extends Thing {

  amount(some)


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


class Lucre(amount : Int)(using c : Container) extends Pieces(amount) {

  name = "Lucre"
  description = "Coarse golden splinters. They sting the hand. It's almost as if the baleful shards despise possession."
  aliases.addOne("splinters")

}


object Porch extends Room {

  Connect(north, FarmHouse)

  name = "Porch"
  description = s"Ah, yes, the great outdoors. $farm lies before you. You feel the wood planks beneath your feet. Your chicken coop lies to the west. There's a pond btw"

  val crops = ~"You have lovely little fwends growing in neat stupid fucking rows divided by pointless cobblestones." aka "plants" amount some

  val parsnip= ~"A single perfect parsnip, ripe and ready just for you"

  val porchLucre = Lucre(10)


  object maw extends Thing with Container {

    description = s"this hungry thing is the opposite of a treasure chest. Put things in and wait for it to process your refuse into Lucre. How does it work? Maru is conducting an inquiry into its mysteries."
    name = "The Maw"
    aliases.addOne("maw")
    maw.proper = true
    maw.transparent = false
    open = false

    var lucreWaiting = 0

    val pouch = ~s"this ${ if lucreWaiting > 0 then "engorged" else "flaccid"} sac contains the fruit of your labors" composes this aka "sac"


    instead(putting, of[Thing], this.asSecondNoun) {
      if(noun[Thing].valueInLucre <= 0){
        Say(s"The Maw rejects the ${Randomly("important", "worthless")} scent of $noun")
        stop
      } else continue
    }

    report(putting, of[Thing], this.asSecondNoun){
      Say(s"The Maw greedily gobbles $noun")
    }

    after(examining, this){
      if(lucreWaiting > 0){
        if(lucreWaiting <= 100){
          Say(s"A pittance of $lucreWaiting Lucre jingles for you from within a protective pouch.")
        }
        else if (lucreWaiting <= 10000) {
          Say(s"A pile of $lucreWaiting Lucre bulges from within a protective pouch.")
        }
        else {
          Say(s"A heap of $lucreWaiting Lucre eagerly spills to the ground in sloppy gobs.")
        }
      }
    }


  /*
    instead(taking, lucre){

      if(lucreWaiting > 0) {
        Say("You roughly pile the Lucre into your coffer")
        lucreWaiting = 0
        player.lucreHeld += lucreWaiting
      } else {
        Say("Alas, The Maw is spent, but not for lack of trying.")
      }

    }
    */



    after(World.EndingDay){
      if(currentLocation == Porch || currentLocation == FarmHouse) {
        if(this.contents.nonEmpty){
          Say("Your rest is punctuated by dutiful munching as The Maw transmutes its burden to Lucre")
        }
      }

      for (c <- contents) {
        lucreWaiting += c.asInstanceOf[Thing].valueInLucre
        c.parentContainer = null
      }
      contents.clear()

      this.open = false
    }




  }

  report(taking, parsnip) {
    Say("Pop! You gently but firmly yank the parsnip, extricating it from its bed.")
  }

  val parsnips = new Vegetable named "baby parsnips" desc "These parsnips are young and unprepared to leave their homes." amount plural aka "parsnips"

  val seedlings = new Vegetable named "seedlings" desc "There guys look dry and sad" aka "babies" aka "seedling" // ripe= false watered = false

  instead(taking, crops :: parsnips :: seedlings :: Nil) Say s"You have failed. $noun remains where it is."

  object watering_can extends Thing {
    var waterAmount = 0

    after(examining, watering_can) {
      if (watering_can.waterAmount == 0) Say(Randomly("It's bone dry", "it's spent", "A miniature tumbleweed " +
        "flops across the basin of the watering can"))
      if (watering_can.waterAmount == 1) Say("a paucity of water glints in the pail")
      if (watering_can.waterAmount == 2) Say("a hollow hymn rings as the can sloshes weakly")
      if (watering_can.waterAmount == 3) Say("a mysterious liquid obscures the bottom")
      if (watering_can.waterAmount == 4) Say("the can brims with life giving manna")
      if (watering_can.waterAmount == 5) Say("the weight of potential is literally quite substantial, your frail arms have atrophied from over-reliance on iridium irrigation technology")
    }

    this named "watering can" aka "can" aka "water can" aka "pail" aka "bucket" desc "you never felt like upgrading your copper watering can. it is jealous of the other tools."
  }


  object filling extends Action(1, "fill", "submerge", "refill", "dunk"){

    inflict(filling, watering_can) {
      Say("You submerge the watering can in the pond, filling it with potential")
      watering_can.waterAmount = 5
    }
  }


  object emptying extends Action(1, "empty", "dump", "drain", "spill")

  instead(emptying, player lacks watering_can) Say Randomly("You practice a watering can emptying motion so you don't get out of practice.", "You may not have a can, but you can still have emptiness")

  inflict(emptying, watering_can) {
    if (watering_can.waterAmount > 0) {
      watering_can.waterAmount = 0
      Say(Randomly("It's raining!_! T_T", "You have an empty can now. Now you can fill it again.", "The water spills over the soil, forever lost to you like the innocence of youth"))
    }
    else {
      Say(Randomly("You tilt the watering can over the ground, and you could swear some vapor escaped maybe", "Nothing. there Was nothing and there IS nothing."))
      stop
    }
   }

    object watering extends Action(1, "water", "spray", "hydrate", "douse", "irrigate"){
    instead(watering, player lacks watering_can) Say Randomly("You try but your tank is empty.", "Stage fright strikes again!", "Performance anxiety overcomes you when you look at the person on the other side of the screen")

    inflict(watering, of[Vegetable]) {
      val v = noun[Vegetable]
      if watering_can.waterAmount > 0 then
        v.watered = true
        watering_can.waterAmount = watering_can.waterAmount - 1
      else
        Say(Randomly(s"You tilt the watering can expectantly over $noun, but the dry vessel provides no succor", s"Your watering can is empty. You drool on the $noun a little just in case it helps", s"You have no water, but you hope the sweat from your brow provided moisture to $noun."))
        stop
    }

    report(watering) {
      Say(s"You spritz $noun, and it somehow seems happier")
    }
  }



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


  val mayo_machine = new Box {
   //   val void_mayo = ~"void mayo."
    name = "mayo machine"
    description = "a mayo machine beyond your wildest dreams"
  //  Understand(void_mayo) {
  //    void_mayo.parentContainer == this
  //  }
  } aka "mm" aka "machine" is fixed
  val void_mayo = ~"Fresh void mayo. It contains universes within, in a convenient colloidal suspension. It's black, with red glistening spots. It's beautiful." aka "mayo" aka "mayonnaise"
  object dix extends Thing{
    name = "dix"
    aliases.addOne("dixxx")//aka "dixxx"
    description = "just dix"
    properties.addOne(scenery).addOne(fixed)
  }
  //  Understand(void_mayo) { void_mayo.parentContainer == mayo_machine }
  void_mayo transferTo mayo_machine //omg it took me way too long to notice that this is how you do it

  before(taking, void_mayo) {
    if (void_mayo.parentContainer == player) {
      Say("You fondle the void mayo in your pocket")
      false  //what does this even do in before??
    }
    else if (void_mayo.parentContainer == mayo_machine) {
      Say(Randomly("You reverently take the fresh void mayo out of the machine. It contains universes within, in a convenient colloidal suspension", "You lovingly extricate the void mayo from its berth."))
      true
    }
    else {
      Say(Randomly("You hastily gather up the void mayo in a plastic baggy and stuff it in your pocket. Almost no worse for wear", "You put that mayo back where it belongs. With you, always."))
      true
    }
    Say("You love void mayo like your own child")
  }


  instead(examining, mayo_machine) {
    if (mayo_machine.open == false) {
      Say("You look hopefully at the mayo machine. You left some void egg in there last night.")
      true
    }
    else {
      if (void_mayo.parentContainer == mayo_machine) {
        Say("There it is. At last, the final piece of the puzzle, the icing on the cake, the cap on the marker, the bonnet on the bee, the kangaroo in the pouch. You've been waiting for so long to be able to present this mayo to the junimos ")
        true
      }
      else {
        Say("Emptiness. Like the wrong kind of void.")
        true
      }
    }
    if (false) {
      Say(mayo_machine.description)
    }
  }
  val chickens = new Animal named "chickens" desc "There are some cute lil chickens waiting for your love." aka "chicken" aka "chicks" aka "fluffballs" aka "cuties" amount some

  report(taking, chickens) {
    Say("You scoop up every chicken and shove them in your trousers. They purr contentedly, sending vibrations through your body")
  }

  object petting extends Action(1,"pet", "hug", "pat", "love"){
    inflict(petting, chickens) {
      if (chickens.petted)
        Say("You pet the chickens again, extra hard. They make little contented clucks but don't love you any harder.")
      else
        Say("You pet each and every chicken. They let out little <3's and love you even more now.")

      chickens.petted = true
    }
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

  instead(smelling, this) {
    Say("The pleasant aroma of feathers and mayonnaise intertwine here")
  }

  before(going, east, this) {
    if (void_mayo.parentContainer == player) {
      Say("With your prize in tow, you depart")
    }
    //elseif you dropped the mayo elsewhere, you totally fucked lol
    else {
      Say("But, but, the mayo, the chickens!")
      stop
    }
  }

  instead(going, north, this) {
    Say("You lift up one leg. The chickens look at you reproachfully. You change your mind about getting into the nest.")
  }


  instead(going, west, here) {
    val dix1 =
      if (void_mayo.parentContainer == player) {
        Say("you got that sweet mayo, but there's no exit there anyways.")
        true
      }
      else {
        Say("you really want that mayo")
        false
      }
    if (dix1) Say("The true dix are the friends you make along the way")
    else if (!chickens.petted) Say("you really should pet the chickens, idiot")

  }

}








