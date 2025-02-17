package Test

import Zext.*
import Zext.exports.*
import Zext.Actions.*

import scala.language.postfixOps


object guy extends PlayerClass(BigTop) {
  override val name = "SLEEMO"
  override val description = "Your bipes bip fast."
  var insured = false
  val stick = ~"sticky"

}


extension(z : ZextObjectProxy[PlayerClass]) {
  implicit def toGuy : guy.type = z.resolve.asInstanceOf[guy.type]
}


object count {

  var calls = 0
}


object unlocking extends Action(1, "unlock")
{
  inflict(unlocking, of[Container]) {
    noun[Container].openable = true
  }
  report(unlocking) Say s"You unlock $noun"
}

object Dirt extends Room {

  override val name: StringExpression = "dirt"
  override val description: StringExpression = str {
    once("the floorboards creak underfoot.")
   "You are buried in soil."
  }


  val lockbox = new Box("A transparent lockbox with a small round keyhole") {
    val treasure = ~"lucre"
  }
  lockbox.openable = false
  lockbox.transparent = true

  val pebble = ~"the size of a small boulder"
  val mud = ~"dirt juice" amount some
  val walls = ~"they're everywhere" are fixed

  val bucket = new Box("pebble purgatory") {
    val sand = ~"paperless sandpaper" amount some
    val pants = ~"rag ensemble" and RoomDescription("A pair of pants is tangled with bucket particles") amount some // add custom amountifiers like a pair
  }

  //val zebra = z"Striped, suited to camouflage $time times in this environment"

  val chemicals = ~"10 mol\\. guydrofluouric \\?type\\? \\\\acid\\\\"


  report(opening, bucket) Say "Your pry open the bucket lid"
  // instead(opening, bucket) Say "It's sealed with bucket glue"

  val hook = Supporter("hungry tines")

  object not_yours extends Property
  val scarves = ~"An array of zebra patterned tactical scarves" is scenery and not_yours
  val mantles = ~"How did they get all these fireplaces in here???" is scenery and not_yours
  val sashes = ~"Second place winner in the number of sashes competition" is scenery and not_yours


  instead(taking, not_yours) Say "that would be uncouth"

  val crumble_block = ~"It disintegrated." and RoomDescription("A fragile crumble block teeters on the brink of existence") aka "block"

  var time = 0

  inflict(being) {
    if(first) Say("Time is ticking")
    time = time + 1
  }

  report(being) Add s"The time is $time"


  report(going, south, here) Say "You tunnel to the south."
  report(going, north, here) Say "You mosey to the north."

  report(leaving, here) Say "The tunnel collapses behind you."
  inflict(leaving, here) {
    //Disconnect(south)
    //Disconnect(north)
  }




}


object hanging extends Action(2, "hang") {
  override val implicitSubjectSelector = player

  report(hanging) Say s"$noun hangs from $secondNoun"

}


object crows_above extends Backdrop {
  val crows = ~"Several crows are circling above you."
}

object Circus extends RoomRegion("Circus Region") {

  this designates (BigTop, CrowsNest)

  val circusStuff = new Backdrop {
    val circus = ~"The Sleemo Brothers Ring-a-Ding Circus Extravaganza"
    val tent = ~"A spiral candystripe towering overhead" aka "roof"

    this backdrops Circus
  }

}

case class wet(var wetness: Int) extends Property

object drying extends Action(1, "dry") {

  applying(drying, of[wet]) {
    if (noun[wet].wetness > 0 && scala.util.Random.nextInt(4) == 0){
      continue
    } else stop
  }

  inflict(drying, of[wet]) {
    noun[wet].wetness -= 1
  }

  after(drying, of[wet]) {
    val wetness = noun[wet].wetness
    if (wetness != 0)
      Say(s"$noun dries out a little. It looks like it has $wetness drops of water left.")
    else if (wetness == 0)
      Say(s"$noun is completely dry")
  }

}


object dryingWith extends Action(2, "dry") {

  report(dryingWith) {
    Say(s"You dry $noun with $secondNoun")
  }

}



object BigTop extends Room {

  override val name: StringExpression = "Big Top"
  override val description: StringExpression = "You are in a giant stadium, covered by a bright tent. It seems to be sagging in the middle"

  val sagging = ~"Upon further inspection you see the sagging is caused by a small trapeze." is scenery is fixed aka "sag"

  val ladder = ~"It leads to the crow's nest" is fixed

  val red_hat  = ~"a red hat"
  val blue_hat  = ~"a blue hat"
  val hat  = ~"a hat"
  val hat_hat  = ~"a hat hat"



  val bucket = new Box {
    this is wet(3)

    override val name = str {
      if (this[wet].wetness > 0) {
        "bucket of water"
      } else {
        "dry bucket"
      }
    }


    override val description = str {
      if (this[wet].wetness > 0) {
        "a bucket with an amount of quick-dry water"
      } else {
        "bone dry"
      }
    }
  }


  bucket holds hat


  val clothes_rack = new Supporter {


    this is fixed
    //this aka "rack"

    automaticallyListContents = false


    val shirt = ~"Solidified dye in the shape of a tank top"

    override val description = str {
      if (contents.contains(shirt))
        "A shirt is pinned to a line strewn between two very serious pots."
      else
        "The line hangs limply"
    }


    report(taking, shirt, this holds shirt) {
      Say("Trying not to disturb the pots, you carefully unclip the shirt from the line")
    }
  }

  this southward WormPile
  this upward CrowsNest

}






object CrowsNest extends Room {
  override val name: StringExpression = "Crow's Nest"
  override val description: StringExpression = "A circular platform at the top of the ladder from which you can reach the trapeze"


  val trapeze =  "The trapeze hangs limply from a bit of scaffolding" initially
    "It looks like a barber pole, only it's orange and purple" is fixed

  report(examining, trapeze, !trapeze.isAccessibleTo(player)) Say "A handlebar that seems to be hanging from something in the sky"

  instead(hanging, player -> trapeze, !player.insured) Say "You are not insured for that"

  crows_above backdrops this

  after(dropping, player.stick) {
    player.insured = true
  }

  inflict(determiningVisibility, trapeze, Circus.here){
    replace
  }


}




object WormPile extends Room {

  override val name: StringExpression = "Worm Pile"
  override val description: StringExpression = "I'm not sure what you expected."

  val pile_of_worms = ~"writhing all around you."

  this southward Dirt

}


object FairyFountain extends Room {

  override val name: StringExpression = "Fairy Fountain"
  override val description: StringExpression = "Piped-in harp music indicates the presence of a creature with immense power."
  val fairy_armadillo = ~"Nigiri with feet"


  instead(leaving, here) Say "The big guy wants your attention here"

  this northward Dirt
}


object Test extends App{

  Zext.Parser.StartInterpreter(guy, "Test")

}
