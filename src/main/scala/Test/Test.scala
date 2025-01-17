package Test

import Zext.*
import Zext.exports.*
import Zext.Actions.*

val guy = new PlayerClass:
  override val name = "SLEEMO"
  override val description = "Your bipes bip fast."
  val stick = ~"sticky"

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

object Dirt extends Room with StartingRoom {

  override val name: StringExpression = "dirt"
  override val description: StringExpression = once("the floorboards creak underfoot.") + "You are buried in soil."


  val lockbox = simpleBox("A transparent lockbox with a small round keyhole") {
    val treasure = ~"lucre"
  }
  lockbox.openable = false
  lockbox.transparent = true

  val pebble = ~"the size of a small boulder"
  val mud = ~"dirt juice" amount some
  val walls = ~"they're everywhere" are fixed

  val bucket = simpleBox("pebble purgatory") {
    val sand = ~"paperless sandpaper" amount some
    val pants = ~"rag ensemble" and RoomDescription("A pair of pants is tangled with bucket particles") amount some // add custom amountifiers like a pair
  }

  //val zebra = z"Striped, suited to camouflage $time times in this environment"

  val chemicals = ~"10 mol\\. guydrofluouric \\?type\\? \\\\acid\\\\"


  report(opening, bucket) Say "Your pry open the bucket lid"
  // instead(opening, bucket) Say "It's sealed with bucket glue"

  val hook = simpleSupporter("hungry tines")

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
    Disconnect(south)
    Disconnect(north)
  }


  val clothes_rack = new Supporter("Clothes Rack") {


    this is fixed
    this aka "rack"

    automaticallyListContents = false


    val shirt = ~"Solidified dye in the shape of a tank top"

    override val description = str {
      if (contents.contains(shirt))
        "A shirt is pinned to a line strewn between two very serious pots."
      else
        "The line hangs limply"
    }



    report(taking, shirt, this has shirt) {
      Say("Trying not to disturb the pots, you carefully unclip the shirt from the line")
    }
  }




}


object hanging extends Action(2, "hang") {
  implicitSubjectSelector = _ == player
}


object CrowsNest extends Room with StartingRoom {
  override val name: StringExpression = "The Crow's Nest"
  override val description: StringExpression = "A circular platform at the top of the ladder from which you can reach the trapeze"

  val trapeze = ~"It looks like a barber pole, only it's orange and purple" is fixed


  instead(hanging, anything -> trapeze) Say "You are not insured for that"

}


object WormPile extends Room {

  override val name: StringExpression = "Worm Pile"
  override val description: StringExpression = "I'm not sure what you expected."

  Connect(south, Dirt)
}


object FairyFountain extends Room {

  override val name: StringExpression = "Fairy Fountain"
  override val description: StringExpression = "Piped-in harp music indicates the presence of a creature with immense power."
  val fairy_armadillo = ~"Nigiri with feet"


  instead(leaving, here) Say "The big guy wants your attention here"


  Connect(north, Dirt)
}


object Test extends App{

  Zext.Parser.StartInterpreter(guy, "Test")

}
