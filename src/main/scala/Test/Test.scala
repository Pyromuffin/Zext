package Test

import Zext.*
import Zext.exports.*
import Zext.Actions.*

val player = new Player:
  override val name = "SLEEMO"
  override val description = "Your bipes bip fast."



object Dirt extends Room with StartingRoom {

  override val name: String = "dirt"
  override val description: StringExpression = "You are buried in soil."

  val pebble = ~"the size of a small boulder"
  val mud = ~"dirt juice" amount some
  val walls = ~"they're everywhere" are fixed

  val bucket = box("A rough pail") {
    val corn = ~"cobbic" amount some
    val sand = ~"paperless sandpaper" amount some
  }
  
  val hook = supporter("hungry tines") 

  object not_yours extends Property
  val scarves = ~"An array of zebra patterned tactical scarves" is scenery is not_yours
  val mantles = ~"How did they get all these fireplaces in here???" is scenery is not_yours
  val sashes = ~"Second place winner in the number of sashes competition" is scenery is not_yours


  var time = 0

  inflict(being) {
    time = time + 1
  }

  //report(being) Say Cycle("ouch", "my bones", s"the current time is $time")


  report(going, south, here) Say "You tunnel to the south."
  report(going, north, here) Say "You mosey to the north."

  report(leaving, here) Say "The tunnel collapses behind you."
  inflict(leaving, here) {
    currentLocation.Disconnect(south)
    currentLocation.Disconnect(north)
  }





}

object WormPile extends Room {

  override val name: String = "Worm Pile"
  override val description: StringExpression = "I'm not sure what you expected."

  Connect(south, Dirt)
}


object FairyFountain extends Room {

  override val name: String = "Fairy Fountain"
  override val description: StringExpression = "Piped-in harp music indicates the presence of a creature with great power."
  val fairy_armadillo = ~"Nigiri with feet"

  instead(leaving, here) Say "The big guy wants your attention"

  
  Connect(north, Dirt)
}


object Test extends App{

  Zext.Parser.StartInterpreter(player, "Test")

}
