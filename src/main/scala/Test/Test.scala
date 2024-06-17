package Test

import Zext.*
import Zext.exports.*

val player = new Player:
  override val name = "SLEEMO"
  override val description = "Your bipes bip fast."


def thing(name : String)(using c : Container) = {
  c
}



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




}

object Test extends App{

  println("Hello world.")


  Zext.Parser.StartInterpreter(player, "Test")



}
