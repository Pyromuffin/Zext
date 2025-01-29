package Tests

import Zext.*
import Zext.exports.*


object TestPlayer extends PlayerClass {
  override val name = "Test Player"
  override val description = "Detestable"
}

object TestRoom extends Room with StartingRoom {

  override val name = "Test Room"
  override val description = ""

  val pebble = ~"macrosand"
  val box = Box("Test box")


  
  
  val table = new Supporter("Test table") {
    val stick =  ~"sticky"
  }
  val gum = "A wad of gum is stuck to the bottom of the table" initially
    "A dried out piece of chewing gum" inside table


}


object Tests extends App {

  scala.util.Random.setSeed(123456789)

  Parser.InitTests(TestPlayer, "Tests")

  var failureCount = 0

  if !Parser.RunTest("Invalid input", Array("Hello"), Array("[REDACTED].")) then failureCount += 1
  if !Parser.RunTest("Examine self",  Array("x self"), Array("Detestable.")) then failureCount += 1
  if !Parser.RunTest("box opening",  Array("open box"), Array("You open the box.")) then failureCount += 1
  if !Parser.RunTest("table opening",  Array("open table"), Array("The table can't be opened.")) then failureCount += 1
  if !Parser.RunTest("box closing",  Array("close box"), Array("You close the box.")) then failureCount += 1
  if !Parser.RunTest("table closing",  Array("close table"), Array("The table can't be closed.")) then failureCount += 1
  if !Parser.RunTest("pebble box putting",  Array("put pebble in box"), Array("(First taking the pebble).", "(First opening the box).", "You put the pebble into the box.")) then failureCount += 1
  if !Parser.RunTest("pebble table putting",  Array("put pebble on the table"), Array("(First taking the pebble).", "You put the pebble on to the table.")) then failureCount += 1
  if !Parser.RunTest("already contained putting",  Array("put pebble on the table"), Array("The pebble is already on the table.")) then failureCount += 1
  if !Parser.RunTest("table contents listing",  Array("x table"), Array("Test table.", "A wad of gum is stuck to the bottom of the table.", "On the table you can also see a stick and a pebble.")) then failureCount += 1
  if !Parser.RunTest("stick taking",  Array("take stick"), Array("You take the stick off of the table.")) then failureCount += 1
  if !Parser.RunTest("inventory listing",  Array("i"), Array("In your possessionary you have a stick.")) then failureCount += 1
  if !Parser.RunTest("stick dropping",  Array("drop stick"), Array("Discarded, the stick crashes into earth.")) then failureCount += 1
  if !Parser.RunTest("stick dropping failure",  Array("drop stick"), Array("Can't drop what you don't have.")) then failureCount += 1
  if !Parser.RunTest("empty inventory",  Array("i"), Array("In your possessionary you have nothing.")) then failureCount += 1

  println("=================================")
  if(failureCount == 0){
    println("All tests succeeded")  
  } else {
    Console.err.println(s"Failure Count: $failureCount")
  }
  
  
  


}