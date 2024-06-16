package Test

import Zext.*

val player = new Player:
  override val name = "SLEEMO"
  override val description = "Your bipes bip fast."

object Dirt extends Room with StartingRoom {

  override val name: String = "dirt"
  override val description: StringExpression = "You are buried in soil."
}

object Test extends App{

  println("Hello world.")
  Zext.Parser.StartInterpreter(player, "Test")


}
