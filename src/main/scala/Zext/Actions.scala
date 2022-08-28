package Zext

import Zext.Interpreter.*
import Zext.Parser.*
import Zext.Rule.*
import Zext.World.*
import Zext.ZextObject.*

import scala.collection.mutable
import scala.reflect.TypeTest
import Condition.*
import Game.JunimoGame.encumbrance
import Zext.player.playerName

object Actions {

  val commandAliases = mutable.HashMap[String, Command]()


  def UnderstandAlias(str: String, action: Action, zextObject1: ZextObject = null, zextObject2: ZextObject = null): Unit = {
    commandAliases.addOne(str -> Command(action, Option(zextObject1), Option(zextObject2)))
  }


  def UnderstandAlias(strs: Seq[String], action: Action, zextObject1: ZextObject, zextObject2: ZextObject) : Unit = {
    commandAliases.addAll(strs.map( _ -> Command(action, Option(zextObject1), Option(zextObject2))))
  }


  def Randomly(one_in: Int): Boolean = util.Random.nextInt(one_in) == 0

  def Randomly(strs: StringExpression*): StringExpression = {
    s"${
      val which = util.Random.nextInt(strs.length)
      strs(which)
    }"
  }


  object being extends Action(1)

  object entering extends Action(1) {

    inflict(entering, of[Room]) {
      player.Move(noun[Room])
      LineBreak()
      execute(examining, currentLocation)
      true
    }

    after(entering, of[Room]){
      noun[Room].visited = true
      true
    }

  }

  object going extends Action(1,"go", "travel", "walk", "run", "cartwheel") {

    instead(going, reflexively) Say "You don't have to go right now."

    UnderstandAlias("east", going, Direction.east)
    UnderstandAlias("e", going, Direction.east)
    UnderstandAlias("west", going, Direction.west)
    UnderstandAlias("w", going, Direction.west)
    UnderstandAlias("north", going, Direction.north)
    UnderstandAlias("n", going, Direction.north)
    UnderstandAlias("south", going, Direction.south)
    UnderstandAlias("s", going, Direction.south)



    /// inflict[Direction](going) { d => goingDir = d; execute(going, currentLocation.connections.getOrElse(d, nowhere)) }

    // you can still circumvent going direction based movements by going directly to a location... eh i'll fix it later
    // desired behavior:
    // override the "i went x " text, which should be the act of reporting going
    // once we have moved to the other room, and reported, we want to automatically examine the room.

    inflict(going, of[Direction]) {
      val d = noun[Direction]
      val connected = currentLocation.connections.contains(d)

      if(!connected){
        Say(s"You can't go $d")
      }

      connected
    }

    report(going, of[Direction]) {
      val d = noun[Direction]
      val room = currentLocation.connections(d)
      Say(s"I went $d to $room.")
    }

    after(going, of[Direction]) {
      val d = noun[Direction]
      val room = currentLocation.connections(d)
      execute(entering, room)


    }
  }

  object dropping extends Action(1,"drop", "abandon") {

    report(dropping, reflexively) {
      Say(Randomly("You stop, drop, and roll.", "You fall to your knees for no reason.",
        """You throw yourself to the floor.
          |Like a cat licking itself nonchalantly after doing something embarrassing, you pretend you dropped a contact.""".stripMargin))
    }


    inflict(dropping) {
      if (noun.parentContainer == player) {
        noun.transferTo(currentLocation)
        true
      }
      else {
        Say("Can't drop what you don't have.")
        false
      }
    }

    report(dropping) Say Randomly(s"$noun gently flutters to the ground.", s"Discarded, $noun crashes into earth.", s"You abandon $noun to its fate.")

  }


  case class flavor(desc: StringExpression) extends Property

  object tasting extends Action( 1,"eat", "taste", "lick", "nom", "mouth", "nibble") {

    player.properties += flavor("the inside of your mouth")

    inflict(tasting, reflexively) {
      Say("Slurp!")
      execute(tasting, player)
    }

    report(tasting, prop[flavor]) {
      Say(s"$noun tastes like ${noun[flavor].desc}")
    }

    report(tasting) {
      Say(s"$noun is strangely flavorless")
    }

  }

  object taking extends Action(1,"take", "get", "pick up", "g") {


    before(taking){
      if !noun.isAccessible(currentLocation) then
        Say(s"$noun $is inaccessible") // maybe say why?
        false
      else
        true
    }

    instead(taking, reflexively) Say s"I can't take nothing."

    instead(taking, player has noun) {
      Say(s"I shuffle around the items in my pockets, looking for $noun")
    }

    instead(taking, fixed) {
      Say(s"$noun $is shoracle")
    }

    report(taking) {
      Say(s"You slip $noun into your backpack.")
    }

    after(taking){
      Say(s"$encumbrance slots left and then you DIE.")
      true
    }

    inflict(taking) {
      noun.transferTo(player)
      true
    }
  }

  object examining extends Action(1,"examine", "x", "look", "l") {

    instead(examining, reflexively) {
      execute(examining, currentLocation)
    }

    inflict(examining, of[Crevice]) {
      Title(currentLocation.name + " It's small! ")
      Say(currentLocation.description)
      true
    }


    inflict(examining) {
      Say({noun.description})
      true
    }


    inflict(examining, of[Room]) {
      Title(currentLocation.name)
      Say(currentLocation.description)
      true
    }

    after(examining, of[Room]) {
      val r = noun[Room]
      val visible = r.contents.filterNot(_ ? scenery)
      if (!visible.isEmpty) {
        var s = "You can see "
        for (i <- visible) {
          if (!i.?(scenery))
            s += i.indefinite + ", "
        }
        s = s.stripSuffix(", ")
        s += "."
        Say(s)
      }
      false
    }

    after(examining, of[Container]) {

      val c = noun[Container]
      var response = ""
      if c.open then response +=s"$noun is open" else response += s"$noun is closed"

      if (c.open  || c.transparent) {
        response += ", "
        if(c.contents.isEmpty)
          response += "nothing is inside"
        else
          response += s"inside you can see ${c.ContentsString}"
      }

      Say(response)
      true
    }
  }


  object opening extends Action(1, "open"){

    inflict(opening, of[Container]){
      if !noun[Container].open then
        noun[Container].open = true
        true
      else
        Say(s"$noun is already open")
        false
    }

    report(opening, of[Container]) {
      if !noun[Container].transparent then
        Say(s"You open $noun, inside you can see ${noun[Container].ContentsString}")
      else
        Say(s"You open $noun")
    }
  }

  object exiting extends Action(0,"exit") {
    inflict(exiting) {
      Say(s"Goodbye $playerName")
      exit = true
      true
    }
  }

  object takingInventory extends Action(0,"inventory", "i") {
    inflict(takingInventory) {
      var s = "In your possessionary, you have "
      for (i <- player.contents) {
        s += i.indefinite + ", "
      }
      s = s.stripSuffix(", ")
      s += "."
      Say(s)
      true
    }
  }

  object putting extends Action(2,"put", "insert", "place", "stuff", "thrust", "jam", "shove", "smash") {

    type Zontainer = Container & ZextObject


    inflict(putting, ofSecond[Zontainer]) {
      if (noun.parentContainer == player && secondNoun.isAccessible(currentLocation)) {
        noun transferTo secondNoun[Container]
        true
      } else {
        Say(s"$secondNoun is inaccessible")
        false
      }
    }

    report(putting, ofSecond[Zontainer]) Say s"You put $noun into $secondNoun"

  }
}

