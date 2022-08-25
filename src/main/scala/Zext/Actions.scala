package Zext

import Zext.Interpreter.*
import Zext.Parser.*
import Zext.Rule.*
import Zext.World.*
import Zext.ZextObject.*

import scala.collection.mutable
import scala.reflect.TypeTest

object Actions {

  val commandAliases = mutable.HashMap[String, Command]()

  def UnderstandAlias(str: String, action: Action, zextObject1: ZextObject = null, zextObject2: ZextObject = null): Unit = {
    commandAliases.addOne(str -> Command(action, Option(zextObject1), Option(zextObject2)))
  }


  def Randomly(one_in: Int): Boolean = util.Random.nextInt(one_in) == 0

  def Randomly(strs: StringExpression*): StringExpression = {
    val which = util.Random.nextInt(strs.length)
    strs(which)
  }


  object going extends Action("go", "travel", "walk", "run", "cartwheel") {

    UnderstandAlias("east", going, Direction.east)
    UnderstandAlias("e", going, Direction.east)
    UnderstandAlias("west", going, Direction.west)
    UnderstandAlias("w", going, Direction.west)
    UnderstandAlias("north", going, Direction.north)
    UnderstandAlias("n", going, Direction.north)
    UnderstandAlias("south", going, Direction.south)
    UnderstandAlias("s", going, Direction.south)

    val nowhere = Room();
    nowhere.global = true;
    nowhere.name = "nowhere";
    nowhere.proper = true

    /// inflict[Direction](going) { d => goingDir = d; execute(going, currentLocation.connections.getOrElse(d, nowhere)) }

    // you can still circumvent going direction based movements by going directly to a location... eh i'll fix it later
    // desired behavior:
    // override the "i went x " text, which should be the act of reporting going
    // once we have moved to the other room, and reported, we want to automatically examine the room.

    inflict[Direction](going) { d =>
      val connected = currentLocation.connections.contains(d)

      if(!connected){
        Say(s"You can't go $d")
      }

      connected
    }

    report[Direction](going) { d =>
      val room = currentLocation.connections(d)
      Say(s"I went $d to $room.")
    }

    after[Direction](going) { d =>
      val room = currentLocation.connections(d)
      currentLocation = room
      LineBreak()
      execute(examining, currentLocation)
      currentLocation.OnEnter()
    }
  }

  object dropping extends Action("drop", "abandon") {

    report(dropping) {
      Say(Randomly("You stop, drop, and roll.", "You fall to your knees for no reason.",
        """You throw yourself to the floor.
          |Like a cat licking itself nonchalantly after doing something embarrassing, you pretend you dropped a contact.""".stripMargin))
    }

    inflict[ZextObject](dropping) { z => // a/n z is noun  (this lamda has to take ZextObject argument)
      if (z.parentContainer == inventory) {
        z.transferTo(currentLocation)
        true
      }
      else {
        Say("Can't drop what you don't have.")
        false
      }
    }

    report[ZextObject](dropping) { z =>
      Say(Randomly(s"$noun gently flutters to the ground.", s"Discarded, $noun crashes into earth.", s"You abandon $noun to its fate."))
    }
  }

  object smelling extends Action("smell", "inhale", "snort", "vape", "endocytose", "flehm", "flehmen", "sniff", "nasalize") {

    inflict(smelling) {
      Say(Randomly("You wrinkle your nose and lift your lips, giving' that vestigial Vomeronasal Organ another go", "Ah, the smells.", "Moved here for the grandpa, stayed for the smells."))
      true
    }

    after[ZextObject](smelling, inventory has noun) { z =>
      Say(s"The scent of $noun is soaked through your clothing now.")
    }


    instead(smelling, fixed) {
      Say(s"$noun: Scentless and fixed.")
      false
    }

    report[ZextObject](smelling) { n =>
      Say(s"You smell $n. Wow.")
    }

    inflict[ZextObject](smelling) { n =>

      true
    }
  }


  object tasting extends Action("eat", "taste", "lick", "nom", "mouth", "nibble") {

    inflict(tasting) {
      execute(tasting, currentLocation)
    }


    inflict[ZextObject](tasting) { n =>
      val immediate = s"$n: ${n.description}"
      Say(immediate)
      true
    }
  } //taste test


  object taking extends Action("take", "get", "pick up", "g") {

    inflict(taking) {
      Say(s"I can't take nothing.")
      false
    }

    instead[ZextObject](taking, inventory has noun) { z =>
      Say(s"I shuffle around the items in my pockets, looking for $noun")
    }

    instead(taking, fixed) {
      Say(s"$noun $is shoracle")
      false
    }

    report[ZextObject](taking) { n =>
      Say(s"You took $n. Wow.")
    }

    inflict[ZextObject](taking) { n =>
      noun.transferTo(inventory)
      true
    }
  }

  object examining extends Action("examine", "x", "look", "l") {

    inflict(examining) {
      execute(examining, currentLocation)
    }

    inflict[Room](examining) { r =>
      Title(currentLocation.name)
      Say(currentLocation.description)
      true
    }


    inflict[Crevice](examining) { r =>
      Title(currentLocation.name + " It's small! ")
      Say(currentLocation.description)
      true
    }


    inflict[ZextObject](examining) { n =>
      val immediate = s"$n: ${n.description}"
      Say(immediate)
      true
    }
  }

  object examining1 extends Action("examine1", "look1") {

    inflict(examining1) {
      execute(examining1, currentLocation)
    }

    inflict[Room](examining1) { r =>
      Title(currentLocation.name)
      Say(currentLocation.description)
      true
    }


    inflict[Crevice](examining1) { r =>
      Title(currentLocation.name + " It's small! ")
      Say(currentLocation.description)
      true
    }


    inflict[ZextObject](examining1) { n =>
      val immediate = s"$n: ${n.description}"
      Say(immediate)
      true
    }
  }

  report[Room](examining) { r =>
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
  }


  object exiting extends Action("exit") {
    inflict(exiting) {
      Say(s"Goodbye $playerName")
      exit = true
      true
    }
  }

  object takingInventory extends Action("inventory", "i") {
    inflict(takingInventory) {
      var s = "In your possessionary, you have "
      for (i <- inventory.contents) {
        s += i.indefinite + ", "
      }
      s = s.stripSuffix(", ")
      s += "."
      Say(s)
      true
    }
  }

  object putting extends Action("put", "insert") {

    type Zontainer = Container & ZextObject

    inflict[ZextObject, Zontainer](putting) { (z1, z2) =>

      if (z1.parentContainer == inventory && z2.isAccessible(currentLocation)) {
        z1 transferTo z2
        true
      } else {
        Say(s"$z2 is inaccessible")
        false
      }
    }

    report[ZextObject, Zontainer](putting) { (z1, z2) =>
      Say(s"You put $noun into $secondNoun")
    }
  }
}

