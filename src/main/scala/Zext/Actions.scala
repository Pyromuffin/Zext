package Zext

import Zext.Interpreter.*
import Zext.Parser.*
import Zext.Rule.*
import Zext.World.*

import scala.collection.mutable

object Actions {

  val commandAliases = mutable.HashMap[String, Command]()

  def Understand(str: String, action: Action, zextObject: ZextObject = null) : Unit = {
    commandAliases.addOne(str -> Command(action, Option(zextObject)))
  }

  def Randomly(one_in : Int) : Boolean = util.Random.nextInt(one_in) == 0


  object going extends Action("go", "travel", "walk", "run", "cartwheel"){

    Understand("east", going, Direction.east)
    Understand("e", going, Direction.east)
    Understand("west", going, Direction.west)
    Understand("w", going, Direction.west)
    Understand("north", going, Direction.north)
    Understand("n", going, Direction.north)
    Understand("south", going, Direction.south)
    Understand("s", going, Direction.south)

    val nowhere = Room()
    var goingDir : Direction = null

    carryOut[Direction](going) { d => goingDir = d; execute(going, location.connections.getOrElse(d, nowhere)) }

    // weird hack
    before(going, nowhere.asDestination){
      Say(s"I can't go $goingDir from here.")
      false
    }

    // you can still circumvent going direction based movements by going directly to a location... eh i'll fix it later

    // desired behavior:
    // override the "i went x " text, which should be the act of reporting going
    // once we have moved to the other room, and reported, we want to automatically examine the room.

    carryOut[Room](going) { r =>
      // room has to be connected to location
      var connected = false

      for( (d, room) <- location.connections) {
        if(r == room){
          connected = true
          goingDir = d
        }
      }

      if(!connected) {
        Say(s"I can't get to $r from here.")
      }
      connected
    }

    report[Room](going){ r =>
      Say(s"I went $goingDir to $r.")
    }

    after[Room](going){ r =>
      location = r
      LineBreak()
      execute(examining, location)
      location.OnEnter()
      true
    }
  }


  object taking extends Action("take", "get", "pick up") {

    carryOut(taking) {
      Say(s"I can't take nothing.")
      false
    }

    instead(taking, noun.parentContainer == inventory){
      Say(s"I shuffle around the items in my pockets, looking for $noun")
    }

    instead(taking, fixed) {
      Say(s"$noun $is shoracle")
      false
    }

    report[ZextObject](taking) { n =>
      Say(s"You took $n. Wow.")
    }

    carryOut[ZextObject](taking) { n =>
      noun.TransferTo(inventory)
      true
    }
  }

  object examining extends Action("examine", "x", "look", "l") {

    carryOut(examining) {
      execute(examining, location)
    }

    carryOut[Room](examining) { r =>
      Title(location.name)
      Say(location.description)
      true
    }


    carryOut[Crevice](examining) { r =>
      Title(location.name + " It's small! ")
      Say(location.description)
      true
    }


    carryOut[ZextObject](examining) { n =>
      val immediate = s"$n: ${n.description}"
      Say(immediate)
      true
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
  }


  object exiting extends Action("exit") {
    carryOut(exiting) {
      Say(s"Goodbye $playerName")
      exit = true
      true
    }
  }

  object takingInventory extends Action("inventory", "i") {
    carryOut(takingInventory) {
      var s = "I am holding "
      for (i <- inventory.contents) {
        s += i.indefinite + ", "
      }
      s = s.stripSuffix(", ")
      s += "."
      Say(s)
      true
    }
  }
}
