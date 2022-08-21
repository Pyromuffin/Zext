package Zext

import Zext.Interpreter.*
import Zext.Parser.*
import Zext.Rule.*
import Zext.World.*

object Actions {


  object goingEast extends Action("east", "e")
  object goingWest extends Action("west", "w")
  object goingNorth extends Action("north", "n")
  object goingSouth extends Action("south", "s")

  object going extends Action("go", "travel", "walk", "run", "cartwheel"){

    carryOut(goingEast) { execute(going, Direction.east) }
    carryOut(goingWest) { execute(going, Direction.west) }
    carryOut(goingNorth) { execute(going, Direction.north) }
    carryOut(goingSouth) { execute(going, Direction.south) }

    carryOut[Direction](going){ d =>
      val other = location.connections.get(d)
      other match{
        case Some(r) => {
          Say(s"I went $d to $r.")
          location = r
          LineBreak()
          execute(examining, location)
          location.OnEnter()
        }
        case _ => {
          Say(s"I can't go $d.")
        }
      }
      true
    }
  }


  object taking extends Action("take", "get") {

    carryOut(taking) {
      Say(s"I can't take nothing.")
      false
    }


    instead(taking, fixed) {
      Say(s"$noun $is stuck.")
      false
    }

    report[ZextObject](taking) { n =>
      Say(s"I took $n.")
    }

    carryOut[ZextObject](taking) { n =>
      inventory += n
      val container = n.parentContainer.contents
      container.remove(container.indexOf(n))
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
      for (i <- inventory) {
        s += i.indefinite + ", "
      }
      s = s.stripSuffix(", ")
      s += "."
      Say(s)
      true
    }
  }
}
