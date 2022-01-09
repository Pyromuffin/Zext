package Zext

import Zext.Interpreter.*
import Zext.Parser.*
import Zext.Rule.*
import Zext.World.*

object Actions {

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

  object examining extends Action("examine", "x", "look") {

    carryOut(examining) {
      execute(examining, location)
    }

    carryOut[Room](examining) { r =>
      Say(location.name)
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
