package Zext

import Zext.Interpreter.Say
import Zext.Parser.exit
import Zext.Rule.*
import Zext.World.*


object Actions {

  object taking extends Action("take", "get") {
    override def executeNone() : Boolean = {
      Say(s"I can't take nothing.")
      true
    }


    instead(taking, fixed) {
      Say(s"$noun $is stuck.")
    }

    report(taking) {
      Say(s"I took $noun.")
    }

    override def executeOne(zextObject: ZextObject) : Boolean = {
      inventory += noun
      val container = noun.parentContainer.contents
      container.remove( container.indexOf(noun) )
      true
    }
  }

  object examining extends Action("examine", "x", "look" ) {
    override def executeNone() : Boolean = {
      noun = location
      Say(location.name)
      Say(location.description)
      true
    }

    override def executeOne(noun : ZextObject) : Boolean = {
      val immediate = s"$noun: ${noun.description}"
      Say(immediate)
      true
    }

    after(examining, classOf[Room]) {
      val r = noun.asInstanceOf[Room]
      val visible = r.contents.filterNot(_ ? scenery)
      if(!visible.isEmpty){
        var s = "You can see "
        for(i <- visible){
          if( !i.?(scenery) )
            s += i.indefinite + ", "
        }
        s = s.stripSuffix(", ")
        s += "."
        Say(s)
      }
    }
  }


  object exiting extends Action("exit") {
    override def executeNone(): Boolean = {
      Say(s"Goodbye $playerName")
      exit = true
      true
    }
  }


  object takingInventory extends Action("inventory", "i"){
    override def executeNone() = {
      var s = "I am holding "
      for(i <- inventory){
        s += i.indefinite + ", "
      }
      s = s.stripSuffix(", ")
      s += "."
      Say(s)
      true
    }
  }
}
