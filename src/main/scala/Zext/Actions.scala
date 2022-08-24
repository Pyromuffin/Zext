package Zext

import Zext.Interpreter.*
import Zext.Parser.*
import Zext.Rule.*
import Zext.World.*

import scala.collection.mutable
import scala.reflect.TypeTest

object Actions {

  val commandAliases = mutable.HashMap[String, Command]()

  def Understand(str: String, action: Action, zextObject1: ZextObject = null, zextObject2: ZextObject = null) : Unit = {
    commandAliases.addOne(str -> Command(action, Option(zextObject1), Option(zextObject2)))
  }

  def Randomly(one_in : Int) : Boolean = util.Random.nextInt(one_in) == 0
  def Randomly(strs: StringExpression*) : StringExpression = {
    val which = util.Random.nextInt(strs.length)
    strs(which)
  }


  object going extends Action("go", "travel", "walk", "run", "cartwheel"){

    Understand("east", going, Direction.east)
    Understand("e", going, Direction.east)
    Understand("west", going, Direction.west)
    Understand("w", going, Direction.west)
    Understand("north", going, Direction.north)
    Understand("n", going, Direction.north)
    Understand("south", going, Direction.south)
    Understand("s", going, Direction.south)

    val nowhere = Room(); nowhere.global = true
    var goingDir : Direction = null

    inflict[Direction](going) { d => goingDir = d; execute(going, location.connections.getOrElse(d, nowhere)) }

    // weird hack
    before(going, nowhere.asDestination){
      Say(s"I can't go $goingDir from here.")
      false
    }

    // you can still circumvent going direction based movements by going directly to a location... eh i'll fix it later

    // desired behavior:
    // override the "i went x " text, which should be the act of reporting going
    // once we have moved to the other room, and reported, we want to automatically examine the room.

    inflict[Room](going) { r =>
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
    }
  }

  object dropping extends Action("drop", "abandon"){

    report(dropping){
      Say( Randomly("You stop, drop, and roll.", "You fall to your knees for no reason.",
        """You throw yourself to the floor.
          |Like a cat licking itself nonchalantly after doing something embarrassing, you pretend you dropped a contact.""".stripMargin))
    }

    inflict[ZextObject](dropping){ z =>   // a/n z is noun  (this lamda has to take ZextObject argument) 
      if(z.parentContainer == inventory){
        z.transferTo(location)
        true
      }
      else {
        Say("Can't drop what you don't have.")
        false
      }
    }

    report[ZextObject](dropping){ z =>
      Say( Randomly(s"$noun gently flutters to the ground.", s"Discarded, $noun crashes into earth.", s"You abandon $noun to its fate.") )
    }
  }

 object smelling extends Action( "inhale", "snort", "vape", "endocytose", "flehm", "flehmen", "sniff", "nasalize"){

   inflict(smelling){
     Say (Randomly("You wrinkle your nose and lift your lips, giving' that vestigial Vomeronasal Organ another go", "Ah, the smells.", "Moved here for the grandpa, stayed for the smells."))
    true
   }
   after(smelling, noun.parentContainer == inventory) {
     Say(s"The scent of $noun is soaked through your clothing now.")
   }



   instead(smelling, fixed) {
     Say(s"$noun: Scentless and fixed. ")
     false
   }

   report[ZextObject](smelling) { n =>
     Say(s"You smell $n. Wow.")
   }

   inflict[ZextObject](smelling) { n =>

     true
   }
 }


  object tasting extends Action("eat", "lick", "nom", "mouth", "nibble") {

    inflict(tasting) {
      execute(tasting, location)
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

    inflict[ZextObject](taking) { n =>
      noun.transferTo(inventory)
      true
    }
  }

  object examining extends Action("examine", "x", "look", "l") {

    inflict(examining) {
      execute(examining, location)
    }

    inflict[Room](examining) { r =>
      Title(location.name)
      Say(location.description)
      true
    }


    inflict[Crevice](examining) { r =>
      Title(location.name + " It's small! ")
      Say(location.description)
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
      execute(examining1, location)
    }

    inflict[Room](examining1) { r =>
      Title(location.name)
      Say(location.description)
      true
    }


    inflict[Crevice](examining1) { r =>
      Title(location.name + " It's small! ")
      Say(location.description)
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

      if (z1.parentContainer == inventory && z2.isAccessible(location)) {
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