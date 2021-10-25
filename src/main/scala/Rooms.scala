package Zext

import Zext._
import Zext.Globals._
import Zext.World._
import Zext.Rule._
import Zext.Interpreter._
import Zext.Direction._

import scala.language.postfixOps

object bedRoom extends Room {



    name = "bedroom"
    description = "The loft of the apartment is large enough for a bed and a desk. You have neither."
    val rock = new thing a "rock" desc "a fragment of the old mountain" has initialDescription(s"It's lodged in the floor. $playerName put it here")
    // val bob = new person named "Bob" desc "Bob is here for some reason."
    val table = new Supporter the "table" is fixed desc "a plywood mishap held up on the suggestion of four legs."
    val box = new Supporter the "box" desc "it's full of garbage."
    val chairs = new thing some "chairs" are scenery and fixed desc "A committee of seats"
    val sheet = new thing a "sheet of paper" is wet desc "incomprehensible scribbles litter the surface."
    var time = 10

    val shoes = ~ "Standard checkerboard pattern."
    shoes.indefiniteArticle = "some"


    before(taking, rock) {
        Say(s"I might make $playerName mad.")
    }

    before(examining, rock){
        val init = noun.properties(0).asInstanceOf[initialDescription]
        Say(init.desc)
    }

    instead(taking, bathroom) {
        Say("I shouldn't take anything in the bathroom.")
    }


    instead(taking, bathroom, wet) say s"I might slip! The current time is $time."


    instead(taking, sheet){
        Say("I've seen enough.")
    }


    instead(taking, fixed) {
        Say(s"$noun $is stuck.")
    }

    instead(taking, chairs){
        Say("they're bolted to the floor.")
    }

    bathroom connect west


    def main(args: Array[String]): Unit = {

        val potato = Macros.variableName
        println(potato)

        location = bedRoom
        println(shoes.name)
        World.playerName = args(0)
        execute(taking, rock)
        execute(examining, shoes)
        time = 5
        execute(examining, shoes)
        execute(taking, rock)
        playerName = "Potato"
        execute(examining, rock)
        execute(taking, chairs)
        execute(taking, table)
        execute(taking, sheet)
        location = bathroom
        execute(taking, sheet)
        execute(taking, chairs)
        execute(taking, table)
        time = 7
        execute(taking, sheet)

    }
}


object bathroom extends Room {
    name = "bathroom"
    description = "A damp closet that reeks of shotgun potpourri."
    val potpourri = ~ "By shotguns, for shotguns." is scenery

}