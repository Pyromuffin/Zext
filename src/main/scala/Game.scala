package Zext

import Zext.*
import World.*
import Rule.*
import Interpreter.*
import Direction.*
import Actions.*
import thing.*

import scala.language.postfixOps

object Rooms {

    object bedroom extends Room {

        name = "bedroom"
        description = "The loft of the apartment is large enough for a bed and a desk. You have neither."
        val rock = new thing a "rock" desc "a fragment of the old mountain" has initialDescription(s"It's lodged in the floor. $playerName put it here")
        val table = new Supporter the "table" is fixed desc "a plywood mishap held up on the suggestion of four legs."
        val box = new Supporter the "box" desc "it's full of garbage."
        val chairs = new thing some "chairs" are scenery and fixed desc "A committee of seats"
        val sheet = new thing a "sheet of paper" is wet desc "incomprehensible scribbles litter the surface."

        val shoes = ~ "Standard checkerboard pattern." mass


        before(taking, rock) {
            Say(s"I might make $playerName mad.")
        }

        before(examining, rock){
            val init = noun.properties(0).asInstanceOf[initialDescription]
            Say(init.desc)
        }


        instead(taking, sheet){
            Say("I've seen enough.")
        }


        instead(taking, chairs){
            Say("they're bolted to the floor.")
        }

        bathroom connect west
    }


    object bathroom extends Room {
        name = "bathroom"
        description = "A damp closet that reeks of shotgun potpourri."
        val shotgun_potpourri = ~ "By shotguns, for shotguns." is scenery mass

        instead(taking, bathroom, wet) say s"I might slip! The current time is $time."

        report(taking, shotgun_potpourri) {
            Say("I shoved a fistful of the powder into my pocket.")
        }

        after(taking, shotgun_potpourri) {
            description = "A damp closet. A hint of stale gunpowder wafts through the air."
        }
    }
}

