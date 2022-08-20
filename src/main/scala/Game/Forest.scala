package Game

import Zext.Parser.*
import Zext.*
import Zext.Interpreter.*
import Zext.thing.*
import Zext.Direction.*
import JunimoGame.*
import Zext.Actions.*
import Zext.Rule.*




object Forest extends Room {

 name = "secret woods"

 description = "A splash of birds paint cotton-ball clouds into the sky. Seams of bark and branch are splayed with jigsaw leaf shadows. A verdant and inviting musk wafts from a clearing in the trees to the north." +
 "You are dozing on a soft tuft of grass."

 val birds = ~"Chirps punctuate the rustling of branches in the breeze" are scenery
 val leaves = ~"It's early fall. The leaves are teasing shades of orange and yellow" are scenery
 val musk = ~"Hints of cool spice and the promise of earth" is scenery
 val trees = ~"Proud, yet yielding playfully to the breeze" is scenery
 val grass = ~"The tender blades flit through your fingers as your hand mingles with the vegetation" aka "tuft" is scenery

 instead(going, Seq(south, east, west), this) {
  Say("I'd rather rest here for a while.")
 }


 Clearing connect north
}

object Clearing extends Room {
 name = "clearing"
 description = "Shrubs dot the perimeter of this modestly exposed gap in the overgrowth."


}