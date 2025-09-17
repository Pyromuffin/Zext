package Test

import Tests.wet
import Zext.*
import Zext.exports.*
import Zext.Actions.*
import Zext.EverythingParser.ParseResult
import Zext.Idea.thinking
import Zext.Parser.{Command, Disambiguate}
import Zext.Relations.Direction.*
import Zext.RoomRegioning.designates

import scala.language.postfixOps


object guy extends PlayerClass(BigTop) {
  override val name = "SLEEMO"
  override val description = "Your bipes bip fast."
  var insured = false
  val stick = ~"sticky"

}


extension(z : RelatableProxy[PlayerClass]) {
  implicit def toGuy : guy.type = z.resolve.asInstanceOf[guy.type]
}


object count {

  var calls = 0
}


object unlocking extends Action(1, "unlock")
{
  inflict(unlocking, of[Container]) {
    noun[Container].openable = true
  }
  report(unlocking) Say s"You unlock $noun"
}

object Dirt extends Room {

  override val name: StringExpression = "dirt"
  override val description: StringExpression = str {
    once("the floorboards creak underfoot.")
   "You are buried in soil."
  }


  val lockbox = new Box("A transparent lockbox with a small round keyhole") {
    val treasure = ~"lucre"
  }
  lockbox.openable = false
  lockbox.transparent = true

  val pebble = ~"the size of a small boulder"
  val mud = ~"dirt juice" amount some
  val walls = ~"they're everywhere" is fixed

  val bucket = new Box("pebble purgatory") {
    val sand = ~"paperless sandpaper" amount some
    val pants = ~"rag ensemble" is RoomDescription("A pair of pants is tangled with bucket particles") amount some // add custom amountifiers like a pair
  }

  //val zebra = z"Striped, suited to camouflage $time times in this environment"

  val chemicals = ~"10 mol\\. guydrofluouric \\?type\\? \\\\acid\\\\"


  report(opening, bucket) Say "Your pry open the bucket lid"
  // instead(opening, bucket) Say "It's sealed with bucket glue"

  val hook = Supporter("hungry tines")

  object not_yours extends Property
  val scarves = ~"An array of zebra patterned tactical scarves" is scenery is not_yours
  val mantles = ~"How did they get all these fireplaces in here???" is scenery is not_yours
  val sashes = ~"Second place winner in the number of sashes competition" is scenery is not_yours


  instead(taking, not_yours) Say "that would be uncouth"

  val crumble_block = ~"It disintegrated." is RoomDescription("A fragile crumble block teeters on the brink of existence") aka "block"

  var time = 0

  inflict(being) {
    if(first) Say("Time is ticking")
    time = time + 1
  }

  report(being) Add s"The time is $time"

  report(going, south, here) Say "You tunnel to the south."
  report(going, north, here) Say "You mosey to the north."

  report(leaving, here) Say "The tunnel collapses behind you."

  inflict(leaving, here) {
    disconnect(north)
    disconnect(south)
  }




}


object hanging extends Action(2, "hang") {
  override val implicitSubjectSelector = player

  report(hanging) Say s"$noun hangs from $secondNoun"

}


object crows_above extends Backdrop {
  val crows = ~"Several crows are circling above you."
}


object phantom_gun extends Backdrop {
  val gun = ~"this gun couild be anywhere."
  this backdrops everywhere
}

object idea_world {

  val violence = new Idea("violence") {
    override val description = "it occurs to you that guns could be anywhere."
  } is innate

  val guns = new Idea("guns") {
    override val description = "phantom guns are known to manifest when secrets are known"
  } is obvious

  after(thinking, violence) {
    subject can_discover guns
  }

  val secret = new Idea("secrets") {
    override val description = "This is a secret thought that must be revealed via some other means."
  }

  after(thinking, guns) {
    subject can_discover secret
  }

  val kitties = new Idea("kitties") {
    override val description = "that fluffy kitties are cuddly."
  } is innate

}

object Circus extends RoomRegion("Circus Region") {

  this designates (BigTop, CrowsNest)

  val circusStuff = new Backdrop {
    val circus = ~"The Sleemo Brothers Ring-a-Ding Circus Extravaganza"
    val tent = ~"A spiral candystripe towering overhead" aka "roof"

    this backdrops Circus
  }

}

object wet extends Property with Value[Int]

object drying extends Action(1, "dry") {

  applying(drying, wet) {
    if (noun(wet) > 0 && scala.util.Random.nextInt(4) == 0){
      continue
    } else fail
  }

  inflict(drying, wet) {
    noun(wet) -= 1
  }

  after(drying, wet) {
    val wetness = noun(wet)
    if (wetness != 0)
      Say(s"$noun dries out a little. It looks like it has $wetness drops of water left.")
    else if (wetness == 0)
      Say(s"$noun is completely dry")
  }

}


object dryingWith extends Action(2, "dry") {

  report(dryingWith) {
    Say(s"You dry $noun with $secondNoun")
  }

}



object finding extends Action(1, "find") with DebugAction {

  def getConnectedDirection(start : Room, next : Room): Option[Direction] = {

    for (d <- Direction.directions) {
      if( start.queryRelated(d.relation).contains(next) ) {
        return Some(d)
      }
    }

    None
  }

  inflict(finding, of[Thing]) {
    ReplaceAction(finding, Redirect(noun[Thing].room))
  }

  inflict(finding, of[Room]) {
    val start = player.room
    val end = noun[Room]
    val route = Graph.findPath(start, end, RoomAdjacency)
    var str = start.name.toString
    var prev = start
    for(i <- 1 until route.length){
      val next = route(i).asInstanceOf[Room]
      val dir = getConnectedDirection(prev, next)
      str += " -> " + dir.get.name + " -> " + next.name.toString
      prev = next
    }

    println(str)
  }

}


object BigTop extends Room {

  override val name: StringExpression = "Big Top"
  override val description: StringExpression = "You are in a giant stadium, covered by a bright tent. It seems to be sagging in the middle"

  val sagging = ~"Upon further inspection you see the sagging is caused by a small trapeze." is scenery is fixed aka "sag"

  val ladder = ~"It leads to the crow's nest" is fixed

  val red_hat  = ~"a red hat"
  val blue_hat  = ~"a blue hat"
  val hat  = ~"a hat"
  val hat_hat  = ~"a hat hat"

  val mr_super_duper_strong = ~"very strong"
  val weginald = ~"a nice guy" is wet(100) is fixed

  val bucket = new Box {
    this is wet(3)

    override val name = str {
      if (this(wet) > 0) {
        "bucket of water"
      } else {
        "dry bucket"
      }
    }


    override val description = str {
      if (this(wet) > 0) {
        "a bucket with an amount of quick-dry water"
      } else {
        "bone dry"
      }
    }
  }


  bucket holds hat


  val clothes_rack = new Supporter {


    this is fixed
    //this aka "rack"

    automaticallyListContents = false


    val shirt = ~"Solidified dye in the shape of a tank top"

    override val description = str {
      if (contents.contains(shirt))
        "A shirt is pinned to a line strewn between two very serious pots."
      else
        "The line hangs limply"
    }


    report(taking, shirt, this holds shirt) {
      Say("Trying not to disturb the pots, you carefully unclip the shirt from the line")
    }
  }

  this southward WormPile
  this upward CrowsNest

}

object loudness extends Property with Value[Int]
object loud extends Property

object clapping extends Action(0, "clap") {
  this is loudness(2)
  waiting is loudness(5)

  inflict(loud.determining) {

  }


  inflict(clapping) {
    Say("You clap!")
  }

  after(act is loud?) {
    Say("That was loud!")
  }


}


object screaming extends CustomAction(-1, "scream") {


  var screamingMode = false

  var screamCount = 0

  after(postprocessingText, screamingMode) {
    val text = postprocessingText.GetActionContext()
    postprocessingText.SetActionContext(text.toUpperCase)
  }


  before(preprocessingInput){
    val text = preprocessingInput.GetActionContext()
    if(text.toUpperCase == text){
      screamingMode = true
      screamCount += 1
      if (screamCount == 3) {
        Say("Why are you screaming?")
      }
    } else {
      screamingMode = false
    }
  }

  override def intercept(rawInput: String, parseResult: ParseResult): Command = {
    val verb = parseResult.nouns(0)(0).asInstanceOf[Action]
    val target = Disambiguate(parseResult.nouns(1)).asInstanceOf[ZextObject] // this does not respect visibility or the other normal command rules.
    screamingMode = true
    ExecuteAction(verb, subject = player, target = target)
    screamingMode = false
    Command(screaming, Array())
  }
}


object CrowsNest extends Room {
  override val name: StringExpression = "Crow's Nest"
  override val description: StringExpression = "A circular platform at the top of the ladder from which you can reach the trapeze"


  val trapeze =  "The trapeze hangs limply from a bit of scaffolding" initially
    "It looks like a barber pole, only it's orange and purple" is fixed

  report(examining, trapeze, !player.canAccess(trapeze, examining) ) Say "A handlebar that seems to be hanging from something in the sky"

  instead(hanging, player -> trapeze, !player.insured) Say "You are not insured for that"

  crows_above backdrops this

  after(dropping, player.stick) {
    player.insured = true
  }

  inflict(determiningVisibility, trapeze, Circus.here){
    succeed
  }


}




object WormPile extends Room {

  override val name: StringExpression = "Worm Pile"
  override val description: StringExpression = "I'm not sure what you expected."

  val pile_of_worms = ~"writhing all around you."

  this southward Dirt

}


object FairyFountain extends Room {

  override val name: StringExpression = "Fairy Fountain"
  override val description: StringExpression = "Piped-in harp music indicates the presence of a creature with immense power."
  val fairy_armadillo = ~"Nigiri with feet"


  instead(leaving, here) Say "The big guy wants your attention here"

  this northward Dirt
}

object listExits extends Property

object MazeEntrance extends Room {
  override val name: StringExpression = "Maze Entrance"
  override val description: StringExpression = "A daunting door looms inside."


  after(examining, listExits) {

    for(dir <- Direction.directions) {
      for( connection <- noun[Room].connections(dir))
      Say(s"$connection is to $dir")
    }

  }

  this eastward CrowsNest

  val roomCount = 100

  val rooms = Array.tabulate(roomCount) { i =>
    new Room {
      val x = i % 10
      val y = i / 10
      override val name: StringExpression = s"Maze Room ($x,$y)"
      override val description: StringExpression = "A nondescript room in the maze."
      this is listExits
    }
  }

  def linearIndex(x : Int, y : Int) = x + y * 10
  val connectionChance = 3


  for( (room, i) <- rooms.zipWithIndex) {
    // connect rooms randomly.
    val x = i % 10
    val y = i / 10

    // connect north if not on the top row
    if(y != 9 && util.Random.nextInt(connectionChance) == 0) {
      val northern = rooms(linearIndex(x, y + 1))
      room northward northern
    }

    // connect south
    if (y != 0 && util.Random.nextInt(connectionChance) == 0) {
      val southern = rooms(linearIndex(x, y - 1))
      room southward southern
    }

    // connect east
    if (x != 9 && util.Random.nextInt(connectionChance) == 0) {
      val eastern = rooms(linearIndex(x + 1, y))
      room eastward eastern
    }
    // connect west
    if (x != 0 && util.Random.nextInt(connectionChance) == 0) {
      val western = rooms(linearIndex(x - 1, y))
      room westward western
    }
  }

  val treasureRoom = rooms(util.Random.nextInt(100))
  val maze_treasure = ~"the friends we made along the way" inside treasureRoom

  rooms(0) inward this
}

object strength extends Property with Value[Int]
object nice extends Property

object purporting extends Action(1, "purport") {

  report(purporting) {

    if( noun(strength) > 3 )
      println("Strong")

    val properties = noun.queryRelated(property_having)
    Say(s"the properties of $noun are: " + properties.mkString(", "))
  }

}


object strong_zone  {

  inflict(nice.determining) {
    val name = subject.toString
    if (name.startsWith("w")) succeed
  }

  inflict(strength.valuation) {
    val name = subject.toString
    strength.valuation.SetActionContext(Some(name.length))
  }
}



object Test extends App{

  Zext.Parser.StartInterpreter(guy, "Test")

}
