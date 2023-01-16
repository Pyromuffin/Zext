package Game

import Zext.Actions.*
import Zext.Rule.*
import Zext.Interpreter.*
import Zext.{Action, Macros, StringExpression, TernaryExtension, player, reflexively}
import Zext.World.{EndingDay, StartingGame, currentLocation}


/*
ideas
survival burgers
put the pet in the game


start out in your house
need to get one item to complete the collection
grandpa's ghost needs to show up
season detection
*/



object JunimoGame {

  enum Gender :
    case triangle, hourglass, octagon


  var farmer : String = "Farmer"
  var farm : String = "Grandpa's Farm"
  var favoriteThing : String = "Parsnips"
  var gender : Gender = Gender.octagon
  var rigidity : Int = 0
  var maxEncumbrance = 10
  def encumbrance = maxEncumbrance - player.contents.length
  var dirty = true


  val flannelShirt = Clothing(
    name = "Flannel Shirt",
    fastenedDesc = "A gift from Gus during a late night pub crawl, the well-used red and black patterned flannel is buttoned tightly against your body. It smells like sweat and freshly-tilled soil",
    unfastenedDesc = "The flannel drapes around your shoulders, anticipating its defeat to gravity",
    unfasteningText = "You hastily unbutton the shirt, tensely releasing each button from its captivity, like letting your livestock out to graze in the morning sun",
    removalText = s"The soft yet durable fabric slides off your ${dirty ? "dusty"} skin, a shed husk, a sacrifice to yoba for the sins you're about to commit",
    fastened = true)


  var hatSlot : Option[Clothing] = None
  var overallsSlot : Option[Clothing] = None
  var shirtSlot : Option[Clothing] = Some(flannelShirt)
  var pantsSlot : Option[Clothing] = None
  var undergarmentSlot : Option[Clothing] = None
  var socksSlot : Option[Clothing] = None
  var shoesSlot : Option[Clothing] = Some(steelToeBoots)


  def GetEquipmentDescription() : String = {
    var s = ""
    s += "On your head you are wearing " + hatSlot.map(_.name).getOrElse("nothing") + ".\n"
    s += "Your outer shell is " + overallsSlot.map(_.name).getOrElse("bare") + ".\n"
    s += "On your upper bod is " + shirtSlot.map(_.name).getOrElse("skin") + ".\n"
    s += "On your lower bod you are wearing " + pantsSlot.map(_.name).getOrElse("a birthday suit") + ".\n"
    s += "Under all that betrays " + undergarmentSlot.map(_.name).getOrElse("nudity") + ".\n"
    s += "On your toes you keep " + socksSlot.map(_.name).getOrElse("forgetting to put on socks") + ".\n"
    s += "and " + shoesSlot.map(_.name).getOrElse("hopes alone") + " protect your feet"
    s
  }

  case class Clothing(name : StringExpression, fastenedDesc : StringExpression, unfastenedDesc : StringExpression, removalText : StringExpression, unfasteningText : StringExpression, var fastened: Boolean)

  def GetDressState() = {

  }



  before(StartingGame){
    val parsed = StardewParser.Do()
    farmer = parsed._1
    farm = parsed._2
    favoriteThing = parsed._3

    val startDescription = s"After a hard day toiling on fields of $farm, $farmer decides to take a break with a relaxing nap in the Secret Woods"
    Say(startDescription)
  }
// I broke this and I cannot begin to guess how
//  before(takingInventory) {
//    if(dirty)
//      Say(s"The dust you earned from today's productive work at $farm clings to your body. A reminder of an honest job well done.")
//  }

  // inventory is something like: backpack : Pickaxe, shovel, hoe, sword, watering can, stones, fiber, sap, logs, clothes

  def GetRigidity() = {

    if(rigidity == 0)
       "Flaccid as a trashcan souffle"

    else if(rigidity == 1)
       "Limp as wet straw"

    else if(rigidity == 2)
       "Halfie, like a freshly peeled orange slice"

    else if(rigidity == 3)
       "Firm, as Mayor Lewis' handshake"

    else if(rigidity == 4)
       "Rigid, a chiseled monument to Stardew Valley"

    else
       "Unknown Rigidity"
  }

}

// idk where to put this
object sleeping extends Action(0, "sleep", "rest", "slumber", "nap") {


  instead(sleeping, currentLocation != FarmHouse){
    Say("You'd rather sleep in your bed.")
  }

  report(sleeping){
    Say("You drift away in night's embrace")
  }

  after(sleeping){
    execute(EndingDay, reflexively)
  }


}