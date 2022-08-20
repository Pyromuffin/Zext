package Game

import Zext.Actions.*
import Zext.Rule.*
import Zext.Interpreter.*
import Zext.StringExpression
import Zext.World.StartingGame

object JunimoGame {

  enum Gender :
    case triangle, hourglass, octagon


  var farmer : String = "Zexa-Swan"
  var farm : String = "Zero Zillion"
  var favoriteThing : String = "Holograms"
  var gender : Gender = Gender.octagon
  var rigidity : Int = 0
  var maxEncumbrance = 10
  var dirty = true


  val flannelShirt = Clothing(
    name = "Flannel Shirt",
    fastenedDesc = "A gift from Gus during a late night pub crawl, the well-used red and black patterned flannel is buttoned tightly against your body. It smells like sweat and freshly-tilled soil",
    unfastenedDesc = "The flannel drapes around your shoulders, anticipating its defeat to gravity",
    unfasteningText = "You hastily unbutton the shirt, tensely releasing each button from its captivity, like letting your livestock out to graze in the morning sun",
    removalText = s"The soft yet durable fabric slides off your ${dirty ? "dusty"} skin, a shed husk, a sacrifice to yoba for the sins you're about to commit",
    fastened = true)


  var hatSlot : Clothing = null
  var overallsSlot : Clothing = null
  var shirtSlot : Clothing = flannelShirt
  var pantsSlot : Clothing = null
  var undergarmentSlot : Clothing = null
  var socksSlot : Clothing = null
  var shoesSlot : Clothing = null

  case class Clothing(name : StringExpression, fastenedDesc : StringExpression, unfastenedDesc : StringExpression, removalText : StringExpression, unfasteningText : StringExpression, var fastened: Boolean)

  def GetDressState() = {

  }

  val startDescription = s"After a hard day toiling on fields of $farm, $farmer decides to take a break with a relaxing nap in the Secret Woods"

  before(StartingGame){
    Say(startDescription)
  }

  before(takingInventory) {
    if(dirty)
      Say(s"The dust you earned from today's productive work at $farm on clings to your body. A reminder of an honest job well done.")
  }

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
