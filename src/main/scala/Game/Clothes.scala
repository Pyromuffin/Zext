package Game

import Game.JunimoGame.Clothing
import Zext.*
import Zext.exports.*

object Mountains extends Room{

  val name = "mountains"
  val description = "Das Gebirge"

  report(being, here, Randomly(8), visited) Say "You catch hints of steam from the hot springs. Or maybe that's just your imagination. Wishful thinking"

//  report(entering, here, !visited)


  Connect(south, Path) //yes i know
}

val steelToeBoots = Clothing(
  name = "Steel Toe Boots",
  fastenedDesc = "A gift from Gus during a late night pub crawl, the well-used red and black patterned flannel is buttoned tightly against your body. It smells like sweat and freshly-tilled soil",
  unfastenedDesc = "The flannel drapes around your shoulders, anticipating its defeat to gravity",
  unfasteningText = "You hastily unbutton the shirt, tensely releasing each button from its captivity, like letting your livestock out to graze in the morning sun",
  removalText = s"The soft yet durable fabric slides off your} skin, a shed husk, a sacrifice to yoba for the sins you're about to commit",
  fastened = true)
//
//val boxerBriefs = Clothing(
//  name = "Boxer Briefs"
//)