package Game

import java.io.*
import scala.io.Source

class StardewParser {

  val documentsDir = System.getenv("APPDATA")
  val stardewDir = new File(documentsDir + "\\StardewValley\\Saves")
  val folders = stardewDir.listFiles()
  val filter = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = name == "SaveGameInfo"
  }

  val saveGameInfo = folders.maxBy(_.lastModified).listFiles(filter)
  val xmlSource = xml.XML.loadFile(saveGameInfo(0)).child
  val farmer = xmlSource.find( _.label == "name")
  val farm = xmlSource.find( _.label == "farmName")
  val favorite = xmlSource.find( _.label == "favoriteThing")
  farmer.foreach( t => println(t.text))
  farm.foreach( t => println(t.text))
  favorite.foreach( t => println(t.text))


}
