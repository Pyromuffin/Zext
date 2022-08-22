package Game

import java.io.*
import java.nio.file.attribute.BasicFileAttributes
import scala.io.Source


object StardewParser {

  def Do() : (String, String, String) = {
    val documentsDir = System.getenv("APPDATA")
    val stardewDir = new File(documentsDir + "\\StardewValley\\Saves")

    if(!stardewDir.exists())
      return ("farmer", "farm", "favoriteThing")


    val folders = stardewDir.listFiles().filter(_.isDirectory)
    val filter = new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name == "SaveGameInfo"
    }

    val attrs = folders.map(f => f -> java.nio.file.Files.readAttributes(f.toPath, classOf[BasicFileAttributes]).lastModifiedTime())
    val saveGameInfo = attrs.maxBy(_._2)

    val filtered = saveGameInfo._1.listFiles(filter)
    val file = filtered(0)
    val fileP = scala.io.Source.fromFile(file)
    val text = fileP.mkString
    fileP.close()
    val nameRegex = raw"<name>(.*?)</name>".r
    val farmNameRegex = raw"<farmName>(.*?)</farmName>".r
    val favoriteThingRegex = raw"<favoriteThing>(.*?)</favoriteThing>".r

    val farmer = nameRegex.findFirstMatchIn(text).get.group(1)
    val farm = farmNameRegex.findFirstMatchIn(text).get.group(1)
    val favoriteThing = favoriteThingRegex.findFirstMatchIn(text).get.group(1)

    (farmer, farm, favoriteThing)
  }
}
