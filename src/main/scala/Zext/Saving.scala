package Zext

import java.io.*
import java.lang.reflect.Modifier
import scala.collection.mutable.ArrayBuffer

object Saving {

  // we want to save the contents of every item in every room.
  // we also want to save the properties on each item.
  // maybe some global variables like turn number, and player location.

  def SaveWorld() = {
    println(s"saving world")

    val fileName = "world.zav"

    //World.currentWorld.allObjects = ZextObject.allObjects

    val objectOutputStream = new ObjectOutputStream(new FileOutputStream(fileName))

    for(obj <- ZextObject.allObjects){
      obj.SerializeMembers(objectOutputStream)
    }

    /*
    for(z <- ZextObject.allObjects) {
      val clazz = z.getClass
      val fields = clazz.getDeclaredFields
      val lastField = fields.last
      if(lastField.getName == "MODULE$"){
        // objects

        println(s"object detected $z")
        z.SerializeMembers(objectOutputStream)
      } else {
        // normal instances or anonymous classes
        println(s"serializing normal instance $z")
        objectOutputStream.writeObject(z)
      }
    }
    */

    objectOutputStream.close
  }


  def FindPlayer(rooms : ArrayBuffer[Room]) : PlayerClass = {

    for(r <- rooms){
      val hasPlayer = r.contents.find( _.getClass == classOf[PlayerClass])
      if(hasPlayer.nonEmpty)
        return hasPlayer.get.asInstanceOf[PlayerClass]
    }

    throw ClassNotFoundException()
  }

  def LoadWorld() : Unit = {

    println(s"loading world")
    val fileName = "world.zav"
    val ois = new ObjectInputStream(new FileInputStream(fileName))

    /*
    for (r <- World.currentWorld.rooms) {
      r.contents = ois.readObject().asInstanceOf[ArrayBuffer[ZextObject]]
    }*/

    for (obj <- ZextObject.allObjects) {
      obj.DeserialzeMembers(ois)
    }


    //World.currentWorld.player = FindPlayer(World.currentWorld.rooms)

    /*

    for (z <- ZextObject.allObjects) {
      val clazz = z.getClass
      val fields = clazz.getDeclaredFields
      val lastField = fields.last
      if (lastField.getName == "MODULE$") {
        // objects

        println(s"object input detected $z")
        z.DeserialzeMembers(ois)
      } else {
        // normal instances or anonymous classes
        println(s"deserializing normal instance $z")
        val obj = ois.readObject()

      }
    }


    */

      ois.close

      //World.currentWorld = worldState

  }
}
