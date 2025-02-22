package Zext

import scala.collection.mutable.ArrayBuffer
import Zext.*
import Zext.Actions.*
import Zext.Condition.canBecome
import Zext.Parser.BuildUnderstandables
import Zext.Relations.Containment.holds
import Zext.Rule.*

import scala.reflect.TypeTest

object nothing extends ZextObject {
  val name = "nothing"
  val description = ""

  ZextObject.globals.addOne(this)
}

class ZextObjectClassHolder(tt : TypeTest[ZextObject | Container,?], depth: Int, className : String) extends ConditionHelper {

  var not = false
  def unary_! = {
    not = !not
    this
  }

  def createCondition(queryPrecedence: QueryPrecedence) = {

    val condition = new Condition(
      {
        val target = if (queryPrecedence == QueryPrecedence.Class) noun else secondNoun
        var success = test(target)
        if(not) success = !success

        //println(s"testing if $target is $className with depth $depth : $success")
        success
      }
      , queryPrecedence)

    condition.specificity = depth
    condition
  }

  def test(z : ZextObject) : Boolean = {
    z match {
      case tt(z) => true
      case _ => false
    }
  }
}



class ZextObjectPropHolder(tt : TypeTest[Property,?], depth: Int, propName : String) extends ConditionHelper {

  var not = false

  def unary_! = {
    not = !not
    this
  }

  def createCondition(queryPrecedence: QueryPrecedence) = {

    val condition = new Condition(
      {
        val target = if (queryPrecedence == QueryPrecedence.Property) noun else secondNoun
        var success = test(target)
        if (not) success = !success

        //println(s"testing if $target has property $propName: $success")
        success
      }
      , queryPrecedence)

    condition.specificity = depth
    condition
  }

  def test(z: ZextObject): Boolean = {
    z.properties.exists{ p =>
      p match {
        case tt(p) => true
        case _ => false
      }
    }
  }
}

object anything extends ZextObject {
  val name = "anything"
  val description = ""
}


object nowhere extends Room {
  val name = "nowhere"
  val description = ""
  proper = true


}


abstract class PlayerClass(startingLocation : ZContainer) extends Thing(using startingLocation) with Container {

  properties += scenery
  automaticallyListContents = false
  open = false
  transparent = false

  def Move(container: ZContainer): Unit = {
    container holds this
  }

  this.aliases.addOne("self").addOne("me")
}

class WorldState{

  val rooms = ArrayBuffer[Room]()
  val regions = ArrayBuffer[RoomRegion]()
  var player : PlayerClass = null

  var time = 0
}

object player extends ZextObjectProxy[PlayerClass] {
  override def resolve = World.currentWorld.player
}



object World  {

  var testingOutput = false
  val testOutput = new ArrayBuffer[String]()
  var currentWorld = new WorldState


  def RevealSecrets(path: String, className : String): Unit = {
    val c = Class.forName(s"$path.$className")
    val f = c.getMethod("Reveal")
    f.invoke(null)
  }

  def TouchPackage(path: String): Unit = {

    val cl = ClassLoader.getSystemClassLoader
    val resource = cl.getResource(path)
    val dir = new java.io.File(resource.getFile)
    val files = dir.listFiles()
    val classNames = files.filter(f => f.getName.endsWith("$SecretHolder.class")).map(f => f.getName.stripSuffix(".class"))

    classNames.foreach { cn =>
      RevealSecrets(path, cn)
    }
  }

  val secretNames = Array(
  "Actions$SecretHolder",
  "Inflector$SecretHolder",
  "Interpreter$SecretHolder",
  "Person$SecretHolder",
  "Room$SecretHolder",
  "Rule$SecretHolder",
  "Saving$SecretHolder",
  "World$SecretHolder",
  "Container$SecretHolder",
  "Relations$SecretHolder",
  "Zext$SecretHolder",
  )



  def Init(gamesPlayer : PlayerClass, gamePackageName : String): Unit = {

    // i think it will be hard or impossible to switch players, but maybe that's ok.
    currentWorld.player = gamesPlayer

    for(name <- secretNames){
      RevealSecrets("Zext", name)
    }

    TouchPackage(gamePackageName)

  }
}
