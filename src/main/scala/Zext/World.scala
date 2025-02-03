package Zext

import scala.collection.mutable.ArrayBuffer
import Zext.*
import Zext.Actions.*
import Zext.Condition.canBecome
import Zext.Parser.BuildUnderstandables
import Zext.Rule.*

import scala.reflect.TypeTest

object nothing extends ZextObject {
  val name = "nothing"
  val description = ""
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


abstract class PlayerClass extends Thing(using World.GetStartingLocation()) with Container {

  properties += scenery
  automaticallyListContents = false
  open = false
  transparent = false

  def Move(container: ZContainer): Unit = {
    container contains this
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
  def playerLocation = player.parentContainer
  def playerRoom = player.findRoom()


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
  "Zext$SecretHolder",
  )

  def GetStartingLocation() : ZContainer = {
    currentWorld.rooms.find(_.isInstanceOf[StartingRoom]).get

  }

  def Init(gamesPlayer : PlayerClass, gamePackageName : String): Unit = {

    // i think it will be hard or impossible to switch players, but maybe that's ok.
    currentWorld.player = gamesPlayer

    for(name <- secretNames){
      RevealSecrets("Zext", name)
    }

    TouchPackage(gamePackageName)

    //val startingRoom = GetStartingLocation()
    //startingRoom.visited = true
    //gamesPlayer.parentContainer = startingRoom
    //startingRoom.contents.addOne(gamesPlayer)



  }

  


}

import scala.quoted.* // imports Quotes, Expr
def inspectCode(x: Expr[Any])(using Quotes): Expr[Any] =
  import quotes.reflect.*

  println(x.show)

  val ext = '{
    extension(x : Any) {
      def potato = println(x)
    }
  }

  val method = '{
      def potato(z : Any) = println(z)
  }


  println(ext.asTerm)
  println(method.asTerm)
  x

inline def inspect(inline x: Any): Any = ${ inspectCode('x) }

transparent inline def MakeOption[T](thing : T) : Any = ${ MakeOptionImpl('thing) }

def MakeOptionImpl[T](expr: Expr[T])(using Quotes, Type[T]): Expr[Option[T]] = {
  //import quotes.reflect.*

  '{Option(${expr})}
}


/*

class printTree extends MacroAnnotation {

  override def transform(using q : Quotes)(tree: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] = {
    import q.reflect.{*, given}


    val classSymbol = tree match {
      case c : ClassDef => c.symbol
    }

    val body = tree match {
      case c : ClassDef => c.body
      case _ => null
    }




    for(st <- body){
      st match {
        case d: DefDef =>{
          println("print defdef: " + d.show(using Printer.TreeStructure))
          println(d.symbol.flags.show)

          val stuff = DefDef.unapply(d)
          val defdefType = stuff._3.tpe

          val methodSymbol = d.symbol

          val extensionSymbol = Symbol.newMethod(classSymbol, "Extension_" + d.name, defdefType, Flags.Method | Flags.ExtensionMethod | Flags.Infix, Symbol.noSymbol)




        }
        case _ =>
      }
    }




    println("print tree: " + tree.show(using Printer.TreeStructure))
    List(tree)
  }
}

*/