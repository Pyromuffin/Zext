package Zext



object StringExpression{
  implicit def fromString(str : => String): StringExpression = {
    new StringExpression(str)
  }
}


class StringExpression(lazyStr : => String) {
  override def toString: String = {
    lazyStr
  }
}


object Interpreter{
  def Say(str: StringExpression): Unit = {
    println(str.toString.capitalize)
  }
}

import Interpreter.*
import Zext.Rule.{ActionRuleSet, before, execute, ruleSets}
import Zext.World.playerName

import scala.util.parsing.combinator.*


object Parser extends RegexParsers{

  var exit = false


  def VerbParser(action: Action) : Parser[Action] = {
    val words = action.verb.toList
    val parsers = words.map {
      _.r ^^ { s => action }
    }
    val parser = parsers.reduce( _ | _ )
    parser
  }

  def TargetParser(noun: ZextObject) : Parser[ZextObject] = {
    val words = Seq(noun.name) // .concat(noun.aliases.toSeq)
    val parsers = words.map {
      _.toString.r ^^ { s => noun }
    }
    val parser = parsers.reduce( _ | _ )
    parser
  }

  def RegisterVerbs() = {
    // this is incredibly hideous but i do not care anymore.
    import org.reflections.*
    val zext = new Reflections("Zext")
    val actionsClasses = zext.getSubTypesOf(classOf[Action])
    actionsClasses.forEach { a =>
      try{
        val companion = a.getField("MODULE$").get(a).asInstanceOf[Action]
      } catch {
        case e : NoSuchFieldException => ;
      }
    }
  }

  case class Command(action: Action, noun: Option[ZextObject])


  def CommandParser(actions: Parser[Action], nouns : Parser[ZextObject]) = {
    val command = actions ~ opt(nouns) ^^ { (a) => Command(a._1,a._2) }
    command
  }


  object exiting extends Action("exit") {
    override def executeNone(): Boolean = {
      Say(s"Goodbye $playerName")
      exit = true
      true
    }
  }


  def main(args: Array[String]): Unit = {
    import scala.io.StdIn.readLine
    RegisterVerbs()
    World.location = bedRoom


    val actions = ruleSets.keys
    val actionParser = actions.map(VerbParser(_)).reduce( _ | _ )
    val nounParser = ZextObject.nouns.map(TargetParser(_)).reduce( _ | _)
    val commandParser = CommandParser(actionParser, nounParser)

    execute(examining)

    while(!exit){
      print("> ")
      val input = readLine()
      val command = parse(commandParser, input)
      if(command.successful){
        val c = command.get
        execute(c.action, c.noun)
      }
    }
  }
}