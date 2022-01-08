package Zext

import Zext.*
import Zext.Actions.*
import Zext.Interpreter.*
import Zext.Rule.*
import Zext.World.*


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



import scala.util.parsing.combinator.*


object Parser extends RegexParsers{

  var exit = false
  override def skipWhitespace = false


  def VerbParser(action: Action) : Parser[Action] = {
    val words = action.verb.toList
    val parsers = words.map {
      _.r ^^ { s => action }
    }
    val parser = parsers.reduce( _ | _ )
    parser
  }

  def NounParser(noun: ZextObject) : Parser[ZextObject] = {
    val words = Seq(noun.name).concat(noun.aliases.toSeq)
    val parsers = words.map {
      _.toString.r ^^ { s => noun }
    }
    val parser = parsers.reduce( _ | _ )
    parser
  }


  case class Command(action: Action, noun: Option[ZextObject])

  def CommandParser(actions: Parser[Action], nouns : Parser[ZextObject]) = {
    val command = actions ~ opt("\\s+".r ~> nouns) ^^ { (a) => Command(a._1, a._2) }
    command
  }




  def main(args: Array[String]): Unit = {
    import scala.io.StdIn.readLine
    World.Init()

    
    val actions = ruleSets.keys
    val actionParser = actions.map(VerbParser(_)).reduce( _ ||| _ )
    val nounParser = ZextObject.nouns.map(NounParser(_)).reduce( _ ||| _)
    val commandParser = CommandParser(actionParser, nounParser)

    execute(examining)

    while(!exit){
      print("> ")
      val input = readLine()
      val command = parseAll(commandParser, input)
      if(command.successful){
        val c = command.get
        execute(c.action, c.noun)
      } else {
        Say("I'm not sure what you mean.")
      }
    }
  }
}