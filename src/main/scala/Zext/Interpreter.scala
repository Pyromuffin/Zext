package Zext

import Zext.*
import Zext.Actions.*
import Zext.Interpreter.*
import Zext.Parser.{boldControlCode, unboldControlCode}
import Zext.Rule.*
import Zext.World.*


object StringExpression{
  implicit def fromString(str : => String): StringExpression = {
    new StringExpression(str)
  }
}


implicit class TernaryExtension(that : Boolean) {

  def ?(str : StringExpression) : StringExpression = {
    if(that) str else ""
  }
}

class StringExpression(lazyStr : => String) {
  override def toString: String = {
    lazyStr
  }
}




object Interpreter{


  def CapitalizeStringWithControls(str : String): String = {
    if(str.startsWith(boldControlCode)){
      var s = str.stripPrefix(boldControlCode)
      s = s.capitalize
      s = boldControlCode + s
      s
    } else {
      str.capitalize
    }
  }

  def Capitalize(str : String): String = {
    var sentences = str.split('.')
    sentences = sentences.map(s => s.stripSuffix(" ").stripPrefix(" ").capitalize)
    sentences.reduce( _ + ". " + _ ) + "."

  }

  def Say(str: StringExpression): Unit = {
    if(str.toString == "") println("Empty string!!!")

    println( Capitalize(str.toString) )
  }

  def Title(str : StringExpression): Unit = {
    val orange = "\u001b[38;5;214m"
    println(orange + str.toString.split(" ").map(_.capitalize).reduce(_ + " " + _) + unboldControlCode)

  }

  def LineBreak(): Unit = {
    println("-------------")
  }
}



import scala.util.parsing.combinator.*


object Parser extends RegexParsers{

  var exit = false
  override def skipWhitespace = false

  val boldControlCode = "\u001b[0;1m"
  val unboldControlCode = "\u001b[0;0m"

  var bolded = false

  def b = if(bolded) {
    bolded = false
    unboldControlCode
  } else  {
    bolded = true
    boldControlCode
  }


  def VerbParser(action: Action) : Parser[Action] = {
    val words = action.verb.toList
    val parsers = words.map {
      _.r ^^ { s => action }
    }
    val parser = parsers.reduce( _ | _ )
    parser
  }

  def NounParser(noun: ZextObject) : Parser[ZextObject] = {
    var words = Seq(noun.name).concat(noun.aliases.toSeq)
    if(noun.pluralized){
      words = words.concat(words.map(Inflector.singularize))
    }

    val parsers = words.map {
      _.toString.r ^^ { s => noun }
    }
    val parser = parsers.reduce( _ | _ )
    parser
  }


  case class Command(action: Action, noun: Option[ZextObject], secondNoun : Option[ZextObject])

  def CommandParser(actions: Parser[Action], nouns : Parser[ZextObject]) = {
    val command = actions ~ opt("\\s+".r ~> nouns) ~ opt("\\s+".r ~> nouns) ^^ { (a) => Command(a._1._1, a._1._2, a._2) }
    command
  }


  def GetCommand(input : String, commandParser : Parser[Command]) = {
    val result = commandAliases.getOrElse(input, parseAll(commandParser, input).getOrElse(null) )
    Option(result)
  }


  def main(args: Array[String]): Unit = {
    import scala.io.StdIn.readLine
    World.Init()

    val actions = ruleSets.keys.filter(_.verb.nonEmpty)
    val actionParser = actions.map(VerbParser).reduce( _ ||| _ )
    val nounParser = ZextObject.nouns.map(NounParser).reduce( _ ||| _)
    val commandParser = CommandParser(actionParser, nounParser)

    execute(examining)
    location.OnEnter()

    while(!exit){
      print("> ")
      val input = readLine()
      val command = GetCommand(input, commandParser)
      if(command.nonEmpty){
        val c = command.get
        execute(c.action, c.noun, c.secondNoun)
        everyTurnRules.foreach( _.Execute() )
      } else {
        Say("Redacted.")
      }
    }
  }
}