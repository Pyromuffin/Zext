package Zext

import Zext.*
import Zext.Actions.*
import Zext.Condition.canBecome
import Zext.Interpreter.*
import Zext.Parser.{boldControlCode, unboldControlCode}
import Zext.Rule.*
import Zext.World.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.reflect.TypeTest


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
    val words = action.verbs.toList
    val parsers = words.map {
      _.r ^^ { s => action }
    }
    val parser = parsers.reduce( _ | _ )
    parser
  }

  def NounParser(noun: ZextObject) : Parser[ZextObject] = {
    var words = Seq(noun.name).concat(noun.aliases.toSeq)
 

    val parsers = words.map {
      _.toString.r ^^ { s => noun }
    }
    val parser = parsers.reduce( _ | _ )
    parser
  }


  def WordParser(str : String, parsables: Seq[Parsable[ParsableType]]) : Option[Parser[ParsableType]] = {
    // first check if word conditions are applicable, if not splode
    val possible = parsables.filter(_.possible)
    if(possible.isEmpty)
      return None

    if(possible.length > 1)
      println(s"AMBIGUOUS ! $str")

    val parser = str.r ^^ {s => possible.head.target}

    Some(parser)
  }



  // ok so it seem like
  // each word needs to have its own set of corresponding targets, and conditions
  //type ParsableTypes = ZextObject | Action | Command

  enum PartOfSpeech {
    case noun, verb
  }

  trait ParsableType(val part : PartOfSpeech )

  case class Parsable[+T](target : T, conditions : Condition*){

    def possible = conditions.forall(_.evaluate)
  }

  val understandables = mutable.HashMap[String, Seq[Parsable[ParsableType]]]()

  def Understand[T <: ParsableType](target : T, words : String*)( conditions: Condition* ): Unit = {
    for(word <- words){
      if( understandables.contains(word) ){
        val parsables = understandables(word)
        val added = parsables :+ Parsable(target, conditions*)
        understandables(word) = added
      } else {
        understandables.update(word, Seq(Parsable(target, conditions*)))
      }
    }
  }

  def Understand[T <: ParsableType](target : T, words : String*): Unit = {
    Understand(target, words*)()
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



  def BuildParser() = {

    BuildParser2()


    val actions = ruleSets.keys.filter(_.verbs.nonEmpty)
    val actionParser = actions.map(VerbParser).reduce( _ ||| _ )
    val nounParser = null // ZextObject.nouns.map(NounParser).reduce( _ ||| _)
    val commandParser = CommandParser(actionParser, nounParser)


    commandParser
  }


  def BuildParser2() : Parser[(ParsableType, Option[ParsableType], Option[ParsableType])] = {
    val allParsers : Seq[Parser[ParsableType]] = understandables.map(WordParser).filter(_.isDefined).map(_.get).toSeq
    val allParser = allParsers.reduce( _ ||| _ )
    val command = allParser ~ opt("\\s+".r ~> allParser) ~ opt("\\s+".r ~> allParser) ^^ { (a) =>
      val action = a._1._1
      val firstNoun = a._1._2
      val secondNoun = a._2
      (action, firstNoun, secondNoun)
    }

    command
  }


  def BuildCommand(input : String, parser : Parser[(ParsableType, Option[ParsableType], Option[ParsableType])]) : Option[Command]=  {

    if(commandAliases.contains(input)){
      return Some(commandAliases(input))
    }

    val result = parseAll(parser, input)

    if(result.isEmpty){
      return None
    }

    val triple = result.get

    // this kinda sucks
    if(triple._2.isDefined && triple._2.get.part == PartOfSpeech.verb)
      return None

    if(triple._3.isDefined && triple._3.get.part ==  PartOfSpeech.verb)
      return None

    if( triple._1.part == PartOfSpeech.verb ){
      val action = triple._1.asInstanceOf[Action]
      val n1 = triple._2.map( p => p.asInstanceOf[ZextObject] )
      val n2 = triple._3.map( p => p.asInstanceOf[ZextObject] )

      return Some(Command(action, n1, n2))
    }

    None
  }

  def main(args: Array[String]): Unit = {
    import scala.io.StdIn.readLine
    World.Init()

    execute(examining)
    currentLocation.OnEnter()

    while(!exit){
      val commandParser = BuildParser2()

      print("> ")
      val input = readLine()
      val command = BuildCommand(input, commandParser)
      if(command.nonEmpty){
        val c = command.get
        EvaluatePreviouslyConditions(c.action, c.noun, c.secondNoun)
        execute(c.action, c.noun, c.secondNoun)
        everyTurnRules.foreach( _.Execute() )
      } else {
        Say("Redacted.")
      }
    }
  }
}