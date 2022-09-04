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

    val endingPunctuation = Seq('.', '?', '!')

    val sentences = ArrayBuffer[String]()
    var lastPunctuationIndex = 0
    for(c <- str.zipWithIndex){
      if(endingPunctuation.contains(c._1) && c._2 != str.length) {
        var index = c._2
        if(index + 1 < str.length && str(index + 1) == '\"'){
          index += 1
        }
        sentences += str.substring(lastPunctuationIndex, index + 1).stripPrefix(" ").stripSuffix(" ").capitalize
        lastPunctuationIndex = index + 1
      }
    }
    sentences += str.substring(lastPunctuationIndex, str.length).stripPrefix(" ").stripSuffix(" ").capitalize


    var ret = sentences.foldRight("")( _ + " " + _).stripSuffix(" ")
    if(!endingPunctuation.contains(str.last) && str.last != '\"'){
      ret += '.'
    }

    ret
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
    val possibles = parsables.filter(_.possible)

    if (possibles.isEmpty)
      return Option.empty

    val maxPrecedence = possibles.map(_.precedence).max
    val maxPrecedenceParsables = possibles.filter(_.precedence == maxPrecedence)
    val bestParsable = maxPrecedenceParsables.maxBy(_.specificity)

    // println(s"AMBIGUOUS ! $str")

    val parser = str.r ^^ {s => bestParsable.target}

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

    def specificity = {
      conditions.map( _.specificity ).sum
    }

    def precedence = {
      conditions.map(_.precedence).foldLeft(0)( _ max _ )
    }

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



  def BuildParser2() : Parser[(ParsableType, Option[ParsableType], Option[ParsableType])] = {
    val allParsers : Seq[Parser[ParsableType]] = understandables.map(WordParser).filter(_.isDefined).map(_.get).toSeq
    val allParser = allParsers.reduce( _ ||| _ )
    val prepositions = Array("on", "to", "on to", "into", "at", "on top of", "in")
    val prepositionParser = prepositions.map(Parser(_)).reduce(_ ||| _)

    val command = allParser ~ opt("\\s+".r ~> allParser) ~ ( opt("\\s+".r ~> prepositionParser) ~> opt("\\s+".r ~> allParser)) ^^ { (a) =>
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

    var targets = 0
    if(triple._2.isDefined) targets += 1
    if(triple._3.isDefined) targets += 1


    // this kinda sucks
    if(triple._2.isDefined && triple._2.get.part == PartOfSpeech.verb)
      return None


    if(triple._3.isDefined && triple._3.get.part ==  PartOfSpeech.verb)
      return None


    if( triple._1.part == PartOfSpeech.verb ){
      val action = triple._1.asInstanceOf[Action]
      val n1 = triple._2.map( p => p.asInstanceOf[ZextObject] )
      val n2 = triple._3.map( p => p.asInstanceOf[ZextObject] )

      if(action.targets != targets)
        return None

      return Some(Command(action, n1, n2))
    }

    None
  }

  def main(args: Array[String]): Unit = {
    import scala.io.StdIn.readLine
    World.Init()

    execute(examining, reflexively)
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
        execute(being, currentLocation)
      } else {
        Say("Redacted.")
      }
    }
  }
}