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
import scala.io.StdIn.readLine
import scala.language.postfixOps
import scala.reflect.TypeTest



implicit class TernaryExtension(that : Boolean) {

  def ?(str : StringExpression) : StringExpression = {
    if(that) str else ""
  }
}


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


case class Progressively(strs: StringExpression*) extends StringExpression(null) {
  var progress = 0

  override def toString: String = {
    val ret = strs(progress).toString
    progress = scala.math.min(progress + 1, strs.length - 1)
    ret
  }
}


case class Cycle(strs: StringExpression*) extends StringExpression(null) {
  var cycle = 0

  override def toString: String = {
    val ret = strs(cycle).toString
    cycle = (cycle + 1) % strs.length
    ret
  }
}

def Shuffled(strs: StringExpression*): StringExpression = {
  val shuffled = scala.util.Random.shuffle(strs)
  Cycle(shuffled *)
}


def Randomly(one_in: Int): Boolean = util.Random.nextInt(one_in) == 0

def Randomly(strs: StringExpression*): StringExpression = {
  s"${
    val which = util.Random.nextInt(strs.length)
    strs(which)
  }"
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
    val strImmediate = str.toString
    if(strImmediate == "") println("Empty string!!!")

    println( Capitalize(strImmediate) )
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


  def WordParser(str : String, parsables: Seq[Parsable[ParsableType]]) : Option[Parser[Seq[ParsableType]]] = {
    // first check if word conditions are applicable, if not splode

    val possibles = parsables.filter(_.possible)

    if (possibles.isEmpty)
      return Option.empty

    val maxPrecedence = possibles.map(_.precedence).max
    val maxPrecedenceParsables = possibles.filter(_.precedence == maxPrecedence)

    val maxSpecificity = maxPrecedenceParsables.map(_.specificity).max
    val bestParsables = maxPrecedenceParsables.filter(_.specificity == maxSpecificity)

    if(bestParsables.length > 1){
     // println(str + " IS AMBIGUOUS! " + parsables.toString())
    }

    // val bestParsable = bestParsables.head

    val parser = str.toLowerCase ^^ {s => bestParsables.map(_.target)}

    Some(parser)
  }



  // ok so it seem like
  // each word needs to have its own set of corresponding targets, and conditions
  //type ParsableTypes = ZextObject | Action | Command

  enum PartOfSpeech {
    case noun, verb
  }

  trait ParsableType(val part : PartOfSpeech )

  case class Parsable[T <: ParsableType](target : T, conditions : Condition*){

    def possible = {
      val visible = if(target.part == PartOfSpeech.noun){
        target.asInstanceOf[ZextObject].isVisible(currentLocation)
      } else true

      conditions.forall(_.evaluate) && visible
    }

    def specificity = {
      conditions.map( _.specificity ).sum
    }

    def precedence = {
      // we need to do it in this funny way so that we get zero if there are no conditions, instead of just doing .max
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


  type CommandParserType = Parser[(Seq[ParsableType], Option[Seq[ParsableType]], Option[Seq[ParsableType]])]

  def BuildParser2() : CommandParserType = {
    val allParsers : Seq[Parser[Seq[ParsableType]]] = understandables.map(WordParser).filter(_.isDefined).map(_.get).toSeq
    val allParser =  allParsers.reduce( _ ||| _ )
    val space = "\\s+".r
    val anySpace = "\\s*".r
    val prepositions = Array("on", "to", "on to", "into", "at", "on top of", "in", "about")
    val prepositionParser = prepositions.map(Parser(_)).reduce(_ ||| _)

    val command = anySpace ~> allParser ~ opt(space ~> allParser) ~ (opt(space ~> prepositionParser) ~> opt(space ~> allParser)) ^^ { (a) =>
      val action = a._1._1
      val firstNoun = a._1._2
      val secondNoun = a._2
      (action, firstNoun, secondNoun)
    }

    command
  }


  def Disambiguate(parsables : Seq[ParsableType]) : ParsableType = {


    val orange = "\u001b[38;5;214m"

    if(parsables.length == 1)
      return parsables.head


    var s = "Did you mean "
    var index = 1
    for(p <- parsables){
      s += s"($orange$index$unboldControlCode) ${p.toString} or "
      index += 1
    }
    s = s.stripSuffix(" or ")
    s += "?"

    Say(s)

    var done = false
    var choiceIndex = -1
    while(!done){

      print("Choose > ")
      val choice = readLine().toLowerCase

      try{
        choiceIndex = Integer.parseInt(choice)
      } catch {
        case e : NumberFormatException =>
      } finally {
        if(choiceIndex > 0 && choiceIndex <= parsables.length)
          done = true
      }
    }

    parsables(choiceIndex - 1)
  }

  def BuildCommand(input : String, parser : CommandParserType) : Option[Command]=  {

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

    val zeroth = Disambiguate(triple._1)

    if(zeroth.part != PartOfSpeech.verb)
      return None

    val action = zeroth.asInstanceOf[Action]
    if(action.targets != targets)
      return None

    // this kinda sucks
    if(triple._2.isDefined && triple._3.isDefined) {

      val first = Disambiguate(triple._2.get)
      val second = Disambiguate(triple._3.get)

      if(first.part == PartOfSpeech.verb || second.part == PartOfSpeech.verb)
        return None

      if (action.targets != targets)
        return None


      return Some(Command(action, Some(first.asInstanceOf[ZextObject]), Some(second.asInstanceOf[ZextObject])))
    }

    else if(triple._2.isDefined) {
      val first = Disambiguate(triple._2.get)

      if (first.part == PartOfSpeech.verb)
        return None

      return Some(Command(action, Some(first.asInstanceOf[ZextObject]), None))
    } else {

      return Some(Command(action, None, None))
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
      val input = readLine().toLowerCase
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