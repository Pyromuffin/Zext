package Zext

import EverythingParser.{ParseResultType, time}
import Zext.*
import Zext.Actions.*
import Zext.Condition.canBecome
import Zext.Interpreter.*
import Zext.Parser.{boldControlCode, unboldControlCode}
import Zext.Rule.*
import Zext.RuleContext.{_noun, location, silent}
import Zext.Saving.*
import Zext.StringExpression.str
import Zext.World.*

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import scala.annotation.targetName
import scala.io.StdIn.readLine
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine
import scala.language.postfixOps
import scala.util.boundary
import boundary.break
import zobjectifier.Macros


// this maybe has to be the last import?
//import scala.reflect.TypeTest




object StringExpression{

  /**
   * stateful strings use a source code position macro to track which state they should access
   * so if multiple stateful strings are created at the same source code position (eg, procedurally in a loop)
   * then they will share the same state unless you provide an extra key using the withKey method
   */
  trait StatefulString(val key : String) {

    this : StringExpression =>

    var extraKey = ""

    /** Sets an extra key for procedurally created stateful strings  */
    def withKey(str : String) : this.type = {
      extraKey = str
      this
    }

    def GetState() : Int = {
      StringExpression.state.getOrElse(key + extraKey, 0)
    }

    def UpdateState(state : Int): Unit = {
      StringExpression.state(key + extraKey) = state
    }

    def ResetState(): Unit = {
      UpdateState(0)
    }
  }


  private val state = mutable.HashMap[String, Int]()

  implicit def fromString(str : => String): StringExpression = {
    new StringExpression(str)
  }

  def str (s : => StringExpression) : StringExpression = {
    new StringExpression(s"$s")
  }


  implicit def fromStateful(str : StatefulString): StringExpression = {
    str.asInstanceOf[StringExpression]
  }

  private case class ProgressivelyHolder(pos : String, strs: StringExpression*) extends StringExpression(null) with StatefulString(pos) {

    override def toString: String = {
      var state = GetState()
      val ret = strs(state).toString
      state = scala.math.min(state + 1, strs.length - 1)
      UpdateState(state)
      ret
    }
  }

  private case class CycleHolder(pos : String, strs: StringExpression*) extends StringExpression(null) with StatefulString(pos) {

    override def toString: String = {
      var state =  GetState()
      val ret = strs(state).toString
      state = (state + 1) % strs.length
      UpdateState(state)
      ret
    }
  }

  inline def cycle(strs: StringExpression*): StatefulString = {
    CycleHolder(Macros.CodePosition(), strs *)
  }

  inline def progressively(strs: StringExpression*) : StatefulString = {
    ProgressivelyHolder(Macros.CodePosition(), strs *)
  }

  inline def once(str: StringExpression) : StatefulString = {
    progressively(str, "")
  }

  def infrequently(s : StringExpression, one_in: Int) : StringExpression = str {
    if scala.util.Random.nextInt(one_in) == 0 then s else ""
  }

  def randomly(strs: StringExpression*): StringExpression = str {
      val which = util.Random.nextInt(strs.length)
      strs(which)
    }
}

class StringExpression(lazyStr : => String) extends Serializable{

  def unary_~(using c: Container & ZextObject): Thing = {
    SimpleThing(this)
  }

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

  def MakeTextNice(str : String): String = {

    val endingPunctuation = Seq('.', '?', '!')

    val sentences = ArrayBuffer[String]()
    val escapeCharacterPositions = ArrayBuffer[Int]()
    var escapeNext = false
    var unescaped = ""
    for(c <- str.zipWithIndex) {
      if(c._1 == '\\' && !escapeNext) {
        escapeNext = true
        escapeCharacterPositions.append(c._2 - escapeCharacterPositions.length)
      }
      else
      {
        escapeNext = false
        unescaped += c._1
      }
    }

    var lastPunctuationIndex = 0
    for(c <- unescaped.zipWithIndex){
      if(endingPunctuation.contains(c._1) && c._2 != unescaped.length && !escapeCharacterPositions.contains(c._2)) {
        var index = c._2
        if(index + 1 < unescaped.length && unescaped(index + 1) == '\"'){
          index += 1
        }
        sentences += unescaped.substring(lastPunctuationIndex, index + 1).stripPrefix(" ").stripSuffix(" ").capitalize
        escapeCharacterPositions.clear()
        lastPunctuationIndex = index + 1

      }
    }


    // last sentence
    sentences += unescaped.substring(lastPunctuationIndex, unescaped.length).stripPrefix(" ").stripSuffix(" ").capitalize

    var ret = sentences.foldRight("")( _ + " " + _).stripSuffix(" ")
    if(!endingPunctuation.contains(unescaped.last) && unescaped.last != '\"'){
      ret += '.'
    }

    ret = ret.stripSuffix(" ")

    ret
  }

  def NumberPrompt(str: StringExpression) : Int = {
    Say(str)
    print("> ")
    var number = readLine().toLowerCase
    var parsed = scala.util.control.Exception.allCatch.opt( Integer.parseInt(number) )

    while(parsed.isEmpty) {
      Say("That's not a number.")

      number = readLine().toLowerCase
      parsed = scala.util.control.Exception.allCatch.opt( Integer.parseInt(number) )
    }

    parsed.get
  }

  def SystemMessage(str: StringExpression): Unit = {
    saying.text = str.toString
    execute(saying, noun, secondNoun, silent, location = player.location)
  }

  def Say(str: StringExpression): Unit = {
    saying.text = str.toString
    execute(saying, noun, secondNoun, silent, location)
  }

  def Title(str : StringExpression): Unit = {
    val orange = "\u001b[38;5;214m"
    println(orange + str.toString.split(" ").map(_.capitalize).reduce(_ + " " + _) + unboldControlCode)

  }

  def LineBreak(): Unit = {
    println("-------------")
  }
}





object Parser {

  var exit = false

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


  def GetWords(z : ZextObject) : Seq[String] = {
    var names = Seq(z.name).concat(z.aliases).map(_.toString.toLowerCase)

    // filter out ignored words
    names = names.map( _.stripPrefix("the ").stripPrefix("a ").stripPrefix("some "))

    if(z.pluralized.isEmpty && z.isInstanceOf[Thing] && z[Thing].isAutomaticallyPlural) {
      names = names.concat(names.map(Inflector.singularize))
    }
    else if(z.pluralized.isDefined && z.pluralized.get){
      names = names.concat(names.map(Inflector.singularize))
    }

    if(z.autoexplode)
      names = names concat names.flatMap(NLP.GetNouns)

    names.distinct
  }


  def BuildUnderstandables(): Unit = {

    understandables.clear()

    for(a <- Actions.allActions){
      Understand(a, a.verbs *)
    }

    ZextObject.allObjects.foreach { z =>
      Understand(z, GetWords(z)*)
    }
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
      conditions.forall(_.evaluate)
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

  def Disambiguate(parsables : Seq[ParsableType], hint : ParsableType => Boolean) : ParsableType = {

    val orange = "\u001b[38;5;214m"

    if(parsables.length == 1)
      return parsables.head

    if(hint != null){
      val likely = parsables.filter(hint)

      if (likely.length == 1)
        return likely.head
    }


    var s = "Did you mean "
    var index = 1
    for(p <- parsables){
      s += s"($orange$index$unboldControlCode) ${p.toString} or "
      index += 1
    }
    s = s.stripSuffix(" or ")
    s += "?"

    SystemMessage(s)

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


  def BuildZeroTargetCommand(action: Action) : Option[Command] = {
    Some(Command(action, None, None))
  }

  def BuildOneTargetCommand(action: Action, firsts : Option[Array[ParsableType]]) : Option[Command] = {

    if(firsts.isEmpty && action.implicitTargetSelector != null){

      val set = action.implicitTargetSelector.getSet().asInstanceOf[Seq[ZextObject]]

      val candidates = set.filter(_.isVisibleTo(player))
      if(candidates.nonEmpty){
        val first = Disambiguate(candidates.toSeq, action.disambiguationHint).asInstanceOf[ZextObject]
        return Some(Command(action, Some(first), None))
      } else {
        SystemMessage(s"${action.verbs(0)} with what?")
        // no implicit candidates
        return None
      }
    } else if (firsts.isEmpty){
      SystemMessage(s"${action.verbs(0)} with what?")
      // no first noun and no implicit selector
      return None
    }

    val visible = firsts.get.filter {
      case z: ZextObject => z.isVisibleTo(player)
      case _ => false
    }
    if(visible.isEmpty)
      return None

    val first = Disambiguate(visible, action.disambiguationHint).asInstanceOf[ZextObject]
    Some(Command(action, Some(first), None))
  }


  def BuildTwoTargetCommand(action: Action, firsts : Option[Array[ParsableType]], seconds : Option[Array[ParsableType]]) : Option[Command] =  {

    val first : ZextObject =
      if (firsts.isEmpty && action.implicitSubjectSelector != null) {

        val set = action.implicitSubjectSelector.getSet().asInstanceOf[Seq[ZextObject]]
        val candidates = set.filter(_.isVisibleTo(player))

        if (candidates.nonEmpty) {
          Disambiguate(candidates.toSeq, action.disambiguationHint).asInstanceOf[ZextObject]
        } else {
          SystemMessage(s"${action.verbs(0)} with what?")
          // no implicit candidates
          null
        }

      } else if (firsts.isEmpty) {
        SystemMessage(s"${action.verbs(0)} with what?")
        // no first noun and no implicit selector
        null
      } else  {

        val visible = firsts.get.filter {
          case z: ZextObject => z.isVisibleTo(player)
          case _ => false
        }

        if(visible.isEmpty)
          null
        else
          Disambiguate(visible, action.disambiguationHint).asInstanceOf[ZextObject]
      }

    if(first == null){
      return None
    }
    
    // for tests against noun in the target selector hint
    _noun = first

    if(seconds.isEmpty && action.implicitTargetSelector != null){

      val set = action.implicitTargetSelector.getSet().asInstanceOf[Seq[ZextObject]]
      val candidates = set.filter(_.isVisibleTo(player))
      if(candidates.nonEmpty){
        val second = Disambiguate(candidates.toSeq, action.disambiguationHint).asInstanceOf[ZextObject]
        return Some(Command(action, Some(first), Some(second)))
      } else {
        // no implicit candidates
        SystemMessage(s"${action.verbs(0)} $first with what?")
        return None
      }
    } else if( seconds.isEmpty){
      // no second target and no implicit selector
      SystemMessage(s"${action.verbs(0)} $first with what?")
      return  None
    }

    val visible = seconds.get.filter {
      case z: ZextObject => z.isVisibleTo(player)
      case _ => false
    }

    if (visible.isEmpty)
      return None

    val second = Disambiguate(visible, action.disambiguationHint).asInstanceOf[ZextObject]
    Some(Command(action, Some(first), Some(second)))
  }


  def BuildCommand(input : String, result: ParseResultType) : Option[Command]=  {

    if(commandAliases.contains(input)){
      return Some(commandAliases(input))
    }

    if(result.isEmpty){
      return None
    }

    val triple = result.get

    val parsedTargetCount =
      if (triple._2.isDefined && triple._3.isDefined) 2
      else if (triple._2.isDefined) 1
      else 0

    val parsedTargetCountFilter : ParsableType => Boolean = {
      case a: Action => a.targets == parsedTargetCount
      case _ => false
    }

    val zeroth = Disambiguate(triple._1, parsedTargetCountFilter)
    if(zeroth.part != PartOfSpeech.verb)
      return None

    val action = zeroth.asInstanceOf[Action]

    if(parsedTargetCount > action.targets) {
      return None
    }

    action.targets match {
      case 0 => BuildZeroTargetCommand(action)
      case 1 => BuildOneTargetCommand(action, triple._2)
      case 2 => BuildTwoTargetCommand(action, triple._2, triple._3)
      case _ => None
    }
  }

  def InitTests(player: PlayerClass, gamePackageName: String) : Unit = {
      World.Init(player, gamePackageName)
      World.testingOutput = true
  }

  def RunTest(testName: String, inputStrings : Array[String], expectedOutput : Array[String]) : Boolean = {

    testOutput.clear()

    for(input <- inputStrings) {
      val result = EverythingParser.parse(input)

      val command1 = BuildCommand(input, result._1)
      val command2 = BuildCommand(input, result._2)
      val command3 = BuildCommand(input, result._3)

      val commands = Array(command3, command2, command1)

      boundary{
        for (command <- commands) {
          command does { c =>
            ExecuteAction(c.action, c.noun, c.secondNoun)
            execute(being, player.location)
            break()
          }
        }
        SystemMessage("[REDACTED]")
      }
    }

    for(output <- expectedOutput.zip(World.testOutput)) {
      if(output._1 != output._2) {
        println(s"Test output mismatch: Expected: ${output._1} != ${output._2}")
        return false
      }
    }

    println(s"$testName: succeeded")
    true
  }

  def StartInterpreter(player : PlayerClass, gamePackageName : String): Unit = {
    World.Init(player, gamePackageName)

    time("startup"){
      EverythingParser.parse("start")
      execute(starting)
    }

    execute(examining, player.location)

    while(!exit) {
      print("> ")
      val input = readLine().toLowerCase

      time("interpreter loop") {

        val results  = time("parsing") {
          val r = EverythingParser.parse(input)
          Array(r._3, r._2, r._1)
        }

        boundary {
          for (result <- results) {
            val command = BuildCommand(input, result)
            command does { c =>
              RunApplyingBeforeRules(c)
              time("command execution"){
                ExecuteAction(c.action, c.noun, c.secondNoun)
              }
              RunApplyingRules(c)
              execute(being, player.location)
              break()
            }
          }
          SystemMessage("[REDACTED]")
        }
      }
    }
  }
}



