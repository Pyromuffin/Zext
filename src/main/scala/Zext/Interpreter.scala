package Zext

import EverythingParser.{ParseResult, time}
import Zext.*
import Zext.Actions.*
import Zext.Condition.canBecome
import Zext.ControlCodes.{boldControlCode, normalControlCode, orange}
import Zext.Interpreter.*
import Zext.Parser.PartOfSpeech.verb
import Zext.Parser.PartOfSpeech
import Zext.Rule.*
import Zext.RuleContext.{GetCurrentRuleContext, _noun, _nouns, location, silent}
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
    ExecuteContextAction(saying(str.toString), subject = system, location = player.location) // always say this at the player location.
  }

  def Say(str: StringExpression): Unit = {
    ExecuteContextAction(saying(str.toString))
  }



  def Title(str : StringExpression): Unit = {
    println(orange + str.toString.split(" ").map(_.capitalize).reduce(_ + " " + _) + normalControlCode)

  }

  def LineBreak(): Unit = {
    println("-------------")
  }
}


object ControlCodes {
  val orange = "\u001b[38;5;214m"
  val boldControlCode = "\u001b[0;1m"
  val normalControlCode = "\u001b[0;0m"
}


object Parser {

  var exit = false
  var bolded = false

  def b = if(bolded) {
    bolded = false
    normalControlCode
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

    understandableNouns.clear()
    understandableVerbs.clear()
    understandableEverything.clear()

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
    case noun, verb, custom
  }

  trait ParsableType(val part : PartOfSpeech )

  case class CustomWord(word : String) extends ParsableType(PartOfSpeech.custom)

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

  val understandableEverything = mutable.HashMap[String, Seq[Parsable[ParsableType]]]()
  val understandableVerbs = mutable.HashMap[String, Seq[Parsable[ParsableType]]]()
  val understandableNouns = mutable.HashMap[String, Seq[Parsable[ParsableType]]]()

  def Understand[T <: ParsableType](target : T, words : String*)( conditions: Condition* ): Unit = {

    var understandables = if (target.part == verb)  understandableVerbs
    else if (target.part == PartOfSpeech.noun)  understandableNouns
    else ???

    for(word <- words){
      if( understandables.contains(word) ){
        val parsables = understandables(word)
        val added = parsables :+ Parsable(target, conditions*)
        understandables(word) = added
      } else {
        understandables.update(word, Seq(Parsable(target, conditions*)))
      }
    }

    // do again for everything dictionary
    understandables = understandableEverything
    for (word <- words) {
      if (understandables.contains(word)) {
        val parsables = understandables(word)
        val added = parsables :+ Parsable(target, conditions *)
        understandables(word) = added
      } else {
        understandables.update(word, Seq(Parsable(target, conditions *)))
      }
    }

  }

  def Understand[T <: ParsableType](target : T, words : String*): Unit = {
    Understand(target, words*)()
  }

  case class Command(action: Action, nouns: Array[ZextObject])

  def Disambiguate(parsables : Seq[ParsableType], hint : ParsableType => Boolean = null) : ParsableType = {

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
      s += s"($orange$index$normalControlCode) ${p.toString} or "
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
    Some(Command(action, Array()))
  }

  def BuildOneTargetCommand(action: Action, targets : Array[Array[ParsableType]]) : Option[Command] = {

    val firsts = targets.headOption

    if(firsts.isEmpty && action.implicitTargetSelector != null){

      val set = action.implicitTargetSelector.getSet()

      val candidates = set.filter(player.canSee(_, action))
      if(candidates.nonEmpty){
        val first = Disambiguate(candidates, action.disambiguationHint).asInstanceOf[ZextObject]
        return Some(Command(action, Array(first)))
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
      case z: ZextObject =>
        if(action.isInstanceOf[DebugAction]) true
        else player.canSee(z, action)

      case _ => false
    }


    if(visible.isEmpty)
      return None

    val first = Disambiguate(visible, action.disambiguationHint).asInstanceOf[ZextObject]
    Some(Command(action, Array(first)))
  }


  def BuildTwoTargetCommand(action: Action, targets : Array[Array[ParsableType]]) : Option[Command] =  {

    val firsts = targets.headOption

    val first : ZextObject =
      if (firsts.isEmpty && action.implicitSubjectSelector != null) {

        val set = action.implicitSubjectSelector.getSet()
        val candidates = set.filter(player.canSee(_, action))

        if (candidates.nonEmpty) {
          Disambiguate(candidates, action.disambiguationHint).asInstanceOf[ZextObject]
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
          case z: ZextObject =>
            if (action.isInstanceOf[DebugAction]) true
            else player.canSee(z, action)

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

    val seconds = targets.lift(1)

    if(seconds.isEmpty && action.implicitTargetSelector != null){

      val set = action.implicitTargetSelector.getSet()
      val candidates = set.filter(player.canSee(_, action))
      if(candidates.nonEmpty){
        val second = Disambiguate(candidates, action.disambiguationHint).asInstanceOf[ZextObject]
        return Some(Command(action, Array(first,second)))
      } else {
        // no implicit candidates
        SystemMessage(s"${action.verbs(0)} $first with what?")
        return None
      }
    } else if(seconds.isEmpty){
      // no second target and no implicit selector
      SystemMessage(s"${action.verbs(0)} $first with what?")
      return None
    }

    val visible = seconds.get.filter {
      case z: ZextObject =>
        if(action.isInstanceOf[DebugAction]) true
        else player.canSee(z, action)

      case _ => false
    }

    if (visible.isEmpty)
      return None

    val second = Disambiguate(visible, action.disambiguationHint).asInstanceOf[ZextObject]
    Some(Command(action, Array(first, second)))
  }


  def BuildNTargetCommand(action: Action, targets : Array[Array[ParsableType]]) : Option[Command] =  {

    if(targets.isEmpty) {
      return None
    }

    val disambiguated = targets.map { target =>
      val visible = target.filter {
        case z: ZextObject =>
          if (action.isInstanceOf[DebugAction]) true
          else player.canSee(z, action)

        case _ => false
      }

      // we do not want to leak that something exists but is invisible.
      if (visible.isEmpty) {
        return None
      }

      Disambiguate(visible, action.disambiguationHint).asInstanceOf[ZextObject]
    }

    if(disambiguated.length != targets.length) {
      return None
    }

    Some(Command(action, disambiguated))
  }



  def BuildCommand(input : String, result: Option[ParseResult]) : Option[Command]=  {

    if(commandAliases.contains(input)){
      return Some(commandAliases(input))
    }

    if(result.isEmpty){
      return None
    }

    val triple = result.get
    val parsedTargetCount = triple.nouns.length

    val parsedTargetCountFilter : ParsableType => Boolean = {
      case a: Action => a.targets == parsedTargetCount
      case _ => false
    }

    val zeroth = Disambiguate(triple.verbs, parsedTargetCountFilter)
    if(zeroth.part != PartOfSpeech.verb)
      return None

    val action = zeroth.asInstanceOf[Action]

    if(parsedTargetCount > action.targets && action.targets != -1) {
      return None
    }

    action match {
      case custom : CustomAction => return Some(Command(action, Array()))
      case _ =>
    }

    action.targets match {
      case 0 => BuildZeroTargetCommand(action)
      case 1 => BuildOneTargetCommand(action, triple.nouns)
      case 2 => BuildTwoTargetCommand(action, triple.nouns)
      case -1 => BuildNTargetCommand(action, triple.nouns)
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
      time("interpreter loop") {

        val results = time("parsing") {
          val r = EverythingParser.parse(input)
          Array(r._1, r._2)
        }

        boundary {
          for (result <- results) {
            val command = BuildCommand(input, result)
            command does { c =>
              RunApplyingBeforeRules(c)
              time("command execution") {
                c.action match
                  case action: CustomAction =>
                    val customCommand = action.intercept(input, result.get)
                    ExecuteAction(customCommand.action, RuleContext(player, customCommand.nouns, false, player.location))
                  case action: Action =>
                    ExecuteAction(c.action, RuleContext(player, c.nouns, false, player.location))
              }
              RunApplyingRules(c)
              ExecuteAction(being, RuleContext(player, Array(player.location), false, player.location))
              break()
            }
          }
          SystemMessage("Input couldn't be interpreted as a command.")
        }
      }
    }

    for(output <- expectedOutput.zip(World.testOutput)) {
      if(output._1 != output._2) {
        println(s"$testName: Test output mismatch: Expected: ${output._1} != ${output._2}")
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
      ExecuteAction(starting, subject = player, location = player.location)
    }


    while(!exit) {
      print("> ")
      var input : String = readLine()

      input = ExecuteContextAction(preprocessingInput(input), subject = system).ret

      time("interpreter loop") {
        val results  = time("parsing") {
          val r = EverythingParser.parse(input)
          Array(r._1, r._2)
        }

        boundary {
          for (result <- results) {
            val command = BuildCommand(input, result)
            command does { c =>
              RunApplyingBeforeRules(c)
              time("command execution") {
              c.action match
                case action: CustomAction =>
                  val customCommand = action.intercept(input, result.get)
                  ExecuteAction(customCommand.action, RuleContext(player, customCommand.nouns, false, player.location))
                case action : Action =>
                  ExecuteAction(c.action, RuleContext(player, c.nouns, false, player.location))
              }
              RunApplyingRules(c)
              ExecuteAction(being, subject = player, target = player.location, location = player.location)
              break()
            }
          }
          SystemMessage("Input couldn't be interpreted as a command.")
        }
      }
    }
  }
}



