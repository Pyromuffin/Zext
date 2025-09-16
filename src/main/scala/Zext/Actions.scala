package Zext

import Zext.Interpreter.*
import Zext.Parser.*
import Zext.Rule.*
import Zext.World.*
import Zext.ZextObject.*
import Zext.StringExpression.*
import Zext.RuleContext.*

import scala.collection.mutable
import scala.reflect.TypeTest
import Condition.*
import Zext.Relations.*
import Zext.ControlCodes.*

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import RelatableProxy.*


object Actions {

  val commandAliases = mutable.HashMap[String, Command]()
  val allActions = ArrayBuffer[Action]()
  val allMetaActions = ArrayBuffer[MetaAction[?]]()

  def UnderstandAlias(str: String, action: Action, zextObject1: ZextObject = null, zextObject2: ZextObject = null): Unit = {
    val targets = ConsolidateTargets(zextObject1, zextObject2).map(_.asInstanceOf[ZextObject]) //@todo hack
    commandAliases.addOne(str -> Command(action, targets))
  }

  def UnderstandAlias(strs: Seq[String], action: Action, zextObject1: ZextObject, zextObject2: ZextObject) : Unit = {
    val targets = ConsolidateTargets(zextObject1, zextObject2).map(_.asInstanceOf[ZextObject])  //@todo hack
    commandAliases.addAll(strs.map( _ -> Command(action, targets)))
  }

  object being extends Action(1)
  object waiting extends Action(0, "wait", "loiter") {
    report(waiting) Say "You wait for minute"
  }

  object starting extends Action(0) {
    var started = false
    Understand(starting, "start")(!started)

    inflict(starting) {
      started = true
    }

    report(starting){
      println("------Zext--------")
    }

    after(starting) {
      ExecuteAction(examining, subject = player, target = player.location)
    }

  }


  object preprocessingInput extends Action(0) with Context[String]{

    inflict(preprocessingInput) {
      val text = GetActionContext()
      SetActionContext(text.toLowerCase)
    }

  }

  object postprocessingText extends Action(0) with Context[String] {

    inflict(postprocessingText) {
      val text = GetActionContext()
      SetActionContext( MakeTextNice(text) )
    }

  }

  // for hooking, call SetActionContext with the final name
  object printing_name extends Action(1) with Context[String]

  object saying extends Action(0) with Context[String] {
    check(saying) {
      if(subject == system)
        continue

      if(location != player.location)
        stop

      if (silent)
        stop
    }

    inflict(saying) {
      val text = GetActionContext()
      if (text == "") stop // maybe an error

      var postprocessed = ExecuteContextAction(postprocessingText(text)).ret
      if(subject == system){
        postprocessed = postprocessed.bold
      }

      if (testingOutput){
        testOutput.addOne(MakePlain(postprocessed))
      }
      else
        println(postprocessed)
    }

  }



  /*
    considerations for room movement:
      i think we want the behavior to be something like
      1) check if leaving, going, and entering is possible, if not, dont do any of those things.
      2) do going, leaving, entering in that order
   */


  object entering extends Action(1) {


    inflict(entering, of[Room]){
      noun[Room].visited = true
    }

    report(entering, of[Room]){
      LineBreak()
      ruleReturn(ExecuteAction(examining, target = noun))
    }

  }
  object leaving extends Action(1)

  object going extends Action(1,"go", "travel", "walk", "run", "cartwheel") {

    override def implicitTargetSelector = nothing

    instead(going, nothing) Say "You don't have to go right now."

    /*
    UnderstandAlias("east", going, Direction.east)
    UnderstandAlias("e", going, Direction.east)
    UnderstandAlias("west", going, Direction.west)
    UnderstandAlias("w", going, Direction.west)
    UnderstandAlias("north", going, Direction.north)
    UnderstandAlias("n", going, Direction.north)
    UnderstandAlias("south", going, Direction.south)
    UnderstandAlias("s", going, Direction.south)
    UnderstandAlias("up", going, Direction.up)
    UnderstandAlias("u", going, Direction.up)
    UnderstandAlias("down", going, Direction.down)
    UnderstandAlias("d", going, Direction.down)
    */


    /*
        check going
        before leaving
        instead leaving
        check leaving
        report going
        report leaving
        inflict going
        inflict leaving
        after going
        run entering completely
        after leaving
     */

    check(going, of[Direction]){
      val d = noun[Direction]

      val connected = player.room.connections(d).isDefined

      // this is so dumb but maybe it will work?
      val leaveCtx = RuleContext(subject, Array(player.location), false, player.location)
      if !RunRule(leaveCtx, ruleSets(leaving).beforeRules) then stop
      if !RunRule(leaveCtx, ruleSets(leaving).insteadRules) then stop
      if !RunRule(leaveCtx, ruleSets(leaving).checkRules) then stop

      if (!connected) {
        Say(s"You can't go $d")
        stop
      }
      val destination = player.room.connections(d).get

      val enterCtx = RuleContext(subject, Array(destination), false,  player.location)
      if !RunRule(enterCtx, ruleSets(entering).beforeRules) then stop
      if !RunRule(enterCtx, ruleSets(entering).insteadRules) then stop
      if !RunRule(enterCtx, ruleSets(entering).checkRules) then stop

    }

    report(going, of[Direction]) {
      val d = noun[Direction]
      val room = player.room.connections(d).get

      Say(s"You went $d to $room.")
    }

    inflict(going, of[Direction]) {
      val d = noun[Direction]
      val room = player.room.connections(d).get

      val leaveCtx = RuleContext(subject, Array(player.location), false, player.location)
      RunRule(leaveCtx, ruleSets(leaving).reportRules)
      RunRule(leaveCtx, ruleSets(leaving).executeRules)

      blackboard = player.location
      player.Move(room)

      val enterCtx = RuleContext(subject, Array(player.location), false, player.location)
      RunRule(enterCtx, ruleSets(entering).reportRules)
      RunRule(enterCtx, ruleSets(entering).executeRules)
    }

    after(going, of[Direction]){
      val previousRoom = blackboard.asInstanceOf[Room]
      RunRule(RuleContext(subject, Array(previousRoom), false, player.location), ruleSets(leaving).afterRules)
      RunRule(RuleContext(subject, Array(player.location), false, player.location), ruleSets(entering).afterRules)
    }

  }

  object dropping extends Action(1,"drop", "abandon") {
    // if you don't provide an object, try dropping nothing
    override def implicitTargetSelector = nothing

    // for ambiguously dropping things, don't try dropping things that are not in your inventory
    disambiguationHint = {
      case t : Thing => t.location == player
      case _ => false
    }

    instead(dropping, nothing) {
      Say(randomly("You stop, drop, and roll.", "You fall to your knees for no reason.",
        """You throw yourself to the floor.
          |Like a cat licking itself nonchalantly after doing something embarrassing, you pretend you dropped a contact.""".stripMargin))
    }

    check(dropping, of[Thing]) {
      if(noun != nothing && noun[Thing].location != player) {
        Say("Can't drop what you don't have.")
        stop
      }
    }

    inflict(dropping, of[Thing]) {
      player.location holds noun[Thing]
    }

    report(dropping) Say randomly(s"$noun gently flutters to the ground.", s"Discarded, $noun crashes into earth.", s"You abandon $noun to its fate.")

  }



  object taking extends Action(1,"take", "get", "pick up", "g") {

    override def implicitTargetSelector = nothing

    // for ambiguously taking things, don't try to take items that are already in your inventory.
    disambiguationHint = {
      case t: Thing => t.location != player
      case _ => false
    }

    check(taking){
      if !subject.canAccess(noun, taking) then
        Say(noun iz "inaccessible") // maybe say why?
        stop
    }

    instead(taking, nothing) Say s"You wrap your arms around yourself, doesn't that feel nice?"

    instead(taking, noun[Thing].isComposite) {
      Say(s"You're going to have a difficult time removing $noun from ${noun[Thing].compositeObject}")
    }

    instead(taking, player holds noun?) {
      Say(s"You rummage around the items in your backpack, looking for $noun")
    }

    instead(taking, fixed) {
      Say(noun iz "shoracle")
    }

    report(taking) {
      Say(s"You slip $noun into your backpack.")
    }

    inflict(taking){
      player holds noun[Thing]
    }

    after(taking, of[RoomDescription]) {
      noun[RoomDescription].disturbed = true
    }

  }

  extension[T] (o : Option[T]) {
    inline def does(inline something : T => Unit): Unit = {
      if(o.isDefined)
        something(o.get)
    }
  }



  object examining extends Action(1,"examine", "x", "look", "l") {

    override def implicitTargetSelector = player.location

    report(examining) {
      Say(noun.description)
    }

    report(examining, ofDebug[Room]("report room examining")) {
      Title(player.location.name)
      Say(noun.description)
    }

    after(examining, ofDebug[Room]("after room examining")) {
      val r = noun[Room]
      var nonscenery = r.contents.filterNot(_ is scenery?).filterNot( _.isType[Person]).toSeq
      val people = r.contents.filter(_.isType[Person]).toSeq

      val roomDescribed = nonscenery.filter{ z =>
        val roomDesc = z.get[RoomDescription]
        roomDesc.isDefined && !roomDesc.get.disturbed
      }

      nonscenery = nonscenery.diff(roomDescribed)

      for(rd <- roomDescribed) {
        Say(rd.get[RoomDescription].get.desc)
      }

      val nonsceneryString = ListNamesNicely(nonscenery)
      nonsceneryString does { s =>
        Say("You can see " + s)
      }

      val peopleString = ListNamesNicely(people)
      peopleString does { s =>
        val be = if( people.length > 1) "are" else "is"
        Say(s + " " + be + " here.")
      }

      stop
    }

    after(examining, ofDebug[Container]("after examining container")) {

      val c = noun[Container]

      if(!c.automaticallyListContents)
        continue

      val roomDescribed = c.contents.filter { z =>
        val roomDesc = z.get[RoomDescription]
        roomDesc.isDefined && !roomDesc.get.disturbed
      }

      val nondescribed = c.contents.diff(roomDescribed)

      if (roomDescribed.nonEmpty) {
        if c.openable || !c.open then
            if c.open then Say(s"$noun is open") else Say(s"$noun is closed")

        if (c.open || c.transparent) {
          for (rd <- roomDescribed) {
            Say(rd.get[RoomDescription].get.desc)
          }
          if(nondescribed.nonEmpty){
            Say(s"${c.preposition} $noun you can also see " + ListNamesNicely(nondescribed).get)
          }
        }
      } else  {
        var response = ""
        if c.openable || !c.open then
          if c.open then response += s"$noun is open" else response += s"$noun is closed"

        if (c.open || c.transparent) {
            if (c.openable || !c.open) {
              response += ", "
              if (c.contents.isEmpty)
                response += s"nothing is ${c.preposition}"
              else
                response += s"${c.preposition} you can see ${c.ContentsString.get}"
            }
            else {
              if (c.contents.isEmpty)
                response += s"nothing is ${c.preposition} $noun"
              else
                response += s"${c.preposition} $noun you can see ${c.ContentsString.get}"
            }
          }

        Say(response)
      }
    }
  }



  object closing extends Action(1, "close", "shut") {

    check(closing, of[Container]){
      if !noun[Container].openable then
        Say(s"$noun can't be closed.")
        stop

      if !noun[Container].open then
        Say(s"$noun is already closed")
        stop
    }

    inflict(closing, of[Container]) {
        noun[Container].open = false
    }

    report(closing, of[Container]) Say s"You close $noun"
  }


  object opening extends Action(1, "open"){

    instead(opening, !of[Container]) Say s"It doesn't seem like you can open $noun"

    check(opening, of[Container]){

      if !noun[Container].openable then
        Say(s"$noun can't be opened.")
        stop

      if noun[Container].open then
        Say(s"$noun is already open")
        stop
    }

    inflict(opening, of[Container]){
        noun[Container].open = true
    }


    report(opening, of[Container]) {
      Say(s"You open $noun")
    }

    after(opening, of[Container]) {
      val c = noun[Container]

      if (c.contents.nonEmpty) {
        val roomDescribed = c.contents.filter { z =>
          val roomDesc = z.get[RoomDescription]
          roomDesc.isDefined && !roomDesc.get.disturbed
        }

        val nondescribed = c.contents.diff(roomDescribed)
        if (roomDescribed.nonEmpty) {

          for (rd <- roomDescribed) {
            Say(rd.get[RoomDescription].get.desc)
          }
          if (nondescribed.nonEmpty) {
            Say(s"${c.preposition} $noun you can also see " + ListNamesNicely(nondescribed.toSeq).get)
          }
        } else {
          Say(s"${c.preposition} $noun you can see ${c.ContentsString.get}")
        }
      }
    }
  }

  object exiting extends Action(0,"exit") {
    inflict(exiting) {
      Say(s"Goodbye ${player.name}")
      exit = true
    }
  }

  object takingInventory extends Action(0,"inventory", "i") {
    inflict(takingInventory) {
      val s = "In your possessionary you have " + player.ContentsString.getOrElse("nothing")
      Say(s)
    }
  }


  object saving extends Action(0, "save"){
    inflict(saving){
      Saving.SaveWorld()
    }
  }


  object loading extends Action(0, "load") {
    inflict(loading) {
      Saving.LoadWorld()
      LineBreak()
      ExecuteAction(examining, target = player.location)
    }
  }


  object debugging extends Action(0, "debug") {

    report(debugging){

      val visibleSet = ZextObject.allObjects.filter(player.canSee(_, debugging))
      val accessibleSet = ZextObject.allObjects.filter(player.canAccess(_, debugging))

      Say("visible set: " + visibleSet.toString)
      Say("accessible set: " + accessibleSet.toString)
    }
  }

  object putting extends Action(2,"put", "insert", "place") {

    before(putting, secondNoun holds noun?) {
      Say(s"$noun is already ${secondNoun[Container].preposition} $secondNoun")
      stop
    }

    before(putting, !player holds noun?, !secondNoun holds noun?){
      if( ExecuteAction(taking, target = noun, silent = Some(true))) {
        Say(s"(First taking $noun)")
      }
    }

    before(putting, !secondNoun[Container].open) {
      if (ExecuteAction(opening, target = secondNoun, silent = Some(true))) {
        Say(s"(First opening $secondNoun)")
      }
    }

    instead(putting, !player holds noun?) Say s"You need to pick up $noun before putting it somewhere."
    instead(putting, !player holds noun?, fixed) Say s"$noun looks happy where it is." // containment takes precedence over properties.
    instead(putting, anything -> !of[Container] ) Say s"I don't think $secondNoun can hold $noun"

    check(putting, anything -> ofDebug[Container]("anything -> container")){

      if(noun == secondNoun){
        Say(s"Stepping into the fourth dimension, you put $noun into itself.")
        stop
      }

      if (!secondNoun[Container].open) {
        Say(s"Grandpa's ghost isn't around at the moment, so you'll have to open $secondNoun before you put $noun inside it.")
        stop
      }

      if(!subject.canAccess(secondNoun, putting)) {
        Say(s"$secondNoun is inaccessible")
        stop
      }
    }

    inflict(putting, anything -> of[ZContainer]) {
        noun[Thing] inside secondNoun[ZContainer]
    }

    report(putting, anything -> of[Container]) Say s"You put $noun into $secondNoun"



  }
}

