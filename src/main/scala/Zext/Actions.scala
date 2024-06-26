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

import scala.collection.mutable.ArrayBuffer


object Actions {

  val commandAliases = mutable.HashMap[String, Command]()
  val allActions = ArrayBuffer[Action]()

  def UnderstandAlias(str: String, action: Action, zextObject1: ZextObject = null, zextObject2: ZextObject = null): Unit = {
    commandAliases.addOne(str -> Command(action, Option(zextObject1), Option(zextObject2)))
  }


  def UnderstandAlias(strs: Seq[String], action: Action, zextObject1: ZextObject, zextObject2: ZextObject) : Unit = {
    commandAliases.addAll(strs.map( _ -> Command(action, Option(zextObject1), Option(zextObject2))))
  }

  object being extends Action(1)


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
      result(execute(examining, noun))
    }

  }
  object leaving extends Action(1)

  object going extends Action(1,"go", "travel", "walk", "run", "cartwheel") {

    instead(going, reflexively) Say "You don't have to go right now."

    UnderstandAlias("east", going, Direction.east)
    UnderstandAlias("e", going, Direction.east)
    UnderstandAlias("west", going, Direction.west)
    UnderstandAlias("w", going, Direction.west)
    UnderstandAlias("north", going, Direction.north)
    UnderstandAlias("n", going, Direction.north)
    UnderstandAlias("south", going, Direction.south)
    UnderstandAlias("s", going, Direction.south)


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
      val connected = currentLocation.connections.contains(d)

      // this is so dumb but maybe it will work?
      val leaveCtx = RuleContext(Some(currentLocation), None, false)
      if !RunRule(leaveCtx, ruleSets(leaving).beforeRules) then stop
      if !RunRule(leaveCtx, ruleSets(leaving).insteadRules) then stop
      if !RunRule(leaveCtx, ruleSets(leaving).checkRules) then stop

      if (!connected) {
        Say(s"You can't go $d")
        stop
      }
      val destination = currentLocation.connections(d)

      val enterCtx = RuleContext(Some(destination), None, false)
      if !RunRule(enterCtx, ruleSets(entering).beforeRules) then stop
      if !RunRule(enterCtx, ruleSets(entering).insteadRules) then stop
      if !RunRule(enterCtx, ruleSets(entering).checkRules) then stop

    }

    report(going, of[Direction]) {
      val d = noun[Direction]
      val room = currentLocation.connections(d)

      Say(s"You went $d to $room.")
    }

    inflict(going, of[Direction]) {
      val d = noun[Direction]
      val room = currentLocation.connections(d)

      val leaveCtx = RuleContext(Some(currentLocation), None, false)
      RunRule(leaveCtx, ruleSets(leaving).reportRules)
      RunRule(leaveCtx, ruleSets(leaving).executeRules)

      blackboard = currentLocation
      player.Move(room)

      val enterCtx = RuleContext(Some(currentLocation), None, false)
      RunRule(enterCtx, ruleSets(entering).reportRules)
      RunRule(enterCtx, ruleSets(entering).executeRules)
    }

    after(going, of[Direction]){
      val previousRoom = blackboard.asInstanceOf[Room]
      RunRule(RuleContext(Some(previousRoom), None, false), ruleSets(leaving).afterRules)
      RunRule(RuleContext(Some(currentLocation), None, false), ruleSets(entering).afterRules)
    }

  }

  object dropping extends Action(1,"drop", "abandon") {

    // for ambiguously dropping things, don't try dropping things that are not in your inventory
    disambiguationHint = { z =>
      z.parentContainer == player
    }

    inflict(dropping, reflexively) {

      Say(randomly("You stop, drop, and roll.", "You fall to your knees for no reason.",
        """You throw yourself to the floor.
          |Like a cat licking itself nonchalantly after doing something embarrassing, you pretend you dropped a contact.""".stripMargin))
    }

    check(dropping) {
      if( noun != reflexively && noun.parentContainer != player) {
        Say("Can't drop what you don't have.")
        stop
      }
    }

    inflict(dropping) {
        noun.transferTo(currentLocation)
    }

    report(dropping) Say randomly(s"$noun gently flutters to the ground.", s"Discarded, $noun crashes into earth.", s"You abandon $noun to its fate.")

  }



  object taking extends Action(1,"take", "get", "pick up", "g") {

    // for ambiguously taking things, don't try to take items that are already in your inventory.
    disambiguationHint = { z =>
      z.parentContainer != player
    }

    check(taking){
      if !noun.isAccessible(currentLocation) then
        Say(noun is "inaccessible") // maybe say why?
        stop
    }

    instead(taking, reflexively) Say s"You wrap your arms around yourself, doesn't that feel nice?"

    instead(taking, noun.isComposite) {
      Say(s"You're going to have a difficult time removing $noun from ${noun.compositeObject}")
    }

    instead(taking, player has noun) {
      Say(s"You rummage around the items in your backpack, looking for $noun")
    }

    instead(taking, fixed) {
      Say(noun is "shoracle")
    }

    report(taking) {
      Say(s"You slip $noun into your backpack.")
    }

    inflict(taking) {
      noun.transferTo(player)
    }
  }

  extension[T] (o : Option[T]) {
    def does( something : T => Unit): Unit = {
      if(o.isDefined)
        something(o.get)
    }
  }


  object examining extends Action(1,"examine", "x", "look", "l") {

    instead(examining, reflexively) {
      execute(examining, currentLocation)
    }

    report(examining) {
      Say({noun.description})
    }

    report(examining, of[Room]) {
      Title(currentLocation.name)
      Say({noun.description})
    }

    after(examining, of[Room]) {
      val r = noun[Room]
      val nonscenery = r.contents.filterNot(_.properties.contains(scenery)).filterNot( _.isType[Person]).toSeq
      val people = r.contents.filter(_.isType[Person]).toSeq

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

    after(examining, of[Container]) {

      val c = noun[Container]

      if(!c.automaticallyListContents)
        continue

      var response = ""
      if c.open then response +=s"$noun is open" else response += s"$noun is closed"

      if (c.open  || c.transparent) {
        response += ", "
        if(c.contents.isEmpty)
          response += "nothing is inside"
        else
          response += s"inside you can see ${c.ContentsString.get}"
      }

      Say(response)
    }
  }

  object closing extends Action(1, "close", "shut") {

    check(closing, of[Container]){
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

    check(opening, of[Container]){
      if noun[Container].open then
        Say(s"$noun is already open")
        stop
    }

    inflict(opening, of[Container]){
        noun[Container].open = true
    }

    report(opening, of[Container]) {
      if !noun[Container].transparent && noun[Container].contents.nonEmpty then
        Say(s"You open $noun, inside you can see ${noun[Container].ContentsString.get}")
      else
        Say(s"You open $noun")
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
      execute(examining, reflexively)
    }
  }


  object debugging extends Action(0, "debug") {
    report(debugging) Say FindVisibleSet().toString()
  }

  object putting extends Action(2,"put", "insert", "place") {

    before(putting, player lacks noun){
      if( execute(taking, noun, silent = true) ){
        Say(s"(First taking $noun)")
      }
    }

    before(putting, !secondNoun[Container].open) {
      if (execute(opening, secondNoun, silent = true)) {
        Say(s"(First opening $secondNoun)")
      }
    }

    instead(putting, player lacks noun) Say s"You need to pick up $noun before putting it somewhere."
    instead(putting, player lacks noun, fixed) Say s"$noun looks happy where it is." // containment takes precedence over properties.
    instead(putting, !ofSecond[Container]) Say s"I don't think $secondNoun can hold $noun"

    check(putting, ofSecond[Container]){

      if(noun == secondNoun){
        Say(s"Stepping into the fourth dimension, you put $noun into itself.")
        stop
      }

      if (!secondNoun[Container].open) {
        Say(s"Grandpa's ghost isn't around at the moment, so you'll have to open $secondNoun before you put $noun inside it.")
        stop
      }

      if(!secondNoun.isAccessible(currentLocation)) {
        Say(s"$secondNoun is inaccessible")
        stop
      }
    }

    inflict(putting, ofSecond[Container]) {
        noun transferTo secondNoun[Container]
    }

    report(putting, ofSecond[Container]) Say s"You put $noun into $secondNoun"



  }
}

