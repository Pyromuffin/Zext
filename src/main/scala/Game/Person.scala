package Game

import Game.JunimoGame.*
import Zext.*
import Zext.exports.*

object talkingTo extends Action(1, "talk", "talk to", "speak to", "speak with", "yell at") {

  after(talkingTo, of[Person]){
    noun[Person].talkedToToday = true
    true
  }

  report(talkingTo, of[Person]) {
    Say( Randomly(s"$noun ignores you.", s"$noun is too busy to respond", s"$noun has better things to do than talk to you.") )
  }

  report(talkingTo) {
    Say(Randomly(s"$noun would be an engaging conversation partner, if only $noun could talk",
      s"$noun remains silent",
      s"$noun will take their secrets to the grave",
      s"$noun has no mouth but must scream" ))
  }

}


class Subject extends ZextObject {

}


class Person(using Container) extends thing {

  var affection : Int = 0
  var giftGivenToday = false
  var talkedToToday = false
  proper = true
}


object PersonHolder extends Room {
  // i guess we can't construct people without having a room for them to start in.

  object MayorLewis extends Person {
    transferTo(Town)
    name = "Mayor Lewis"
    description = "Somewhere between dapper and disheveled, Lewis is the mayor of Pelican Town. Seemingly always busy, but never doing anything, mayoral duties are mysterious indeed. " +
      "Mayor Lewis has too many things to do, and too much time to do them. Regardless of his responsibilities (or lack thereof), he always has a moment to spare for you."


    report(talkingTo, this) {
      Say(s"$noun removes his cap before speaking to you and replies \"Hey $farmer, I hope everything is going well on $farm today!\"")
    }

    report(talkingTo, this, this.talkedToToday){
      Say(s"$noun dabs the sweat from his brow with a polka-dot handkerchief and replies \"Fancy seeing you again today, $farmer.\"")
    }

    val handkerchief = ~"An antique green and blue polka-dot handkerchief is dangling from Mayor Lewis's front pocket." composes this
    val cap = ~"An integral part of The Lewis Attire, the wrinkled leather cap has seen better days" composes this aka "hat"

  }


}


