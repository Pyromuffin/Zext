package Game

import Game.JunimoGame.*
import Zext.*
import Zext.Parser.Understand
import Zext.exports.*

object talkingTo extends Action(1, "talk", "talk to", "speak to", "speak with", "yell at", "t", "ask") {

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
      s"$noun's silence speaks volumes'",
      s"$noun will take their secrets to the grave",
      s"$noun has no mouth, but must scream" ))
  }

}


object talkingAbout extends Action(2, "talk about") {

  /*
  Understand(this, verbs *)
  if (targets == 1) {
    UnderstandAlias(verbs, this, reflexively, null)
  }
  */

  report(talkingAbout, of[Subject], ofSecond[Person]) Say s"You broach the subject of $noun with $secondNoun, but they have nothing interesting to say."

//  inflict(talkingAbout, reflexively.asSecondNoun) {

  //}

}


  //instead( talking about bullshit subject)
  //Say(Randomly(s"They appear to ignore you, but you can see $noun deeply thinking about $topic", s"They don't talk about $topic 'round these parts no more", s"$noun will never speak of $topic again. Not since then.", s"$noun can't believe you brought up $topic, are you out of your goddamn mind????????"))


case class Subject(names : String*) extends ZextObject {
  global = true
  name = names(0)
  aliases.addAll(names.slice(1, names.length))
}

object Subjects {

  val weather = Subject("weather", "the weather", "rain", "snow", "heat", "heatwave", "wind")

}


class Person(using Container) extends thing {

  var affection : Int = 0
  var giftGivenToday = false //should we just literally copy paste their gift reception strings for random objects? i think it would be funny to have something in the game that does that.
  var talkedToToday = false
  proper = true
}


object PersonHolder extends Room {
  name = "Prison" // needs a name or ambiguity will occur with things named ""
  // i guess we can't construct people without having a room for them to start in.

  object MayorLewis extends Person {
    transferTo(Town)
    name = "Mayor Lewis"
    description = "Somewhere between dapper and disheveled, Lewis is the mayor of Pelican Town. Seemingly always busy, but never doing anything, mayoral duties are mysterious indeed. " +
      "Mayor Lewis has too many things to do, and too much time to do them. Regardless of his responsibilities (or lack thereof), he always has a moment to spare for you."


    report(talkingTo, this) {
      Say(s"$noun removes his cap before speaking to you and replies \"Hey $farmer, I hope everything is going well on $farm today!\"")
    }

    report(talkingTo, this, talkedToToday){
      Say(s"$noun dabs the sweat from his brow with a polka-dot handkerchief and replies \"Fancy seeing you again today, $farmer.\"")
    }

    val handkerchief = ~"An antique green and blue polka-dot handkerchief is dangling from Mayor Lewis's front pocket." composes this aka "kerchief" aka "hanky" aka "cloth"
    val cap = ~"An integral part of The Lewis Attire, the wrinkled leather cap has seen better days" composes this aka "hat"
    val sweat = ~"There are reservoirs of fluid somewhere inside Mayor Lewis, or so one might think from his liberal handkerchief use" composes this

    this aka "Lewis"
  }

  object Linus extends Person {
    transferTo(Path)
    name = "Linus"
    description = "Without Linus, Stardew Valley Greater Metropolitan Area would be NOTHING " +
      "Supposedly, Linus is usually alone. But you've never see him alone, not with you here"


    report(smelling, this) {
      Say(Randomly( s"$noun smells like elderberries and the call of the wild, a novel by Jack London", s"$noun smells like David Thoreau at Walden Pond", s"$noun smells like seasonally appropriate wild produce", s"You detect the aroma of anti capitalism with hints of red currant"))
    }

    report(tasting, this) {
      //if friendship > x
      Say(s"$noun blushes and looks away. You tasted raspberries.")
      //elif frienship x
      Say (s"You lick the berry juices off of $noun's fingers. $noun holds still and pretends he understands this normal townsfolk social convention.")
    }

    report(talkingTo, this) {
      Say(s"$noun smiles sadly at you. \"Hi $farmer. Thanks being the one and only guy in town who doesn't be destroying my tent or submitting me to the town authorities for rummaging in trashcans or whatever... ")
    }

    report(talkingTo, this, this.talkedToToday) {
      Say(Randomly(s"$noun stands up out of his foraging squat and carelessly sweeps a wild white strand of hair off of his sweaty forehead.", s"$noun smiles sadly at you.", s"$noun gestures to the thornless clearing in the bushes, offering you the prime foraging spot by his side"))
    } //lol we are so unoriginal with our sweaty brows, huh?

    val mustache = ~"A fine, full white mustache, proving you don't need beard oil to be beautiful. Ends drooping sadly or tips a-tingle with joy, a more expressive mustache you have never seen." composes this aka "beard" aka "face"//do descriptions change based on mood/situation/day/??
    val tunic = ~"Feathers, leaves, and twigs come together in a surprisingly fashionable yellow tunic" composes this aka "coat" aka "shirt" aka "dress"
  }


}


