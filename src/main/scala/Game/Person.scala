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
      s"$noun's silence speaks volumes",
      s"$noun will take their secrets to the grave",
      s"$noun has no mouth, but must scream" ))
  }

}

object giving extends Action(2, "give", "bequeath", "offer") {


  // this kinda sucks, but whatever
  instead(giving, of[Thing], ofSecond[Person], player lacks noun) Say s"You have a lot to give, but not $noun"
  instead(giving, of[Person], ofSecond[Thing], player lacks secondNoun) Say s"You have a lot to give, but not $secondNoun)"

  inflict(giving, ofSecond[Person]) {
    val p = secondNoun[Person]

    if(p.giftGivenToday){
      Say(s"You've already given $secondNoun a gift today, more gifts would cause an embarrassing inventory problem")
      stop
    }

    noun.transferTo(p)
    p.affection += p.getApprovalOf(noun)
    p.giftGivenToday = true
    true
  }

  instead(giving, of[Person]) {
    execute(giving, secondNoun, noun)
  }

  report(giving, ofSecond[Person]){
    val p = secondNoun[Person]
    val approval = p.getApprovalOf(noun)
    if( approval == 0){
      Say(s"upon receiving $noun, $p weeps neutrally")
    } else if (approval == 1){
      Say(s"upon receiving $noun, $p glows an eerie shade of thankful")
    } else if ( approval == -1) {
      Say(s"upon receiving $noun, $p erupts with sparks of displeasure")
    } else
    Say(s"upon receiving $noun, $p emits harmless rays of ${Randomly("eye searing", "hunger inducing", "ghostly")} radiation")
  }

}

object talkingAbout extends Action(2, "talk about") {

  /*
  Understand(this, verbs *)
  if (targets == 1) {
    UnderstandAlias(verbs, this, reflexively, null)
  }
  */

  //instead( talking about bullshit subject)
  report(talkingAbout, of[Subject], ofSecond[Person])   Say(Randomly(s"You broach the subject of $noun with $secondNoun, but they have nothing interesting to say.", s"They appear to ignore you, but you can see $secondNoun deeply thinking about $noun", s"They don't talk about $noun 'round these parts no more", s"$secondNoun will never speak of $noun again. Not since then.", s"$secondNoun can't believe you brought up $noun, are you out of your goddamn mind????????"))
  //Say s"You broach the subject of $noun with $secondNoun, but they have nothing interesting to say."

//  inflict(talkingAbout, reflexively.asSecondNoun) {

  //}

}


case class Subject(names : String*) extends ZextObject {
  global = true
  name = names(0)
  aliases.addAll(names.slice(1, names.length))
}

object Subject {
  val weather = Subject("weather", "the weather", "rain", "snow", "heat", "heatwave", "wind", "cold")
  val friendship = Subject("me", "us", "friendship", "you", "relationship")
}



class Person(using Container) extends Thing with Container {

  var affection : Int = 0
  var giftGivenToday = false //should we just literally copy paste their gift reception strings for random objects? i think it would be funny to have something in the game that does that.
  var talkedToToday = false
  proper = true

  // uhh should people be containers?
  automaticallyListContents = false
  open = false
  transparent = false

  // override this
  def getApprovalOf(z : ZextObject): Int = { // should be thing or ??
     0
  }

}


object PersonHolder extends Room {
  name = "Prison" // needs a name or ambiguity will occur with things named ""
  // i guess we can't construct people without having a room for them to start in.


  instead(opening, of[Person]) Say(Randomly( "Harvey would be better suited to doing that.", "It's too early in their lifecycle for vivisection")
  instead(taking, of[Person]){
    val love = noun[Person].affection
    if (love == 1) {
      Say(s"Maybe once you get to know $noun better")
    }
    else{ //if affection > x
      Say(Randomly(s"You give $noun a helluva bear hug",s"You pick up $noun and spin them around. You are both giggling."))
    }
    //if it's like 1-3
    Say(Randomly(s"You go in for a high five while $noun tries a fist bump, then you both back off and awkwardly shake hands", s"You go in for a fist bump but $noun thinks you're going for a one armed bro hug, so you end up accidentally punching them in the gut", s"You try to give $noun a hug but they politely sidestep"
    ))

  }
  


  after(examining, of[Person]) {
    val love = noun[Person].affection
    Say(s"Friendship Rating: $love")

    val phrase = love match{
      case 0 => "You mean nothing to them."
      case 1 => s"$noun wouldn't cry if you died."
      case 3 => s"$noun knows you exist. They vaguely prefer it that way you think."
      case 2 => s"There aren't enough people in Pelican Town for $noun to pretend they don't know you"
      case 4 => s"$noun is down to 'yes and?' you"
      case 5 => s"You have given $noun sufficient goods of value now to earn their trust"
      case 8 => s"$noun always saves the last bite for you"
      case 9 => s"$noun would give you a kidney if they had to."
      case 10 => s"$noun would gladly give you a kidney, even if there was nothing wrong with yours"
      case 11 => s"$noun hasn't stopped thinking of you for days. You are the wind beneath their wings, the devil on their shoulder, the voice inside their head"
    }

    Say(phrase)
    true


  }

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


    // linus loves everything
    override def getApprovalOf(z: ZextObject) = 1

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


