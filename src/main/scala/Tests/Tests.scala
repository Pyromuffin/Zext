package Tests

import Zext.*
import Zext.EverythingParser.ParseResult
import Zext.Parser.{Command, Disambiguate}
import Zext.exports.*


object TestPlayer extends PlayerClass(TestRoom) {
  override val name = "Test Player"
  override val description = "Detestable"
}

case class wet(var wetness : Int) extends Property

object drying extends Action(1, "dry") {

  check(drying, of[wet]) {
    if(noun[wet].wetness == 0){
      Say(s"$noun is bone dry")
      stop
    }
  }

  inflict(drying, of[wet]) {
    noun[wet].wetness = scala.math.max(noun[wet].wetness -1, 0)
  }

  report(drying) Say s"You dry $noun"
}

object blessing extends Action(-1, "bless") {

  report(blessing) {
    val nouns = GetNouns()
    Say("You bless " + nouns.mkString(", "))
  }

}

object SecondRoom extends Room {
  override val name = "Second Room"
  override val description = "Even more tests!"

  val tomato = ~"tomato"
  val horse = ~"horse"
  val moose = ~"moostical"
  val juice = ~"juice"

  val hat = ~"innocent"
  val hat_hat = ~"troublemaker"
  val bucket = new Thing {
    this is wet(2)

    override val name = str {
      if(this[wet].wetness > 0)
        "bucket"
      else
        "dry bucket"
    }

    override val description: StringExpression = str {
      if(this[wet].wetness > 0)
        "the bucket has some water in it."
      else
        "the bucket is dry."
    }
  }


  this southward TestRoom
}

object nicknaming extends CustomAction(2, "nickname") {

  override def intercept(rawInput: String, parseResult: ParseResult): Command = {
    val name = parseResult.nounStrings(1)
    val target = Disambiguate(parseResult.nouns(0)).asInstanceOf[ZextObject] // this does not respect visibility or the other normal command rules.
    target.aliases.addOne(name)
    blackboard = name
    Command(nicknaming, Array(target))
  }

  report(nicknaming) {
    Say(s"$noun shall now be known as $blackboard")
  }
}

object TestRoom extends Room  {

  override val name = "Test Room"
  override val description = ""

  val pebble = ~"macrosand"
  val box = Box("Test box")

  val table = new Supporter("Test table") {
    val stick =  ~"sticky"
  }

  val hat = ~"innocent"

  val gum = "A wad of gum is stuck to the bottom of the table" initially
    "A dried out piece of chewing gum" inside table amount some

  report(taking, gum) Say "You peel the gum from the table."
  report(putting, gum -> table) Say "You try to stick it back to the underside, but the gum has lost its adhesion. You just leave it on top."
}




object Tests extends App {

  scala.util.Random.setSeed(123456789)

  Parser.InitTests(TestPlayer, "Tests")

  var failureCount = 0

  if !Parser.RunTest("Invalid input", Array("Hello"), Array("Input couldn't be interpreted as a command.")) then failureCount += 1
  if !Parser.RunTest("Examine self",  Array("x self"), Array("Detestable.")) then failureCount += 1
  if !Parser.RunTest("box opening",  Array("open box"), Array("You open the box.")) then failureCount += 1
  if !Parser.RunTest("table opening",  Array("open table"), Array("The table can't be opened.")) then failureCount += 1
  if !Parser.RunTest("box closing",  Array("close box"), Array("You close the box.")) then failureCount += 1
  if !Parser.RunTest("table closing",  Array("close table"), Array("The table can't be closed.")) then failureCount += 1
  if !Parser.RunTest("pebble box putting",  Array("put pebble in box"), Array("(First taking the pebble).", "(First opening the box).", "You put the pebble into the box.")) then failureCount += 1
  if !Parser.RunTest("pebble table putting",  Array("put pebble on the table"), Array("(First taking the pebble).", "You put the pebble on to the table.")) then failureCount += 1
  if !Parser.RunTest("already contained putting",  Array("put pebble on the table"), Array("The pebble is already on the table.")) then failureCount += 1
  if !Parser.RunTest("table contents listing",  Array("x table"), Array("Test table.", "A wad of gum is stuck to the bottom of the table.", "On the table you can also see a pebble and a stick.")) then failureCount += 1
  if !Parser.RunTest("gum initial description",  Array("take gum"), Array("You peel the gum from the table.")) then failureCount += 1
  if !Parser.RunTest("gum putting", Array("put gum on to table"), Array("You try to stick it back to the underside, but the gum has lost its adhesion. You just leave it on top.")) then failureCount += 1
  if !Parser.RunTest("gum disturbed", Array("x table"), Array("Test table.", "On the table you can see some gum, a pebble, and a stick.")) then failureCount += 1
  if !Parser.RunTest("stick taking",  Array("take stick"), Array("You take the stick off of the table.")) then failureCount += 1
  if !Parser.RunTest("inventory listing",  Array("i"), Array("In your possessionary you have a stick.")) then failureCount += 1
  if !Parser.RunTest("stick dropping",  Array("drop stick"), Array("Discarded, the stick crashes into earth.")) then failureCount += 1
  if !Parser.RunTest("stick dropping failure",  Array("drop stick"), Array("Can't drop what you don't have.")) then failureCount += 1
  if !Parser.RunTest("empty inventory",  Array("i"), Array("In your possessionary you have nothing.")) then failureCount += 1
  if !Parser.RunTest("drop nothing",  Array("drop"), Array("You stop, drop, and roll.")) then failureCount += 1
  if !Parser.RunTest("take hat",  Array("take hat"), Array("You slip the hat into your backpack.")) then failureCount += 1
  if !Parser.RunTest("going north",  Array("north"), Array("You went north to the Second Room.", "Even more tests!", "You can see a bucket, a hat, a hat hat, a horse, a juice, a moose, and a tomato.")) then failureCount += 1
  if !Parser.RunTest("non-ambiguous hat drop",  Array("drop hat"), Array("You abandon the hat to its fate.")) then failureCount += 1
  if !Parser.RunTest("dry bucket 1", Array("dry bucket"), Array("You dry the bucket.")) then failureCount += 1
  if !Parser.RunTest("dry bucket 2", Array("dry bucket"), Array("You dry the bucket.")) then failureCount += 1
  if !Parser.RunTest("dry dry bucket", Array("dry bucket"), Array("The dry bucket is bone dry.")) then failureCount += 1
  if !Parser.RunTest("dry dry bucket2", Array("dry dry bucket"), Array("The dry bucket is bone dry.")) then failureCount += 1
  if !Parser.RunTest("bless dry bucket", Array("bless dry bucket"), Array("You bless the dry bucket.")) then failureCount += 1
  if !Parser.RunTest("bless dry bucket and invisible thing", Array("bless dry bucket and gum"), Array("Input couldn't be interpreted as a command.")) then failureCount += 1
  if !Parser.RunTest("bless dry bucket and hat hat", Array("bless dry bucket and hat hat"), Array("You bless the dry bucket, the hat hat.")) then failureCount += 1
  if !Parser.RunTest("bless dry bucket and other stuff", Array("bless dry bucket and hat hat and horse and tomato and juice and moose"), Array("You bless the dry bucket, the hat hat, the horse, the tomato, the juice, the moose.")) then failureCount += 1
  if !Parser.RunTest("take too much stuff", Array("take moose and juice"), Array("Input couldn't be interpreted as a command.")) then failureCount += 1
  if !Parser.RunTest("custom nickname", Array("nickname moose Kelly"), Array("The moose shall now be known as Kelly.")) then failureCount += 1
  if !Parser.RunTest("use nickname", Array("x kelly"), Array("Moostical.")) then failureCount += 1


  println("=================================")
  if(failureCount == 0){
    println("All tests succeeded")  
  } else {
    Console.err.println(s"Failure Count: $failureCount")
  }
  
  
  


}