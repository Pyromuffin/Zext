package Game

import Zext.Parser.*
import Zext.*
import Zext.Interpreter.*
import Zext.thing.*
import Zext.Direction.*

import scala.language.postfixOps

implicit class TernaryExtension(that : Boolean) {

  def ?(str : StringExpression) : StringExpression = {
    if(that) str else ""
  }

}


object Kitchen extends Room {

  var lockdown = true
  description = "I'm in the kitchen now. " +
  lockdown ? { s"A pleasant yet urgent announcement rings $b'Laserite Core criticality event detected: apartment lockdown in effect.'$b " +
    "The doors leading to other parts of the apartment are shielded with comically elaborate folding door layers." } +
    "\n" +
    !visited ? "The refrigerator remains unmentioned. "+
    s"$table obnoxiously takes up most of the useful space in here. " +
    "There's a drawer under a cabinet opposite the table."

  val table = ~"centaur cosplay"

  Bathroom connect west

}

object Bathroom extends Room {

}