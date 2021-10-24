package Zext



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


object Interpreter{
  def Say(str: StringExpression): Unit = {
    println(str.toString.capitalize)
  }
}
import Interpreter.*
import Zext.Rule.{ActionRuleSet, ruleSets}
import org.reflections.scanners.SubTypesScanner

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.*


object Parser extends RegexParsers{

  def VerbParser(action: Action) : Parser[Action] = {
    val words = action.verb.toList
    val parsers = words.map {
      _.r ^^ { s => action }
    }
    val parser = parsers.reduce( _ | _ )
    parser
  }

  def RegisterVerbs() = {
    import org.reflections.Reflections
    val actionsReflections = new Reflections("Zext")
    val actionsClasses = actionsReflections.getSubTypesOf(classOf[Action])
    actionsClasses.forEach{a =>
      val constructor = a.getDeclaredConstructor()
      constructor.setAccessible(true)
      constructor.newInstance().Register()
    }

  }


  def main(args: Array[String]): Unit = {
    RegisterVerbs()
    val actions = ruleSets.keys
    val parsers = actions.map(VerbParser(_))
    val parser = parsers.reduce( _ | _ )
    val result = parse(parser, "take")
    println(result.get)
  }



    case class WordFreq(word: String, count: Int) {
    override def toString = s"Word <$word> occurs with frequency $count"
  }

  class SimpleParser extends RegexParsers {
    def word: Parser[String]   = """[a-z]+""".r       ^^ { _.toString }
    def number: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }
    def freq: Parser[WordFreq] = word ~ number        ^^ { case wd ~ fr => WordFreq(wd,fr) }
  }

  object TestSimpleParser extends SimpleParser {
    def main(args: Array[String]) = {
      parse(freq, "johnny 121") match {
        case Success(matched,_) => println(matched)
        case Failure(msg,_) => println(s"FAILURE: $msg")
        case Error(msg,_) => println(s"ERROR: $msg")
      }
    }
  }

}