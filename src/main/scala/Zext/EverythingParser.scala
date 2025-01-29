package Zext

import Zext.Parser.{BuildUnderstandables, Parsable, ParsableType, understandables}
import fastparse.*
import fastparse.NoWhitespace.*
import fastparse.Parsed.{Failure, Success}

import java.lang.System.nanoTime

object EverythingParser {


   inline def time[T](name: String)(inline block : => T): T = {
    val t0 = nanoTime()
    val result = block
    val t1 = nanoTime()
    val elapsed = {
      (t1 - t0) / 1.0e6
    }
    if(false) println(s"$name: Elapsed time: $elapsed ms")
    result
  }



  def PossibleWord(str: String, parsables: Seq[Parsable[ParsableType]]): Option[(String, Array[ParsableType])] = {
    // first check if word conditions are applicable, if not splode

    val possibles = parsables.filter(_.possible)

    if (possibles.isEmpty)
      return Option.empty

    val maxPrecedence = possibles.map(_.precedence).max
    val maxPrecedenceParsables = possibles.filter(_.precedence == maxPrecedence)

    val maxSpecificity = maxPrecedenceParsables.map(_.specificity).max
    val bestParsables = maxPrecedenceParsables.filter(_.specificity == maxSpecificity)

    val parser = (str.toLowerCase, bestParsables.map(_.target).toArray)
    Some(parser)
  }


  type ParseResultType = Option[(Array[ParsableType], Option[Array[ParsableType]], Option[Array[ParsableType]])]


  def parse(input: String) = {

    val (allWords, allParsables) = time("build understandables") {
      BuildUnderstandables()
      understandables.map(PossibleWord).filter(_.isDefined).map(_.get).toArray.sortBy(_._1.length).reverse.unzip
    }

    def space[$: P] = P( CharIn(" \t\n\r").rep(1))
    def anySpace[$: P] = P(CharIn(" \t\n\r").rep)
    def prepositionParser[$: P] = StringIn("on top of", "about", "on to", "with", "from", "into", "in", "at", "to", "on")
    def ignored[$: P] = StringIn("some", "the", "a")

    def indexResultParserNext[$: P](t : (String, Int)) = () => P( (ignored ~ space).? ~ t._1 ~ &(space) ).map(_ => t._2)
    def indexResultParser[$: P](t : (String, Int)) = () => P( (ignored ~ space).? ~ t._1).map(_ => t._2)

    val allWordsWithIndices = allWords.zipWithIndex
    def parseEverything[$: P] = allWordsWithIndices.map(indexResultParser).foldLeft(Fail.map(_=> -1))((l, r) => l | r())
    def parseEverythingRequireNext[$: P] = allWordsWithIndices.map(indexResultParserNext).foldLeft(Fail.map(_=> -1))( (l, r) => l | r())

    def indexResultParserNextNext[ $: P](t : (String, Int)) = () => P(     (ignored ~ space).? ~ t._1 ~ &(space ~ parseEverythingRequireNext ~ &(spaceOrEnd))     ).map(_ => t._2)
    def parseEverythingRequireNextNext[$: P] = allWordsWithIndices.map(indexResultParserNextNext).foldLeft(Fail.map(_=> -1))( (l, r) => l | r())
    def spaceOrEnd[$: P] = P(space | End)


    def command1Parser[$: P] = P( anySpace ~ parseEverything ~ (space ~ parseEverything ~ &(spaceOrEnd)).? ~ ( (space ~ prepositionParser).? ~ (space ~ parseEverything ~ &(spaceOrEnd)) ).? ~ End)
    def command2Parser[$: P] = P( anySpace ~ parseEverythingRequireNext ~ (space ~ parseEverything ~ &(spaceOrEnd)) ~ ( (space ~ prepositionParser).? ~ (space ~ parseEverything ~ &(spaceOrEnd)) ).? ~ End)
    def command3Parser[$: P] = P( anySpace ~ parseEverythingRequireNextNext ~ (space ~ parseEverythingRequireNext ~ &(spaceOrEnd)) ~ ( (space ~ prepositionParser).? ~ (space ~ parseEverything ~ &(spaceOrEnd)) ) ~ End)


    val result1 = time("parse1") {
      fastparse.parse(input, implicit p => command1Parser)
      match {
        case s: Success[(Int, Option[Int], Option[Int])] => Some(allParsables(s.value._1), s.value._2.map( allParsables(_)), s.value._3.map( allParsables(_)))
        //case f: Failure => println(f.trace().longMsg); None
        case _ => None
      }
    }

    val result2 = time("parse2") {
      fastparse.parse(input, implicit p => command2Parser)
      match {
        case s: Success[(Int, Int, Option[Int])] => Some(allParsables(s.value._1), Option(allParsables(s.value._2)), s.value._3.map(allParsables(_)))
        //case f: Failure => println(f.trace().longMsg); None
        case _ => None
      }
    }

    val result3 = time("parse3") {
      fastparse.parse(input, implicit p => command3Parser)
      match {
        case s: Success[(Int, Int, Int)] => Some(allParsables(s.value._1), Option(allParsables(s.value._2)), Option(allParsables(s.value._3)))
        //case f: Failure => println(f.trace().longMsg); None
        case _ => None
      }
    }

    val ret = (result1, result2, result3)
    ret
  }
}
