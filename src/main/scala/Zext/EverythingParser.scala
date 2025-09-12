package Zext

import Zext.Parser.PartOfSpeech.{noun, verb}
import Zext.Parser.{BuildUnderstandables, CustomWord, Parsable, ParsableType, understandableEverything, understandableNouns, understandableVerbs}
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


  case class ParseResult(verbs : Array[ParsableType], verbString : String, nouns : Array[Array[ParsableType]], nounStrings : Array[String] )


  def parse(input: String) : (Option[ParseResult], Option[ParseResult]) = {

    BuildUnderstandables()
    val (verbWords, verbParsables) = understandableVerbs.map(PossibleWord).filter(_.isDefined).map(_.get).toArray.sortBy(_._1.length).reverse.unzip
    val (everythingWords, everythingParsables) = understandableEverything.map(PossibleWord).filter(_.isDefined).map(_.get).toArray.sortBy(_._1.length).reverse.unzip

    def space[$: P] = P( CharIn(" \t\n\r").rep(1))
    def anySpace[$: P] = P(CharIn(" \t\n\r").rep)
    def prepositionParser[$: P] = StringIn("on top of", "about", "on to", "with", "from", "into", "in", "at", "to", "on")
    def ignored[$: P] = StringIn("some", "the", "a", "an", "and")

    def indexResultParser[$: P](t : (String, Int)) = () => P( (ignored ~ space).? ~ t._1).map(_ => (t._1, t._2))

    val allVerbsWithIndices = verbWords.zipWithIndex
    def parseVerbs[$: P] = allVerbsWithIndices.map(indexResultParser).foldLeft(Fail.map(_=> ("", -1) ))((l, r) => l | r())

    val everythingWithIndices = everythingWords.zipWithIndex
    def parseEverything[$: P] = everythingWithIndices.map(indexResultParser).foldLeft(Fail.map(_ => ("", -1)))((l, r) => l | r())

    def customWord[$ : P] = P(CharIn("a-z") | CharIn("A-Z")).rep(1).!
    def customWordParser[$ : P] = customWord.map((_, -1))

    def spaceOrEnd[$: P] = P(space | End)

    def everythingOrCustom[$: P] = P(parseEverything | customWordParser)

    def commandParser[$: P] = P( anySpace ~ parseVerbs ~ ( (space ~ prepositionParser).? ~ (space ~ parseEverything ~ &(spaceOrEnd)) ).rep ~ End)
    def customParser[$: P] = P( anySpace ~ parseVerbs ~ ( (space ~ prepositionParser).? ~ (space ~ everythingOrCustom ~ &(spaceOrEnd)) ).rep ~ End)


    val customResult = {
      fastparse.parse(input, implicit p => customParser) match {
        case s: Success[(String, Int, Seq[(String,Int)])] => {
          val verb = verbParsables(s.value._2)
          val verbWord = s.value._1
          for(av <- s.value._3){
            //println(av)
          }

          val targets : Seq[ (String, Array[ParsableType]) ] = s.value._3.map { (s, i) =>
            if(i == -1)
              (s, Array())
            else
              (s, everythingParsables(i))
          }

          val (nounWords, nouns) = targets.unzip

          Some( ParseResult(verb, verbWord, nouns.toArray, nounWords.toArray) )
        }
        //case f: Failure => println(f.trace().longMsg); None
        case _ => None
      }

    }


    val result = time("parse4") {
      fastparse.parse(input, implicit p => commandParser)
      match {
        case s: Success[ (String, Int, Seq[(String,Int)]) ] =>  {
          val verb = verbParsables(s.value._2)
          val verbWord = s.value._1
          val nouns = s.value._3.map(n => everythingParsables(n._2))
          val nounWords = s.value._3.map(_._1)

          Some(ParseResult(verb, verbWord, nouns.toArray, nounWords.toArray))
        }
        //case f: Failure => println(f.trace().longMsg); None
        case _ => None
      }


    }

    (result, customResult)
  }
}
