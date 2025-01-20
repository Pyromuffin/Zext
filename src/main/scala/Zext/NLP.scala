package Zext

import opennlp.tools.models.*
import opennlp.tools.postag.*
import java.io.{ByteArrayInputStream, FileInputStream}

object NLP {
  val modelFinder = opennlp.tools.models.simple.SimpleClassPathModelFinder("opennlp-models-pos-en-1.2.0.jar")
  val loader = ClassPathModelLoader()
  val model = modelFinder.findModels(false).iterator().next()
  val loaded = loader.load(model)
  val stream = new ByteArrayInputStream(loaded.model())
  val partOfSpeechModel = new opennlp.tools.postag.POSModel(stream)
  val tagger = new POSTaggerME(partOfSpeechModel, POSTagFormat.UD)


  def GetNouns(str : String) : Array[String] = {
    val words = str.split(" ")
    val tags = tagger.tag(words)
    val zipped = words.zip(tags)
    // println(zipped.mkString("Array(", ", ", ")"))
    val nouns = zipped.filter{ (word, tag) =>

        if(tag == "NOUN") true
        else if(tag == "PROPN") true
        else if(tag == "PRON") true // maybe
        else false

    }.map(_._1.toLowerCase())

    //println(nouns.mkString("Array(", ", ", ")"))
    nouns
  }




}
