package edu.umass.cs.iesl.serialized_wikipedia

import java.io.File

import cc.factorie.app.nlp.Token

object LoadTest {

  def main(args: Array[String]) : Unit = {

    LoadWikipedia.loadFile(new File(args(0)), "UTF-8").foreach{
      f =>
        print("DOCUMENT: ")
        println(f.name)
        println(f.owplString(Iterable((t:Token) => t.string, (t:Token) => t.posTag)))
    }

  }

}
