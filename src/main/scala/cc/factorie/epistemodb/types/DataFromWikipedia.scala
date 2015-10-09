package cc.factorie.epistemodb.types

import java.io.File

import cc.factorie.app.nlp.Token
import edu.umass.cs.iesl.serialized_wikipedia.LoadWikipedia

/**
 * Created by beroth on 10/8/15.
 */
object DataFromWikipedia {
  def main(args: Array[String]) : Unit = {
    LoadWikipedia.loadFile(new File(args(0)), "UTF-8").foreach{
      f =>
        print("DOCUMENT: ")
        println(f.name)

        for (s <- f.sentences) {
          val tokens = s.tokens
          val posTags = s.posTags

          if (tokens.length == posTags.length) {
            val sString = tokens.zip(posTags).map(tokTag => tokTag._1.toString + "/" + tokTag._2.toString).mkString(" ")
            println(sString)
          }
        }

//println(f.owplString(Iterable((t:Token) => t.string, (t:Token) => t.posTag)))
    }

  }
}
