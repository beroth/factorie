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

        val sentences = f.owplString(Iterable((t:Token) => t.posTag)).split("\n\n")
        for (s <- sentences) {
          val tokens = s.split("\n").map(_.split("\t")(3))
          println(tokens.mkString(" "))
        }
//println(f.owplString(Iterable((t:Token) => t.string, (t:Token) => t.posTag)))
    }

  }
}
