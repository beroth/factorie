package cc.factorie.epistemodb.types

import java.io.File

import cc.factorie.app.nlp.{Sentence, Token}
import edu.umass.cs.iesl.serialized_wikipedia.LoadWikipedia

import scala.collection.mutable.ArrayBuffer

/**
 * Created by beroth on 10/8/15.
 */
object DataFromWikipedia {


  def getNounsAndPatterns(sentence: Sentence): Seq[String] /* Seq[(String, Seq[String])]*/ = {
    val tokens = sentence.tokens.zipWithIndex

    val nouns = tokens.foldLeft((false, new ArrayBuffer[ArrayBuffer[String]]))((nounBuffer, tokenIndex) => {
      val tok = tokenIndex._1

      val lastTagWasNoun = nounBuffer._1
      val buf: ArrayBuffer[ArrayBuffer[String]] =  nounBuffer._2

      if (tok.posTag.categoryValue.startsWith("N")) {
        val currNounBuf = if (lastTagWasNoun) {
          buf.last
        } else {
          val nbuf = new ArrayBuffer[String]
          buf+=nbuf
          nbuf
        }
        currNounBuf+=tok.string
        (true, buf)
      } else {
        (false, buf)
      }
    })._2

    nouns.map(_.mkString(" ")).toSeq
  }

  def main(args: Array[String]) : Unit = {
    LoadWikipedia.loadFile(new File(args(0)), "UTF-8").foreach{
      f =>
        //print("DOCUMENT: ")
        //println(f.name)
        for (s <- f.sentences) {
          val tokens = s.tokens
          val sString = tokens.map(tok => tok.string + "/" + tok.posTag.categoryValue.toString).mkString(" ")
          println(sString)
          for (n <- getNounsAndPatterns(s)) {
            println(n)
          }
        }
    }

  }
}
