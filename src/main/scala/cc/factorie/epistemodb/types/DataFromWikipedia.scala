package cc.factorie.epistemodb.types

import java.io.File

import cc.factorie.app.nlp.{Sentence, Token}
import edu.umass.cs.iesl.serialized_wikipedia.LoadWikipedia

import scala.collection.mutable.ArrayBuffer

/**
 * Created by beroth on 10/8/15.
 */
object DataFromWikipedia {

  def leftPatterns(token: Token): Seq[String] = {
    val tokenBuf = new ArrayBuffer[Token]
    var patternComplete = false
    for(i <- 1 to 5;
      if (token.hasPrev(i) && !patternComplete)
    ) {
      val prevTok = token.prev(i)
      tokenBuf += prevTok
      if (prevTok.posTag.categoryValue.startsWith("VB")) {
        patternComplete = true
      }
    }

    if (patternComplete) {
      Seq(tokenBuf.reverse.map(_.string).mkString(" ") + " ARG")
    } else {
      Seq()
    }
  }

  def rightPatterns(sentence: Sentence, token: Token): Seq[String] = {

    ???
  }

  def getNounsAndPatterns(sentence: Sentence): /*Seq[String]*/Seq[(String, Seq[String])] = {

    val nouns = sentence.tokens.foldLeft((false, new ArrayBuffer[ArrayBuffer[Token]]))((nounBuffer, tok) => {
      val lastTagWasNoun = nounBuffer._1
      val buf: ArrayBuffer[ArrayBuffer[Token]] =  nounBuffer._2

      if (tok.posTag.categoryValue.startsWith("N")) {
        val currNounBuf = if (lastTagWasNoun) {
          buf.last
        } else {
          val nbuf = new ArrayBuffer[Token]
          buf+=nbuf
          nbuf
        }
        currNounBuf+=tok
        (true, buf)
      } else {
        (false, buf)
      }
    })._2.filter(_.length <= 3)

    nouns.map(tokBuf => (tokBuf.mkString(" "), leftPatterns(tokBuf.head) )).toSeq
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
