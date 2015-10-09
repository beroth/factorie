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

  def adjPatterns(token: Token): Seq[String] = {
    val tokenBuf = new ArrayBuffer[Token]
    var adjChain = true
    for(i <- 1 to 3;
        if (token.hasPrev(i) && adjChain)
    ) {
      val prevTok = token.prev(i)
      if (prevTok.posTag.categoryValue.startsWith("JJ")) {
        tokenBuf += prevTok
      } else {
        adjChain = false
      }
    }

    if (tokenBuf.length > 0) {
      Seq(tokenBuf.reverse.map(_.string).mkString(" ") + " ARG")
    } else {
      Seq()
    }
  }

  def rightPatterns(token: Token): Seq[String] = {
    val tokenBuf = new ArrayBuffer[Token]
    var patternComplete = false
    for(i <- 1 to 5;
        if (token.hasNext(i) && !patternComplete)
    ) {
      val nextTok = token.next(i)
      tokenBuf += nextTok
      if (nextTok.posTag.categoryValue.startsWith("VB")) {
        patternComplete = true
      }
    }

    if (patternComplete) {
      Seq("ARG " + tokenBuf.map(_.string).mkString(" "))
    } else {
      Seq()
    }
  }

  def getNounsPatternsAndHypernyms(sentence: Sentence): /*Seq[String]*/Seq[(String, Seq[String], Seq[String])] = {

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

    nouns.map(tokBuf => (tokBuf.map(_.string).mkString(" "),
      leftPatterns(tokBuf.head) ++ rightPatterns(tokBuf.last) ++ adjPatterns(tokBuf.head),
      getHypernyms(tokBuf.head, tokBuf.last, nouns))).toSeq
  }

  def getHypernyms(firstToken: Token, lastToken: Token, nouns: ArrayBuffer[ArrayBuffer[Token]]): Seq[String] = {
    val hypernyms = new ArrayBuffer[String]()

    for (n <- nouns) {
      val distanceToTheRight = n.head.position - lastToken.position

      if (distanceToTheRight > 1 && distanceToTheRight <= 4) {
        val sb = new StringBuilder("ARG1")
        for(i <- 1 to distanceToTheRight - 1) {
          sb.append(" ")
          sb.append(lastToken.next(i).string)
        }
        sb.append(" ARG2")

        if (hearstPatterns.contains(sb.toString)) {

          println("===")
          println(firstToken.sentence)
          println(sb.toString)
          println(firstToken.string)
          println(lastToken.string)
          println(n.map(_.string).mkString(" "))

          hypernyms += n.map(_.string).mkString(" ")
        }
      }

      val distanceToTheLeft = firstToken.position - n.last.position

      if (distanceToTheLeft > 1 && distanceToTheLeft <= 4) {

        val sb = new StringBuilder("ARG2")
        for(i <- distanceToTheLeft - 1 to 1 by -1) {
          sb.append(" ")
          sb.append(lastToken.prev(i).string)
        }
        sb.append(" ARG1")

        if (hearstPatterns.contains(sb.toString)) {

          println("===")
          println(firstToken.sentence)
          println(sb.toString)
          println(firstToken.string)
          println(lastToken.string)
          println(n.map(_.string).mkString(" "))

          hypernyms += n.map(_.string).mkString(" ")
        }

      }
    }
    hypernyms.toSeq
  }

  /*
E is a C
E [,] and other C
E [,] or other C
C [,] such as E
C [,] including E
C [,] especially E
such C as E
*/
  val hearstPatterns = Set("ARG1 is a ARG2", "ARG1 is an ARG2", "ARG1 and other ARG2", "ARG1 , and other ARG2",
    "ARG1 or other ARG2", "ARG1 , or other ARG2", "ARG2 such as ARG1", "ARG2 , such as ARG1", "ARG2 including ARG1",
    "ARG2 , including ARG1", "ARG2 especially ARG1", "ARG2 , especially ARG1", "such ARG2 as ARG1")

  val coordinations = Set("and", "or", "or rather", "instead of", "(")

  def getNounPairs(sentence: Sentence): Seq[(String, String)] = {


    ???
  }

  def main(args: Array[String]) : Unit = {
    LoadWikipedia.loadFile(new File(args(0)), "UTF-8").foreach{
      f =>
        //print("DOCUMENT: ")
        //println(f.name)
        for (s <- f.sentences) {
          val tokens = s.tokens
          val sString = tokens.map(tok => tok.string + "/" + tok.posTag.categoryValue.toString).mkString(" ")
          //println(sString)
          for ((noun, patterns, hypernyms) <- getNounsPatternsAndHypernyms(s);
          pat <- patterns) {
            //println(noun + "\t" + pat)
          }
        }
    }

  }
}
