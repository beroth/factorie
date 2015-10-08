package edu.umass.cs.iesl.serialized_wikipedia

import java.io.{FileInputStream, InputStreamReader, BufferedReader, File}

import cc.factorie.app.nlp.Document
import cc.factorie.util.JsonCubbieConverter
import org.json4s.JsonAST.JObject
import org.json4s.jackson.JsonMethods._
import cc.factorie._


object LoadWikipedia {

  def loadLine(string: String): Option[Document] =
    parseOpt(string).map(jvalue => JsonCubbieConverter.toCubbie(jvalue.asInstanceOf[JObject], () => new WikipediaArticleDocumentCubbie()).document)

  def loadLines(lines: Iterator[String]): Iterator[Document] = lines.flatMap(loadLine)

  def loadFile(file: File, codec: String) = loadLines(new BufferedReader(new InputStreamReader(new FileInputStream(file), codec)).toIterator)


}