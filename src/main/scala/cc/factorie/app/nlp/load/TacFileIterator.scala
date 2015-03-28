package cc.factorie.app.nlp.load

import java.io._
import java.util.zip.GZIPInputStream
import java.util.Scanner
import cc.factorie.app.nlp.Document
import scala.collection.JavaConverters._
import cc.factorie.app.nlp.load.TACDocTypes.TACDocumentType

object TACDocTypes {
  sealed trait TACDocumentType
  case object Newswire extends TACDocumentType
  case object DiscussionForum extends TACDocumentType
  case object WebDocument extends TACDocumentType

  object TACDocumentType {
    def fromFilePath(f:File):TACDocumentType = {
      val path = f.getAbsolutePath.toLowerCase
      if(path.contains("discussion_forums")) {
        DiscussionForum
      } else if(path.contains("newswire")) {
        Newswire
      } else if(path.contains("web")) {
        WebDocument
      } else {
        throw new Exception("Unable to assign document at path %s to a document type".format(path))
      }
    }
  }
}

case class DocStringWithId(id:String, docString:String, sourceFilename:String) {
  def toDocument = {
    val doc = new Document(docString).setName(id)
    doc.attr += TACDocumentType.fromFilePath(new File(sourceFilename))
    doc.annotators += classOf[TACDocumentType] -> classOf[TACDocumentType]
    doc
  }
}

/**
 * The base class for splitting up tac files. Reads an iterator of lines of a tac
 * file an returns an iterator of document strings with their names.
 */
class TacStringIterator(lines:Iterator[String], filename:String="") extends Iterator[DocStringWithId] {
  private val docEndString = """</doc>"""
  private val webDocStartString = """<DOC>"""
  private val docIdRegex = """(?i)<DOC ID="([^"]+)"[^>]*>""".r
  private val webDocIdRegex = """(?i)<DOCID> ([^ ]+) </DOCID>""".r

  private var docBuffer = new StringBuilder()
  private var line = null.asInstanceOf[String]
  private var lineNum = 0

  // grouping together to avoid forgetting something
  @inline
  private def advanceLine() {
    docBuffer append line
    docBuffer append "\n"
    line = if(lines.hasNext) lines.next() else null
    lineNum += 1
  }

  //priming the pump - we don't call advanceLine because we don't want to add a null to the start of our doc
  line = if(lines.hasNext) lines.next() else null
  lineNum += 1

  def next() = {

    val docIdMatchOpt = docIdRegex.unapplySeq(line).map(_.head)

    // We should be at the start of a new document here, otherwise we have a problem.
    assert(line.equalsIgnoreCase(webDocStartString) || docIdMatchOpt.isDefined, "Found line: |%s| that was not a valid doc start at line %d in %s".format(line, lineNum, filename))
    val docId = if(docIdMatchOpt.isDefined) {
      docIdRegex.unapplySeq(line).get.head
      //var docIdRegex(docId) = line
    } else if(line equalsIgnoreCase webDocStartString) { // we know that one must be true but let's not tempt fate
      advanceLine()
      //var webDocIdRegex(docId) = line
      webDocIdRegex.unapplySeq(line).get.head
    } else {
      throw new Exception("Found line: |%s| that was not a valid doc start at line %d in %s".format(line, lineNum, filename))
    }

    while(!line.equalsIgnoreCase(docEndString)) {
      advanceLine()
    }
    // the loop exits when the doc end is found, but that us still part of the previous document so we need to consume it.
    advanceLine()
    val docString = docBuffer.toString()
    docBuffer = new StringBuilder()
    DocStringWithId(docId, docString, filename)
  }

  def hasNext = line != null
}


class TacFileIterator(tacDocFile:File) extends Iterator[DocStringWithId] {
  /** we use scanner here so that when we recreate the lines by adding \n we don't change
    * the character count on documents that may use crlf to delimit lines
    */
  private val iter = new TacStringIterator(new Scanner(if(tacDocFile.getName.endsWith(".gz")) {
    new GZIPInputStream(new FileInputStream(tacDocFile))
  } else {
    new FileInputStream(tacDocFile)
  }).useDelimiter("\n").asScala, tacDocFile.getAbsolutePath)


  def hasNext = iter.hasNext
  def next() = iter.next()
}

class TacDocumentIterator(tacDocFile:File) extends Iterator[Document] {
  private val iter = new TacFileIterator(tacDocFile)

  def next() = iter.next().toDocument

  def hasNext = iter.hasNext
}
