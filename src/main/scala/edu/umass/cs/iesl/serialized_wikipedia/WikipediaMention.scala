package edu.umass.cs.iesl.serialized_wikipedia

import cc.factorie.app.nlp.{Section, Document}


case class WikipediaMention (docId: String, offset: Int, length: Int, surfaceForm: Option[String], link: Option[String]) extends Serializable{ // TODO: What do we have to do here?

  def serialize: String = docId.replaceAll("\n\t","_") + "\t" + offset +  "\t" + length + "\t" + surfaceForm.getOrElse("").replaceAll("\n\t","_") + "\t" + link.getOrElse("").replaceAll("\n\t","_")

}


object WikipediaMention {
  def serialize(mentions: Iterable[WikipediaMention]): String = mentions.map(_.serialize).mkString("\n")
}



case class WikipediaMentionCollection(mentions: Iterable[WikipediaMention])

object AlignMentions {
  
  def apply(mentions: Iterable[WikipediaMention], doc:Document, warn: Boolean = false): Unit = doc.sections.map(apply(mentions,_,warn))


  def apply(mentions: Iterable[WikipediaMention],section: Section, warn: Boolean): Unit = {
    mentions.foreach {
      mention =>
        section.offsetSnapToTokens(mention.offset, mention.offset + mention.length) match {
          case Some(ts) =>
            ts.head.attr += mention
          case None =>
            if (warn) {
              println(s"WARNING: we weren't able to align the mention $mention.")
              val docStr = section.string
              if (mention.offset > 0 && mention.offset + mention.length < docStr.length)
                println(s"The document had the string ${docStr.substring(mention.offset,mention.offset + mention.length)}")
              else
                println(s"The document string had a length of only ${docStr.length} characters")
              if (section.tokenAtCharOffset(mention.offset).isDefined)
                println(s"The token at the start offset was: ${section.tokenAtCharOffset(mention.offset).get}")
              else
                println(s"There was no token as the start offset")
              if (section.tokenAtCharOffset(mention.offset + mention.length).isDefined)
                println(s"The token at the end offset was: ${section.tokenAtCharOffset(mention.offset + mention.length).get}")
              else
                println(s"There was no token as the end offset")
            }
        }
    }
  }
}
