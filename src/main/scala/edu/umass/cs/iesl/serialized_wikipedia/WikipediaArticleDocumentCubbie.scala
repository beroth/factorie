package edu.umass.cs.iesl.serialized_wikipedia

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner._
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.util.{Cubbie, IntArrayBuffer}

import scala.collection.mutable.ArrayBuffer


/**
 * Modified version of TacDocumentCubbie
 * * Created by strubell on 3/25/15.
 */
class WikipediaArticleDocumentCubbie extends DocumentCubbie {
  def this(document:Document) = { this(); this := document }
  val string = StringSlot("string")
  val name = StringSlot("name")
  val date = DateSlot("date")
  val sections = CubbieListSlot("sections", () => new WikipediaArticleSectionCubbie)
  //val mentions = WikipediaMentionsSlot("mentions")

  //  val mentions = DocumentMentionsSlot("mentions")
  def :=(document:Document): this.type = {
    name := document.name
    date := new java.util.Date()
    string := document.string
//        if (document.sections.length == 1 && document.sections.head == document.asSection)
    //          sections := new StandardSectionAnnotationsCubbie(document.asSection)
    //        else
    sections := document.sections.collect{ case section: Section => new WikipediaArticleSectionCubbie(section) }
    //    if (document.coref ne null) mentions := document
    
    //mentions := new WikipediaMentionsCubbie(document.attr[WikipediaMentionCollection].mentions)
    
    this
  }
  def document: Document = {
    val doc = new Document(string.value)
    doc.setName(name.value)
    doc.attr += date.value
    assert(sections.isDefined)
    for (sc <- sections.value) doc =: sc
    //mentions.value.=:(doc)
    //    if (mentions.isDefined) doc =: mentions
    doc
  }

  class WikipediaMentionsCubbie extends Cubbie {
    
    def this(mentions: Iterable[WikipediaMention]) = {this(); this := mentions}
    
    val docIds = new StringListSlot("docIds")
    val offsets = new IntSeqSlot("offsets")
    val lengths = new IntSeqSlot("lengths")
    val surfaceForms = new StringListSlot("surfaceForms")
    val links = new StringListSlot("links")

    def :=(mentions: Iterable[WikipediaMention]): this.type = {
      val offsts = new IntArrayBuffer(mentions.size)
      val lngths = new IntArrayBuffer(mentions.size)

      val dIds = new ArrayBuffer[String](mentions.size) // ok?
      val sfs = new ArrayBuffer[String](mentions.size)
      val lnks = new ArrayBuffer[String](mentions.size)
      for (mention <- mentions) {
        dIds += mention.docId
        offsts += mention.offset
        lngths += mention.length
        sfs += mention.surfaceForm.getOrElse("")
        lnks += mention.link.getOrElse("")
      }

      docIds.set(dIds.toSeq)
      offsets.set(offsts)
      lengths.set(lngths)
      surfaceForms.set(sfs.toSeq)
      links.set(lnks.toSeq)
      this
    }
    def =:(document:Document): this.type = {

      val dIds = this.docIds.value
      val offsts = this.offsets.value
      val lngths = this.lengths.value
      val sfs = this.surfaceForms.value
      val lnks = this.links.value

      val mentions = new ArrayBuffer[WikipediaMention](this.offsets.value.length)

      var i = 0
      while (i < dIds.length) {
        val mention = WikipediaMention(dIds(i),offsts(i),lngths(i), if (sfs(i).isEmpty) None else Some(sfs(i)), if(lnks(i).isEmpty) None else Some(lnks(i)))
        mentions += mention
        i += 1
      }

      AlignMentions(mentions,document, warn = true)
      document.attr += WikipediaMentionCollection(mentions)
      this
    }
  }

  class WikipediaMentionsSlot(name: String) extends CubbieSlot(name, () => new WikipediaMentionsCubbie())
  object WikipediaMentionsSlot { def apply(name:String) = new WikipediaMentionsSlot(name) }
}



class WikipediaArticleSectionCubbie extends DocumentCubbie {
  def this(section:Section) = { this(); this := section }
  val start = IntSlot("start")
  val end = IntSlot("end")
  val ts = SectionTokensAndSentencesSlot("ts")
  val pp = SectionPosAndParseSlot("pp")
  //val ner = SectionNerSlot("ner")
  def :=(section:Section): this.type = {
    start := section.stringStart
    end := section.stringEnd
    ts := section
    assert(section.document.annotatorFor(classOf[PennPosTag]).isDefined && (section.sentences.isEmpty || section.sentences.head.attr.contains(classOf[ParseTree])))
    pp := section
    //ner := section
    this
  }
  def =:(document:Document): this.type = {
    val section = new BasicSection(document, start.value, end.value); document += section
    section =: ts
        if (pp.isDefined) section =: pp
    this
  }
  
  class SectionNerSlot(name:String) extends IntSeqSlot(name) {
    def :=(section:Section): this.type = {
      val indices = new IntArrayBuffer(section.tokens.length * 3)
      val spanBuffer = section.attr[OntonotesNerSpanBuffer]
      assert(spanBuffer != null)
      for (span <- spanBuffer){
        indices += span.head.positionInSection
        indices += span.length
        indices += span.label.intValue
      }
      this := indices
      this
    }
    def =:(section:Section): this.type = {
      val indices = this.value; var i = 0
      val buf = new OntonotesNerSpanBuffer()
      while(i < indices.length){
        val pos = i; i += 1
        val len = i; i += 1
        val label = i; i += 1
        buf += new OntonotesNerSpan(section, pos, len, OntonotesNerDomain.category(label))
      }
      section.attr += buf
      section.document.annotators(classOf[OntonotesNerSpanBuffer]) = this.getClass
      // todo: other stuff?
      this
    }
  }
  object SectionNerSlot { def apply(name:String) = new SectionNerSlot(name) }

}
