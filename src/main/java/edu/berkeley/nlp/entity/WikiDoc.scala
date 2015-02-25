package edu.berkeley.nlp.entity

/**
 * Created by matthew on 2/18/15.
 */
case class WikiDoc (docID : String,
                    docPartNo : Int,
                    words : Seq[Seq[String]],
                    pos : Seq[Seq[String]],
                    trees: Seq[DepConstTree],
                    nerChunks : Seq[Seq[Chunk[String]]],
                    corefChunks : Seq[Seq[Chunk[Int]]],
                    speakers : Seq[Seq[String]],
                    wikiRefChunks : Seq[Seq[Chunk[String]]] ) extends Document {

  override val numSents = words.size;

  override def uid = docID -> docPartNo;

  override def fileName = {
    if (docID.contains("/")) {
      docID.substring(docID.lastIndexOf("/") + 1);
    } else {
      docID;
    }
  }

  override def printableDocName = docID + " (part " + docPartNo + ")";

  override def isConversation = docID.startsWith("bc") || docID.startsWith("wb")

  override def getCorrespondingNERChunk(sentIdx: Int, headIdx: Int): Option[Chunk[String]] = None;

  //override def corefChunks = throw new NotImplementedError()

}
