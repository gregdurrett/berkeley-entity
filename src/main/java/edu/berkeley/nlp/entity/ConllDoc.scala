package edu.berkeley.nlp.entity
import edu.berkeley.nlp.futile.syntax.Tree


case class ConllDocJustWords(val docID: String,
                             val docPartNo: Int,
                             val words: Seq[Seq[String]]) {
  def wordsArrs = words.map(_.toArray).toArray;
}

// rawText should only be used to save trouble when outputting the document
// for scoring; never at any other time!
case class ConllDoc(val docID: String,
                    val docPartNo: Int,
                    val words: Seq[Seq[String]],
                    val pos: Seq[Seq[String]],
                    val trees: Seq[DepConstTree],
                    val nerChunks: Seq[Seq[Chunk[String]]],
                    val corefChunks: Seq[Seq[Chunk[Int]]],
                    val speakers: Seq[Seq[String]]) extends Document {
  
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

  override def getCorrespondingNERChunk(sentIdx: Int, headIdx: Int): Option[Chunk[String]] = ConllDoc.getCorrespondingNERChunk(nerChunks(sentIdx), headIdx);
}

object ConllDoc {
  
  def getCorrespondingNERChunk(nerChunks: Seq[Chunk[String]], headIdx: Int): Option[Chunk[String]] = {
    val maybeChunk = nerChunks.filter(chunk => chunk.start <= headIdx && headIdx < chunk.end);
    if (maybeChunk.size >= 1) Some(maybeChunk.head) else None;
  }
}
