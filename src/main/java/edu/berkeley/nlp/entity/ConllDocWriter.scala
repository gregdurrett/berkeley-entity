package edu.berkeley.nlp.entity

import java.io.PrintWriter
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.preprocess.PreprocessingDriver
import edu.berkeley.nlp.futile.syntax.Tree
import scala.collection.mutable.HashSet
import scala.collection.JavaConverters._
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.entity.coref.OrderedClusteringBound
import edu.berkeley.nlp.entity.preprocess.Reprocessor
import edu.berkeley.nlp.entity.wiki.WikiAnnotReaderWriter

object ConllDocWriter {
  
  def writeDoc(writer: PrintWriter, conllDoc: ConllDoc) {
    writeIncompleteConllDoc(writer, conllDoc)
  }

  /**
   * Writes a document with a clustering grafted on (usually what we want for coref experiments,
   * where only the cluster is changing)
   */
  def writeDoc(writer: PrintWriter, conllDoc: ConllDoc, clustering: OrderedClusteringBound) {
    writeIncompleteConllDoc(writer, conllDoc.docID, conllDoc.docPartNo, conllDoc.words, conllDoc.pos, conllDoc.trees.map(_.constTree), conllDoc.speakers, conllDoc.nerChunks, convertOrderedClusteringBoundToChunks(clustering, conllDoc.words.size));
//    val corefBits = getCorefBits(conllDoc.words.map(_.size), convertOrderedClusteringBoundToChunks(clustering, conllDoc.words.size));
//    val numZeroesToAddToPartNo = 3 - conllDoc.docPartNo.toString.size;
//    writer.println("#begin document (" + conllDoc.docID + "); part " + ("0" * numZeroesToAddToPartNo) + conllDoc.docPartNo);
//    for (sentIdx <- 0 until conllDoc.rawText.size) {
//      val sent = conllDoc.rawText(sentIdx);
//      for (tokenIdx <- 0 until sent.size) {
//        val line = conllDoc.rawText(sentIdx)(tokenIdx);
//        val lineNoCoref = line.substring(0, Math.max(line.lastIndexOf("\t"), line.lastIndexOf(" ")) + 1);
////        writer.println(lineNoCoref + corefBits(sentIdx)(tokenIdx));
//        writer.println(lineNoCoref.replaceAll("\\s+", "\t") + corefBits(sentIdx)(tokenIdx));
//      }
//      writer.println();
//    }
//    writer.println("#end document");
  }
  
  def writeDocWithPredAnnotations(writer: PrintWriter,
                                  conllDoc: ConllDoc,
                                  nerChunks: Seq[Seq[Chunk[String]]],
                                  corefClustering: OrderedClusteringBound,
                                  wikiChunks: Option[Seq[Seq[Chunk[String]]]] = None) {
    writeIncompleteConllDocNestedNER(writer, conllDoc.docID, conllDoc.docPartNo, conllDoc.words, conllDoc.pos, conllDoc.trees.map(_.constTree), conllDoc.speakers, nerChunks, convertOrderedClusteringBoundToChunks(corefClustering, conllDoc.words.size), wikiChunks);
  }
  
  
  def writeDocWithPredAnnotationsWikiStandoff(writer: PrintWriter,
                                              standoffWriter: PrintWriter,
                                              conllDoc: ConllDoc,
                                              nerChunks: Seq[Seq[Chunk[String]]],
                                              corefClustering: OrderedClusteringBound,
                                              wikiChunks: Seq[Seq[Chunk[String]]]) {
    writeIncompleteConllDocNestedNER(writer, conllDoc.docID, conllDoc.docPartNo, conllDoc.words, conllDoc.pos, conllDoc.trees.map(_.constTree), conllDoc.speakers, nerChunks, convertOrderedClusteringBoundToChunks(corefClustering, conllDoc.words.size), None);
    WikiAnnotReaderWriter.writeStandoffAnnots(standoffWriter, conllDoc.docID, conllDoc.docPartNo, wikiChunks.map(_.map(Chunk.seqify(_))), conllDoc.words.map(_.size))
  }
  
  def writeIncompleteConllDoc(writer: PrintWriter,
                              doc: ConllDoc) {
    writeIncompleteConllDocNestedNER(writer, doc.docID, doc.docPartNo, doc.words, doc.pos, doc.trees.map(_.constTree), doc.speakers, doc.nerChunks, doc.corefChunks);
  }
  
  // Doesn't write predicate-argument structures, senses, or lemmas (but we don't use these).
  def writeIncompleteConllDoc(writer: PrintWriter,
                              docName: String,
                              partNo: Int,
                              words: Seq[Seq[String]],
                              pos: Seq[Seq[String]],
                              parses: Seq[Tree[String]],
                              speakers: Seq[Seq[String]],
                              nerChunks: Seq[Seq[Chunk[String]]],
                              corefChunks: Seq[Seq[Chunk[Int]]],
                              wikiChunks: Option[Seq[Seq[Chunk[String]]]] = None) {
    val sentLens = words.map(_.size);
    val parseBits = parses.map(tree => PreprocessingDriver.computeParseBits(Reprocessor.convertFromFutileTree(tree)).toSeq);
    val nerBits = getNerBits(sentLens, nerChunks);
    val corefBits = getCorefBits(sentLens, corefChunks);
    val maybeWikiBits = wikiChunks.map(chunks => getNerBitsPossiblyNestedChunks(sentLens, wikiChunks.get))
    writeIncompleteConllDocFromBits(writer, docName, partNo, words, pos, parseBits, speakers, nerBits, corefBits, maybeWikiBits);
  }
  
  def writeIncompleteConllDocNestedNER(writer: PrintWriter,
                                       docName: String,
                                       partNo: Int,
                                       words: Seq[Seq[String]],
                                       pos: Seq[Seq[String]],
                                       parses: Seq[Tree[String]],
                                       speakers: Seq[Seq[String]],
                                       nerChunks: Seq[Seq[Chunk[String]]],
                                       corefChunks: Seq[Seq[Chunk[Int]]],
                                       wikiChunks: Option[Seq[Seq[Chunk[String]]]] = None) {
    val sentLens = words.map(_.size);
    val parseBits = parses.map(tree => PreprocessingDriver.computeParseBits(Reprocessor.convertFromFutileTree(tree)).toSeq);
    val nerBits = getNerBitsPossiblyNestedChunks(sentLens, nerChunks);
    val corefBits = getCorefBits(sentLens, corefChunks);
    val maybeWikiBits = wikiChunks.map(chunks => getNerBitsPossiblyNestedChunks(sentLens, wikiChunks.get))
    writeIncompleteConllDocFromBits(writer, docName, partNo, words, pos, parseBits, speakers, nerBits, corefBits, maybeWikiBits);
  }
  
  def writeIncompleteConllDocFromBits(writer: PrintWriter,
                                      docName: String,
                                      partNo: Int,
                                      words: Seq[Seq[String]],
                                      pos: Seq[Seq[String]],
                                      parseBits: Seq[Seq[String]],
                                      speakers: Seq[Seq[String]],
                                      nerBits: Seq[Seq[String]],
                                      corefBits: Seq[Seq[String]],
                                      maybeWikiBits: Option[Seq[Seq[String]]] = None) {
    val numZeroesToAddToPartNo = 3 - partNo.toString.size;
    writer.println("#begin document (" + docName + "); part " + ("0" * numZeroesToAddToPartNo) + partNo);
    for (sentIdx <- 0 until words.size) {
      val sent = words(sentIdx);
      for (i <- 0 until sent.size) {
        writer.println(docName + "\t" + partNo + "\t" + i + "\t" + words(sentIdx)(i) + "\t" + pos(sentIdx)(i) + "\t" + parseBits(sentIdx)(i) +
          "\t-\t-\t-\t" + speakers(sentIdx)(i) + "\t" + nerBits(sentIdx)(i) + "\t" + corefBits(sentIdx)(i) +
          (if (maybeWikiBits.isDefined) "\t" + maybeWikiBits.get(sentIdx)(i) else ""));
      }
      writer.println();
    }
    writer.println("#end document");
  }
  
  private def convertOrderedClusteringBoundToChunks(clustering: OrderedClusteringBound, numSentences: Int): Seq[Seq[Chunk[Int]]] = {
    val chunksPerSentence = Array.tabulate(numSentences)(i => new ArrayBuffer[Chunk[Int]]());
    for (i <- 0 until clustering.ments.size) {
      val ment = clustering.ments(i);
      chunksPerSentence(ment.sentIdx) += new Chunk(ment.startIdx, ment.endIdx, clustering.clustering.getClusterIdx(i));
    }
    chunksPerSentence;
  }
  
  def getNerBits(sentLens: Seq[Int], nerChunks: Seq[Seq[Chunk[String]]]): Seq[Seq[String]] = {
    for (sentIdx <- 0 until sentLens.size) yield {
      val chunkStarts = new HashMap[Int,String];
      val chunkEnds = new HashSet[Int];
      for (chunk <- nerChunks(sentIdx)) {
        chunkStarts.put(chunk.start, chunk.label);
        chunkEnds += chunk.end - 1;
      }
      for (tokenIdx <- 0 until sentLens(sentIdx)) yield {
        if (chunkStarts.contains(tokenIdx) && chunkEnds.contains(tokenIdx)) {
          "(" + chunkStarts.get(tokenIdx).getOrElse("") + ")";
        } else if (chunkStarts.contains(tokenIdx)) {
          "(" + chunkStarts.get(tokenIdx).getOrElse("") + "*";
        } else if (chunkEnds.contains(tokenIdx)) {
          "*)";
        } else {
          "*";
        }
      }
    }
  }
  
  def getNerBitsPossiblyNestedChunks(sentLens: Seq[Int], nerChunks: Seq[Seq[Chunk[String]]]): Seq[Seq[String]] = {
    for (sentIdx <- 0 until sentLens.size) yield {
      for (tokenIdx <- 0 until sentLens(sentIdx)) yield {
        val chunksStartingHere = nerChunks(sentIdx).filter(chunk => chunk.start == tokenIdx).sortBy(- _.end);
        val numChunksEndingHere = nerChunks(sentIdx).filter(chunk => chunk.end - 1 == tokenIdx).size;
        var str = "";
        for (chunk <- chunksStartingHere) {
          str += "(" + chunk.label;
        }
        str += "*";
        for (i <- 0 until numChunksEndingHere) {
          str += ")";
        }
        str;
      }
    }
  }
  
  def getCorefBits(sentLens: Seq[Int], corefChunks: Seq[Seq[Chunk[Int]]]): Seq[Seq[String]] = {
    for (sentIdx <- 0 until sentLens.size) yield {
      val mentionStarts = new HashMap[Int,ArrayBuffer[Int]];
      val mentionEnds = new HashMap[Int,ArrayBuffer[Int]];
      val mentionStartEnds = new HashMap[Int,Int];
      val chunksThisSent = corefChunks(sentIdx);
      for (chunk <- chunksThisSent) {
        val start = chunk.start;
        val end = chunk.end - 1;
        if (start == end) {
          mentionStartEnds.put(start, chunk.label);
        } else {
          if (!mentionStarts.contains(start)) {
            mentionStarts.put(start, new ArrayBuffer[Int]())
          }
          mentionStarts(start) += chunk.label;
          if (!mentionEnds.contains(end)) {
            mentionEnds.put(end, new ArrayBuffer[Int]())
          }
          mentionEnds(end) += chunk.label;
        }
      }
      for (tokenIdx <- 0 until sentLens(sentIdx)) yield {
        var corefBit = "";
        if (mentionStarts.contains(tokenIdx)) {
          for (start <- mentionStarts(tokenIdx)) {
            corefBit += "(" + start + "|";
          }
        }
        if (mentionStartEnds.contains(tokenIdx)) {
          corefBit += "(" + mentionStartEnds(tokenIdx) + ")|";
        }
        if (mentionEnds.contains(tokenIdx)) {
          for (end <- mentionEnds(tokenIdx)) {
            corefBit += end + ")|";
          }
        }
        if (corefBit.isEmpty) "-" else corefBit.dropRight(1);
      }
    }
  }
  
  def writeDocIllinoisColumnFormat(writer: PrintWriter, conllDoc: ConllDoc) {
    writer.println("O\t0\t0\tO\t-X-\t-DOCSTART-\tx\tx\t0");
    
//    B-LOC   0       0       I-NP    NNP     Portugal        x       x       0
//O       0       1       I-VP    VBD     called  x       x       0
//O       0       2       I-PP    IN      up      x       x       0
//B-ORG   0       3       I-NP    NNP     Porto   x       x       0
//O       0       4       I-NP    JJ      central x       x       0
//O       0       5       I-NP    NN      defender        x       x       0
//B-PER   0       6       I-NP    NNP     Joao    x       x       0
//I-PER   0       7       I-NP    NNP     Manuel  x       x       0
//I-PER   0       8       I-VP    NNP     Pinto   x       x       0
//O       0       9       I-PP    IN      on      x       x       0
//O       0       10      I-NP    NNP     Friday  x       x       0
    for (sentIdx <- 0 until conllDoc.words.size) {
      val sent = conllDoc.words(sentIdx);
      for (tokenIdx <- 0 until sent.size) {
        val line = getConllNerBit(tokenIdx, conllDoc.nerChunks(sentIdx)) + "\t" + sentIdx + "\t" + tokenIdx + "\t" +
            "x\t" + conllDoc.pos(sentIdx)(tokenIdx) + "\t" + conllDoc.words(sentIdx)(tokenIdx).replaceAll("\\s+", "_") + "\t" +
            "x\tx\t0";
        writer.println(line); 
      }
      writer.println();
    }
  }
  
  def getConllNerBit(tokIdx: Int, nerChunks: Seq[Chunk[String]]): String = {
    val relevantNerChunks = nerChunks.filter(chunk => chunk.start <= tokIdx && tokIdx < chunk.end);
    if (relevantNerChunks.size > 0) {
      val relevantNerChunk = relevantNerChunks(0);
      if (tokIdx == relevantNerChunk.start) {
        "B-" + relevantNerChunk.label;
      } else {
        "I-" + relevantNerChunk.label;
      }
    } else {
      "O"
    }
  }
  
  def convertToIllinoisColumnFormat(dataDir: String, outFile: String, shuffle: Boolean) {
    val goldDocuments = ConllDocReader.loadRawConllDocsWithSuffix(dataDir, -1, "gold_conll", Language.ENGLISH);
    val goldDocumentsShuffled = if (shuffle) new scala.util.Random(0).shuffle(goldDocuments) else goldDocuments
    val outWriter = IOUtils.openOutHard(outFile);
    for (doc <- goldDocumentsShuffled) {
      writeDocIllinoisColumnFormat(outWriter, doc);
    }
    outWriter.close();
  }
  
  def main(args: Array[String]) = {
    convertToIllinoisColumnFormat("data/ontonotes-conll/train", "data/illinois-2011/train-shuf.col", true);
    convertToIllinoisColumnFormat("data/ontonotes-conll/dev", "data/illinois-2011/dev.col", false);
    convertToIllinoisColumnFormat("data/ontonotes-conll/test", "data/illinois-2011/test.col", false);
//    convertToIllinoisColumnFormat("data/conll-2012-en/train", "data/illinois-2012/train-shuf.col", true);
//    convertToIllinoisColumnFormat("data/conll-2012-en/dev", "data/illinois-2012/dev.col", false);
//    convertToIllinoisColumnFormat("data/conll-2012-en/test", "data/illinois-2012/test.col", false);
  }
}
