package edu.berkeley.nlp.entity.preprocess

import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser
import edu.berkeley.nlp.entity.ConllDoc
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import edu.berkeley.nlp.entity.ConllDocReader
import edu.berkeley.nlp.syntax.Tree
import edu.berkeley.nlp.futile.util.Logger
import java.util.Arrays
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.entity.Chunk
import edu.berkeley.nlp.entity.ConllDocWriter
import edu.berkeley.nlp.entity.ner.NerSystemLabeled

object Reprocessor {

  def redoConllDocument(parser: CoarseToFineMaxRuleParser, backoffParser: CoarseToFineMaxRuleParser, nerSystem: NerSystemLabeled, docReader: ConllDocReader, inputPath: String, outputPath: String) {
    val writer = IOUtils.openOutHard(outputPath);
    val docs = docReader.readConllDocs(inputPath);
    for (doc <- docs) {
      Logger.logss("Reprocessing: " + doc.docID + " part " + doc.docPartNo);
      val newPos = new ArrayBuffer[Seq[String]]();
      val newParses = new ArrayBuffer[edu.berkeley.nlp.futile.syntax.Tree[String]]();
      val newNerChunks = new ArrayBuffer[Seq[Chunk[String]]]();
      for (sentIdx <- 0 until doc.words.size) {
        if (sentIdx % 10 == 0) {
          Logger.logss("Sentence " + sentIdx);
        }
        val sent = doc.words(sentIdx);
        var parse = PreprocessingDriver.parse(parser, backoffParser, sent.asJava);
        parse = if (parse.getYield().size() != sent.length) {
          Logger.logss("Couldn't parse sentence: " + sent.toSeq);
          Logger.logss("Using default parse");
          convertFromFutileTree(doc.trees(sentIdx).constTree);
        } else {
          parse;
        }
        val posTags = parse.getPreTerminalYield().asScala.toArray;
        newPos += posTags;
        newParses += convertToFutileTree(parse);
        val nerBioLabels = if (nerSystem != null) nerSystem.tagBIO(sent.toArray, posTags) else Array.tabulate(sent.size)(i => "O");
        newNerChunks += convertBioToChunks(nerBioLabels);
      }
      ConllDocWriter.writeIncompleteConllDoc(writer, doc.docID, doc.docPartNo, doc.words, newPos, newParses, doc.speakers, newNerChunks, doc.corefChunks);
    }
    writer.close();
  }
  
  def convertBioToChunks(nerBioLabels: Seq[String]): Seq[Chunk[String]] = {
    var lastNerStart = -1;
    val chunks = new ArrayBuffer[Chunk[String]]();
    for (i <- 0 until nerBioLabels.size) {
      if (nerBioLabels(i).startsWith("B")) {
        if (lastNerStart != -1) {
          chunks += new Chunk[String](lastNerStart, i, "MISC");
        }
        lastNerStart = i;
      } else if (nerBioLabels(i).startsWith("O")) {
        if (lastNerStart != -1) {
          chunks += new Chunk[String](lastNerStart, i, "MISC");
          lastNerStart = -1;
        }
      }
    }
    chunks;
  }
  
  def convertToFutileTree(slavTree: edu.berkeley.nlp.syntax.Tree[String]): edu.berkeley.nlp.futile.syntax.Tree[String] = {
    new edu.berkeley.nlp.futile.syntax.Tree[String](slavTree.getLabel(), slavTree.getChildren().asScala.map(convertToFutileTree(_)).asJava);
  }
  
  def convertFromFutileTree(myTree: edu.berkeley.nlp.futile.syntax.Tree[String]): edu.berkeley.nlp.syntax.Tree[String] = {
    new edu.berkeley.nlp.syntax.Tree[String](myTree.getLabel(), myTree.getChildren().asScala.map(convertFromFutileTree(_)).asJava);
  }
}
