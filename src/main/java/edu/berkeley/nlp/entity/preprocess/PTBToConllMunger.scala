package edu.berkeley.nlp.entity.preprocess

import edu.berkeley.nlp.futile.LightRunner
import java.io.File
import edu.berkeley.nlp.futile.syntax.Trees.PennTreeReader
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.entity.ConllDoc
import edu.berkeley.nlp.entity.DepConstTree
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.Chunk
import edu.berkeley.nlp.entity.ConllDocWriter
import edu.berkeley.nlp.entity.coref.OrderedClusteringBound
import edu.berkeley.nlp.entity.coref.Mention
import edu.berkeley.nlp.entity.coref.OrderedClustering
import edu.berkeley.nlp.futile.util.Logger

/**
 * Takes either a file or directory as input, reads in PTB files one per line,
 * and writes them to a file or directory
 */
object PTBToConllMunger {

  val input = ""
  val output = ""
  
  def main(args: Array[String]) {
//    LightRunner.initializeOutput(PTBToConllMunger.getClass());
    LightRunner.populateScala(PTBToConllMunger.getClass(), args)
    val inputFile = new File(input)
    val outputFile = new File(output)
    var outputWriter = if (outputFile.isDirectory) null else IOUtils.openOutHard(outputFile)
    for (file <- (if (inputFile.isDirectory) inputFile.listFiles.toSeq else Seq(inputFile))) {
      val doc = readParsesMakeDoc(file)
      if (outputWriter == null) {
        outputWriter = IOUtils.openOutHard(outputFile.getAbsolutePath + "/" + doc.docID)
        ConllDocWriter.writeDoc(outputWriter, doc)
        outputWriter.close
        outputWriter = null
      } else {
        ConllDocWriter.writeDoc(outputWriter, doc)
      }
    }
    if (outputWriter != null) {
      outputWriter.close
    }
//    val outputFile = new File(output)
//    if (outputFile.isDirectory()) {
//      for (doc <- docs) {
//        val writer = IOUtils.openOutHard(outputFile.getAbsolutePath() + "/" + doc.docID)
////        ConllDocWriter.writeDoc(writer, doc, new OrderedClusteringBound(Seq[Mention](), new OrderedClustering(Seq[Seq[Int]]())))
//        writer.close
//      }
//    } else {
//      val writer = IOUtils.openOutHard(outputFile)
//      for (doc <- docs) {
//        ConllDocWriter.writeDoc(writer, doc, new OrderedClusteringBound(Seq[Mention](), new OrderedClustering(Seq[Seq[Int]]())))
//      }
//      writer.close
//    }
//    LightRunner.finalizeOutput();
  }
  
  def readParsesMakeDoc(file: File) = {
    val lineItr = IOUtils.lineIterator(IOUtils.openInHard(file))
    val words = new ArrayBuffer[Seq[String]]
    val pos = new ArrayBuffer[Seq[String]]
    val trees = new ArrayBuffer[DepConstTree]
    while (lineItr.hasNext) {
      val currLine = lineItr.next.trim
      if (currLine != "" && currLine != "(())") {
        val currParse = PennTreeReader.parseHard(currLine, false)
        val currDepConstTree = DepConstTree.apply(currParse)
        words += currDepConstTree.words
        pos += currDepConstTree.pos
        trees += currDepConstTree
      }
    }
    val nerChunks = (0 until words.size).map(i => Seq[Chunk[String]]())
    val corefChunks = (0 until words.size).map(i => Seq[Chunk[Int]]())
    val speakers = (0 until words.size).map(i => (0 until words(i).size).map(j => "-"))
    new ConllDoc(file.getName(), 0, words, pos, trees, nerChunks, corefChunks, speakers)
  }
}