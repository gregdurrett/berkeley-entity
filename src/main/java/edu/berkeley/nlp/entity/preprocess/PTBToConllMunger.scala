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
import edu.berkeley.nlp.futile.syntax.Tree
import scala.collection.JavaConverters._
import scala.collection.mutable.HashSet
import edu.berkeley.nlp.futile.syntax.Trees

/**
 * Takes either a file or directory as input, reads in PTB files one per line,
 * and writes them to a file or directory
 */
object PTBToConllMunger {

  val input = ""
  val output = ""
  // Changes trees from having "Mr ." as two tokens to "Mr."; this appears to be
  // a problem when using some tokenizers. Coref systems expect the latter.
  val fixAbbrevs = false
  val abbrevsToFix = new HashSet[String]() ++ Array[String]("Mr", "Mrs", "Ms", "Dr")
  
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
  }
  
  def readParsesMakeDoc(file: File) = {
    val inBuffer = IOUtils.openInHard(file)
    val lineItr = IOUtils.lineIterator(inBuffer)
    val words = new ArrayBuffer[Seq[String]]
    val pos = new ArrayBuffer[Seq[String]]
    val trees = new ArrayBuffer[DepConstTree]
    while (lineItr.hasNext) {
      val currLine = lineItr.next.trim
      if (currLine != "" && currLine != "(())") {
        val currParseRaw = PennTreeReader.parseHard(currLine, false)
        val currParse = if (fixAbbrevs) {
          fixAbbrevs(currParseRaw)
        } else {
          currParseRaw
        }
        val currDepConstTree = DepConstTree.apply(currParse)
        words += currDepConstTree.words
        pos += currDepConstTree.pos
        trees += currDepConstTree
      }
    }
    inBuffer.close()
    val nerChunks = (0 until words.size).map(i => Seq[Chunk[String]]())
    val corefChunks = (0 until words.size).map(i => Seq[Chunk[Int]]())
    val speakers = (0 until words.size).map(i => (0 until words(i).size).map(j => "-"))
    new ConllDoc(file.getName(), 0, words, pos, trees, nerChunks, corefChunks, speakers)
  }
  
  def fixAbbrevs(tree: Tree[String]): Tree[String] = {
    val treeYield = tree.getYield().asScala
    val abbrevIndices = new ArrayBuffer[Int]
    for (abbrev <- abbrevsToFix) {
      var startIdx = 0
      var currIdx = treeYield.indexOf(abbrev, startIdx)
      while (currIdx != -1) {
        abbrevIndices += currIdx
        startIdx = currIdx + 1
        currIdx = treeYield.indexOf(abbrev, startIdx)
      }
    }
    if (abbrevIndices.size == 0) {
      tree
    } else {
      // The transformation could theoretically product X over X so redo this transformation
      new Trees.XOverXRemover().transformTree(transformFixAbbrevs(tree, 0, treeYield.size, abbrevIndices.sorted))
    }
  }
  
  /**
   * Need to do two things to fix abbreviations: add . to the abbreviation and remove the . token
   */
  def transformFixAbbrevs(tree: Tree[String], startIdx: Int, endIdx: Int, abbrevIndices: Seq[Int]): Tree[String] = {
    // Leaf: fix the abbreviation label if necessary
    if (tree.isLeaf()) {
      if (abbrevIndices.contains(startIdx)) {
        new Tree[String](tree.getLabel() + ".")
      } else {
        tree
      }
    } else {
//    } else if (tree.isPreTerminal()) {
//      new Tree[String](tree.getLabel(), transformFixAbbrevs(tree.getChildren().get(0), startIdx, endIdx, abbrevIndices)
//    } else {
      // Select things that either contain this index or the next (the .)
      val matchingAbbrevIndices = abbrevIndices.filter(idx => startIdx <= idx + 1 && idx < endIdx)
      if (matchingAbbrevIndices.size == 0) {
        tree
      } else {
        val children = tree.getChildren().asScala
        var currIdx = startIdx
        val newChildren = new ArrayBuffer[Tree[String]]
        for (child <- children) {
          val childYield = child.getYield().asScala
          val childYieldSize = childYield.size
          val currEndIdx = currIdx + childYieldSize
          // If this child only dominates the offending period
          if (matchingAbbrevIndices.contains(currIdx - 1) && childYieldSize == 1 && childYield.head == ".") {
            // Delete this child by doing nothing
          } else {
            // Otherwise proceed as normal
            newChildren += transformFixAbbrevs(child, currIdx, currEndIdx, abbrevIndices)
          }
          currIdx += childYieldSize
        }
        new Tree[String](tree.getLabel(), newChildren.asJava)
      }
    }
  }
}