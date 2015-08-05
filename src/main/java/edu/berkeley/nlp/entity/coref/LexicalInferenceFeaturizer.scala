package edu.berkeley.nlp.entity.coref

import scala.collection.mutable.ArrayBuffer
import java.util.IdentityHashMap
import scala.collection.mutable.HashSet
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.GUtil

class LexicalInferenceFeaturizer(val lexInfDB: HashSet[(String,String)]) extends AuxiliaryFeaturizer {

  override def featurize(docGraph: DocumentGraph, currIdx: Int, antecedentIdx: Int): Seq[String] = {
    val feats = new ArrayBuffer[String]
    val curr = docGraph.getMention(currIdx)
    val ant = docGraph.getMention(antecedentIdx)
    if (!curr.mentionType.isClosedClass() && !ant.mentionType.isClosedClass()) {
      val currText = curr.spanToString
      val antText = ant.spanToString
      feats += (if (lexInfDB.contains(antText -> currText)) "LI=True" else "LI=False")
      feats += (if (lexInfDB.contains(currText -> antText)) "LIRev=True" else "LIRev=False")
    }
    feats
  }
}

object LexicalInferenceFeaturizer {
  def loadLexInfFeaturizer(lexInfResultsDir: String = "data/lexinf/results/") = {
    val lexInfDB = new HashSet[(String,String)];
    addFileToSet(lexInfDB, lexInfResultsDir + "/train.txt")
    addFileToSet(lexInfDB, lexInfResultsDir + "/dev.txt")
    addFileToSet(lexInfDB, lexInfResultsDir + "/test.txt")
    Logger.logss("Loaded " + lexInfDB.size + " true positive lexical inference pairs from " + lexInfResultsDir)
    new LexicalInferenceFeaturizer(lexInfDB)
  }
  
  def addFileToSet(set: HashSet[(String,String)], file: String) {
    val lineItr = IOUtils.lineIterator(IOUtils.openInHard(file))
    var corr = 0
    var pred = 0
    var gold = 0
    while (lineItr.hasNext) {
      val line = lineItr.next
      if (!line.trim.isEmpty) {
        val lineSplit = line.split("\\t")
        // Gold and prediction
        val goldTrue = lineSplit(6).toLowerCase().startsWith("t")
        val predTrue = lineSplit(7).toLowerCase().startsWith("t")
        if (goldTrue && predTrue) corr += 1
        if (goldTrue) gold += 1
        if (predTrue) pred += 1
        if (lineSplit(7).toLowerCase().startsWith("t")) {
          set += (lineSplit(1) -> lineSplit(4))
        }
      }
    }
    Logger.logss("Lexical inf accuracy in " + file + ": " + GUtil.renderPRF1(corr, pred, gold))
  }
}