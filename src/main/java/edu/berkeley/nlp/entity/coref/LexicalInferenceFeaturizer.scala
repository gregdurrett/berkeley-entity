package edu.berkeley.nlp.entity.coref

import scala.collection.mutable.ArrayBuffer
import java.util.IdentityHashMap
import scala.collection.mutable.HashSet
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.GUtil
import scala.collection.mutable.HashMap

class LexicalInferenceFeaturizer(val lexInfDB: HashMap[(String,String),Seq[String]],
                                 val usePathFeatures: Boolean) extends AuxiliaryFeaturizer {

  override def featurize(docGraph: DocumentGraph, currIdx: Int, antecedentIdx: Int): Seq[String] = {
    val feats = new ArrayBuffer[String]
    val curr = docGraph.getMention(currIdx)
    val ant = docGraph.getMention(antecedentIdx)
    if (!curr.mentionType.isClosedClass() && !ant.mentionType.isClosedClass()) {
      val currText = curr.spanToString
      val antText = ant.spanToString
      val forwardContained = lexInfDB.contains(antText -> currText)
      feats += "LI=" + forwardContained
      if (usePathFeatures && forwardContained) {
        val rels = lexInfDB(antText -> currText)
        if (!rels.isEmpty) {
          for (rel <- rels) {
            feats += "LIPathContains=" + rel
          }
        }
      }
      val reverseContained = lexInfDB.contains(currText -> antText)
      feats += "LIRev=" + reverseContained
      if (usePathFeatures && reverseContained) {
        val rels = lexInfDB(currText -> antText)
        if (!rels.isEmpty) {
          for (rel <- rels) {
            feats += "LIRevPathContains=" + rel
          }
        }
      }
    }
    feats
  }
}

//class LexicalInferenceOracleFeaturizer(val lexInfDB: HashMap[(String,String),Seq[String]]) extends AuxiliaryFeaturizer {
//
//  override def featurize(docGraph: DocumentGraph, currIdx: Int, antecedentIdx: Int): Seq[String] = {
//    val feats = new ArrayBuffer[String]
//    val curr = docGraph.getMention(currIdx)
//    val ant = docGraph.getMention(antecedentIdx)
//    if (!curr.mentionType.isClosedClass() && !ant.mentionType.isClosedClass()) {
//      val currText = curr.spanToString
//      val antText = ant.spanToString
//      val forwardContained = lexInfDB.contains(antText -> currText)
//      if (forwardContained) {
//        val areGold = docGraph.corefDoc.getOraclePredClustering.areInSameCluster(currIdx, antecedentIdx)
//        if (areGold) {
//          feats += "OracleIncluded"
//        }
//      }
//    }
//    feats
//  }
//}

object LexicalInferenceFeaturizer {
  def loadLexInfFeaturizer(lexInfResultsDir: String, usePathFeatures: Boolean) = {
    val lexInfDB = new HashMap[(String,String),Seq[String]];
    addFileToSet(lexInfDB, lexInfResultsDir + "/train.txt")
    addFileToSet(lexInfDB, lexInfResultsDir + "/dev.txt")
    addFileToSet(lexInfDB, lexInfResultsDir + "/test.txt")
    Logger.logss("Loaded " + lexInfDB.size + " true positive lexical inference pairs from " + lexInfResultsDir)
    new LexicalInferenceFeaturizer(lexInfDB, usePathFeatures)
  }
  
  def addFileToSet(map: HashMap[(String,String),Seq[String]], file: String) {
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
          if (lineSplit.size == 9) {
            // Drop ^ and $
            val relStr = lineSplit(8).drop(1).dropRight(1)
            map += (lineSplit(1) -> lineSplit(4)) -> relStr.split("\\s+").toSeq
          } else {
            map += (lineSplit(1) -> lineSplit(4)) -> Seq[String]()
          }
        }
      }
    }
    Logger.logss("Lexical inf accuracy in " + file + ": " + GUtil.renderPRF1(corr, pred, gold))
  }
}