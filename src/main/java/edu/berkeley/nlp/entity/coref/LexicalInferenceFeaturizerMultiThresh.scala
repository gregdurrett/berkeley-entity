package edu.berkeley.nlp.entity.coref

import scala.collection.mutable.ArrayBuffer
import java.util.IdentityHashMap
import scala.collection.mutable.HashSet
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.GUtil
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.entity.WordNetInterfacer

/**
 * N.B. Everything is lowercased here, unlike in the other one
 */
class LexicalInferenceFeaturizerMultiThresh(val lexInfDB: HashMap[(String,String),Seq[Int]],
                                            val indicesToUse: Array[Int],
                                            val wni: Option[WordNetInterfacer],
                                            val useSemClassConj: Boolean) extends AuxiliaryFeaturizer {
  
  val scCache = new HashMap[(UID,Int),String]
  
  var numHits = 0
  var totalAttempted = 0

  override def featurize(docGraph: DocumentGraph, currIdx: Int, antecedentIdx: Int): Seq[String] = {
    val feats = new ArrayBuffer[String]
    val curr = docGraph.getMention(currIdx)
    val ant = docGraph.getMention(antecedentIdx)
    if (!curr.mentionType.isClosedClass() && !ant.mentionType.isClosedClass()) {
//      val currText = curr.spanToString.toLowerCase()
//      val antText = ant.spanToString.toLowerCase()
      val currText = LexicalInferenceExtractor.extractShortLabel(curr).toLowerCase()
      val antText = LexicalInferenceExtractor.extractShortLabel(ant).toLowerCase()
      val key = antText -> currText
      val forwardContained = lexInfDB.contains(key)
      totalAttempted += 1
      if (forwardContained) {
        numHits += 1
      }
      val prefix = "LI"
      feats += prefix + forwardContained
      if (forwardContained) {
        val lexInfJudgments = lexInfDB(key)
        val numTrues = lexInfJudgments.filter(_ == 2).size
        val numFalses = lexInfJudgments.filter(_ == 1).size
        if (numTrues == 0) {
          if (numFalses == 0) {
            feats += prefix + "=AllUnk"
          } else {
            feats += prefix + "=AllFalse"
          }
        } else {
          for (index <- indicesToUse) {
            feats += prefix + "-" + index + "=" + lexInfJudgments(index)
          }
          feats += prefix + "NumTrues=" + bucket(numTrues)
        }
      }
      val keyRev = currText -> antText
      val reverseContained = lexInfDB.contains(keyRev)
      feats += prefix + "Rev=" + reverseContained
      if (reverseContained) {
        val lexInfJudgmentsRev = lexInfDB(keyRev)
        val numTrues = lexInfJudgmentsRev.filter(_ == 2).size
        val numFalses = lexInfJudgmentsRev.filter(_ == 1).size
        if (numTrues == 0) {
          if (numFalses == 0) {
            feats += prefix + "Rev=AllUnk"
          } else {
            feats += prefix + "Rev=AllFalse"
          }
        } else {
          for (index <- indicesToUse) {
            feats += prefix + "Rev-" + index + "=" + lexInfJudgmentsRev(index)
          }
          feats += prefix + "RevNumTrues=" + bucket(numTrues)
        }
      }
      if (useSemClassConj) {
        // Semantic class conjunctions
        val currSc = if (wni.isDefined) {
        val key = docGraph.corefDoc.rawDoc.uid -> currIdx
          if (!scCache.contains(key)) {
            scCache += key -> curr.computeSemClass(wni.get).toString
          }
          scCache(key)
        } else {
          ""
        }
        val antSc = if (wni.isDefined) {
          val key = docGraph.corefDoc.rawDoc.uid -> antecedentIdx
          if (!scCache.contains(key)) {
            scCache += key -> ant.computeSemClass(wni.get).toString
          }
          scCache(key)
        } else {
          ""
        }
        val suffix = "-" + currSc + antSc
        for (feat <- feats.toSeq) {
          feats += feat + suffix
        }
      }
    }
    feats
  }
  
  val bucketBoundaries = Array(0, 1, 2, 3, 4, 5, 8, 16, 32, 64)
  
  def bucket(num: Int) = {
    var i = 0;
    while (i < bucketBoundaries.size && bucketBoundaries(i) < num) {
      i += 1
    }
    i
  }
  
  
  def displayHitRate {
    Logger.logss("Lexical inferencer hit rate: " + GUtil.renderNumerDenom(numHits, totalAttempted))
  }
}

object LexicalInferenceFeaturizerMultiThresh {
  def loadLexInfFeaturizer(lexInfResultsDir: String, indicesToUse: Array[Int], wni: Option[WordNetInterfacer], useSemClassConj: Boolean) = {
    val lexInfDB = new HashMap[(String,String),Seq[Int]];
    addFileToSet(lexInfDB, lexInfResultsDir + "/train.txt")
    addFileToSet(lexInfDB, lexInfResultsDir + "/dev.txt")
    addFileToSet(lexInfDB, lexInfResultsDir + "/test.txt")
    Logger.logss("Loaded " + lexInfDB.size + " true positive lexical inference pairs from " + lexInfResultsDir)
    new LexicalInferenceFeaturizerMultiThresh(lexInfDB, indicesToUse, wni, useSemClassConj)
  }
  
  def addFileToSet(map: HashMap[(String,String),Seq[Int]], file: String) {
    val lineItr = IOUtils.lineIterator(IOUtils.openInHard(file))
    while (lineItr.hasNext) {
      val line = lineItr.next
      if (!line.trim.isEmpty) {
        val lineSplit = line.split("\\t")
        map += (lineSplit(0).toLowerCase -> lineSplit(1).toLowerCase) -> lineSplit.slice(2, lineSplit.size).map(entry => if (entry == "Unknown") 0 else if (entry == "False") 1 else 2)
      }
    }
  }
}