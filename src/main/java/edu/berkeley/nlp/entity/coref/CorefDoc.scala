package edu.berkeley.nlp.entity.coref
import java.io.File
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.mapAsScalaMapConverter
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.futile.syntax.Trees.PennTreeRenderer
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.entity.Document

case class CorefDoc(val rawDoc: Document,
                    val goldMentions: Seq[Mention],
                    val goldClustering: OrderedClustering,
                    val predMentions: Seq[Mention]) {
  
  var oraclePredOrderedClustering: OrderedClustering = null;
  
  def numPredMents = predMentions.size;
  
  def isInGold(predMent: Mention): Boolean = {
    goldMentions.filter(goldMent => goldMent.sentIdx == predMent.sentIdx && goldMent.startIdx == predMent.startIdx && goldMent.endIdx == predMent.endIdx).size > 0;
  }
  
  def getGoldMentionF1SuffStats: (Int, Int, Int) = {
    (predMentions.filter(isInGold(_)).size, predMentions.size, goldMentions.size)
  }
  
  /**
   * Determines and caches an "oracle predicted clustering." For each predicted mention:
   * --If that mention does not have a corresponding gold mention (start and end indices match):
   *   --Put the current mention in its own cluster.
   * --If that mention does have a corresponding gold mention:
   *   --Fetch that mention's antecedents (if any)
   *   --Choose the first with a corresponding predicted mention (if any)
   *   --Assign this mention as the current mention's parent.
   */
  def getOraclePredClustering = {
    if (oraclePredOrderedClustering == null) {
      val predToGoldIdxMap = new HashMap[Int,Int]();
      val goldToPredIdxMap = new HashMap[Int,Int]();
      for (pIdx <- 0 until predMentions.size) {
        for (gIdx <- 0 until goldMentions.size) {
          val pMent = predMentions(pIdx);
          val gMent = goldMentions(gIdx);
          if (pMent.sentIdx == gMent.sentIdx && pMent.startIdx == gMent.startIdx && pMent.endIdx == gMent.endIdx) {
            predToGoldIdxMap.put(pIdx, gIdx);
            goldToPredIdxMap.put(gIdx, pIdx);
          }
        }
      }
      val oracleClusterIds = new ArrayBuffer[Int];
      var nextClusterId = 0;
      for (predIdx <- 0 until predMentions.size) {
        // Fetch the parent
        var parent = -1;
        if (predToGoldIdxMap.contains(predIdx)) {
          val correspondingGoldIdx = predToGoldIdxMap(predIdx);
          // Find the antecedents of the corresponding gold mention
          val goldAntecedentIdxs = goldClustering.getAllAntecedents(correspondingGoldIdx);
          // For each one, do a weird data sanitizing check, then try to find a corresponding
          // predicted mention to use as the predicted parent
          for (goldAntecedentIdx <- goldAntecedentIdxs.reverse) {
            val correspondingGold = goldMentions(correspondingGoldIdx);
            val goldAntecedent = goldMentions(goldAntecedentIdx);
            // wsj_0990 has some duplicate gold mentions, need to handle these...
            val sameMention = goldAntecedent.sentIdx == correspondingGold.sentIdx && goldAntecedent.startIdx == correspondingGold.startIdx && goldAntecedent.endIdx == correspondingGold.endIdx
            if (!sameMention && goldToPredIdxMap.contains(goldAntecedentIdx)) {
              val predAntecedentIdx = goldToPredIdxMap(goldAntecedentIdx)
              if (predAntecedentIdx >= predIdx) {
                val ment = predMentions(predIdx);
                val predAntecedent = predMentions(predAntecedentIdx);
                Logger.logss("Monotonicity violated:\n" +
                          "Antecedent(" + predAntecedentIdx + "): " + predAntecedent.startIdx + " " + predAntecedent.endIdx + " " + predAntecedent.headIdx + "\n" +
                          "Current(" + predMentions.indexOf(ment) + "): " + ment.startIdx + " " + ment.endIdx + " " + ment.headIdx + "\n" +
                          "Gold antecedent(" + goldMentions.indexOf(goldAntecedent) + "): " + goldAntecedent.startIdx + " " + goldAntecedent.endIdx + " " + goldAntecedent.headIdx + "\n" +
                          "Gold current(" + goldMentions.indexOf(correspondingGold) + "): " + correspondingGold.startIdx + " " + correspondingGold.endIdx + " " + correspondingGold.headIdx);
                Logger.logss("Setting parent to -1...");
                parent = -1;
              } else {
                parent = predAntecedentIdx
              }
            }
          }
        }
        // Now compute the oracle cluster ID
        val clusterId = if (parent == -1) {
          nextClusterId += 1;
          nextClusterId - 1;
        } else {
          oracleClusterIds(parent);
        }
        oracleClusterIds += clusterId;
      }
      oraclePredOrderedClustering = OrderedClustering.createFromClusterIds(oracleClusterIds);
    }
    oraclePredOrderedClustering
  }
}

object CorefDoc {
  def displayMentionPRF1(docs: Seq[CorefDoc]) {
    val suffStats = docs.map(_.getGoldMentionF1SuffStats).reduce((a, b) => (a._1 + b._1, a._2 + b._2, a._3 + b._3));
    Logger.logss(GUtil.renderPRF1(suffStats._1, suffStats._2, suffStats._3))
  }
}
