package edu.berkeley.nlp.entity.coref

import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.entity.LikelihoodAndGradientComputerSparse
import edu.berkeley.nlp.futile.util.IntCounter
import edu.berkeley.nlp.entity.AdagradWeightVector
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.GeneralTrainer2
import edu.berkeley.nlp.futile.math.SloppyMath

class MentionRankingDocumentComputer(val featIdx: Indexer[String],
                                     val featurizer: PairwiseIndexingFeaturizer,
                                     val lossFcn: PairwiseLossFunction,
                                     val doSps: Boolean = false,
                                     val lossFromCurrWeights: Boolean = false,
                                     val lossFromGold: Boolean = false) extends LikelihoodAndGradientComputerSparse[DocumentGraph] {

  def getInitialWeights(initialWeightsScale: Double): Array[Double] = Array.tabulate(featIdx.size)(i => 0.0)
  
  private def computeMarginals(ex: DocumentGraph, weights: AdagradWeightVector, scores: Array[Array[Float]], gold: Boolean) = {
    val docGraph = ex
    val marginals = docGraph.cachedMarginalMatrix
    val losses = if (lossFromCurrWeights) {
      lossFcn.lossFromCurrPrediction(docGraph.corefDoc, docGraph.prunedEdges, MentionRankingDocumentComputer.viterbiDecode(ex, featurizer, weights))
    } else if (lossFromGold) {
      lossFcn.lossFromCurrPrediction(docGraph.corefDoc, docGraph.prunedEdges, ex.corefDoc.oraclePredOrderedClustering.getConsistentBackpointers)
    } else {
      lossFcn.loss(docGraph.corefDoc, docGraph.prunedEdges)
    }
    for (i <- 0 until docGraph.size) {
      // Restrict to gold antecedents if we're doing gold, but don't load the gold antecedents
      // if we're not.
      val goldAntecedents: Seq[Int] = if (gold) docGraph.getGoldAntecedentsUnderCurrentPruning(i) else null;
      // NON-LOG VERSION
//      var normalizer = 0.0F;
//      for (j <- 0 to i) {
//        // If this is a legal antecedent
//        if (!docGraph.isPruned(i, j) && (!gold || goldAntecedents.contains(j))) {
//          val score = scores(i)(j) + losses(i)(j)
//          val unnormalizedProb = Math.exp(score).toFloat
//          marginals(i)(j) = unnormalizedProb;
//          normalizer += unnormalizedProb;
//        } else {
//          marginals(i)(j) = 0.0F;
//        }
//      }
//      var total = 0.0
//      for (j <- 0 to i) {
//        marginals(i)(j) /= normalizer;
//        total += marginals(i)(j)
//      }
      // END NON-LOG VERSION
      // LOG-VERSION
      // This is as precise as anything using doubles throughout
      var logNormalizer = Double.NegativeInfinity
      for (j <- 0 to i) {
        // If this is a legal antecedent
        if (!docGraph.isPruned(i, j) && (!gold || goldAntecedents.contains(j))) {
          val unnormalizedLogProb = scores(i)(j) + losses(i)(j)
          marginals(i)(j) = unnormalizedLogProb.toFloat;
          logNormalizer = SloppyMath.logAdd(logNormalizer, unnormalizedLogProb)
        } else {
          marginals(i)(j) = Float.NegativeInfinity;
        }
      }
      var total = 0.0
      for (j <- 0 to i) {
        marginals(i)(j) = Math.exp(marginals(i)(j) - logNormalizer).toFloat;
        total += marginals(i)(j)
      }
      // END LOG-VERSION
      if (Math.abs(total - 1.0) > 0.1) {
        throw new RuntimeException("Total has diverged; numerical problems! " + total)
      }
    }
    marginals
  }
  
  private def computeMax(ex: DocumentGraph, weights: AdagradWeightVector, scores: Array[Array[Float]], gold: Boolean): (Array[Int],Double) = {
    val docGraph = ex
    var totalScore = 0.0
    val losses = if (lossFromCurrWeights) {
      lossFcn.lossFromCurrPrediction(docGraph.corefDoc, docGraph.prunedEdges, MentionRankingDocumentComputer.viterbiDecode(ex, featurizer, weights))
    } else if (lossFromGold) {
      lossFcn.lossFromCurrPrediction(docGraph.corefDoc, docGraph.prunedEdges, ex.corefDoc.oraclePredOrderedClustering.getConsistentBackpointers)
    } else {
      lossFcn.loss(docGraph.corefDoc, docGraph.prunedEdges)
    }
    val results = Array.tabulate(docGraph.size)(i => {
      var bestIdx = -1
      var bestScore = Float.NegativeInfinity;
      // Restrict to gold antecedents if we're doing gold, but don't load the gold antecedents
      // if we're not.
      val goldAntecedents: Seq[Int] = if (gold) docGraph.getGoldAntecedentsUnderCurrentPruning(i) else null;
      for (j <- 0 to i) {
        // If this is a legal antecedent
        if (!docGraph.isPruned(i, j) && (!gold || goldAntecedents.contains(j))) {
          // N.B. Including lossFcn is okay even for gold because it should be zero
          val score = (scores(i)(j) + losses(i)(j)).toFloat;
//          val score = scores(j) + lossFcn.loss(docGraph.corefDoc, i, j).toFloat;
          if (bestIdx == -1 || score > bestScore) {
            bestIdx = j
            bestScore = score
          }
        }
      }
      totalScore += bestScore
      bestIdx
    })
    results -> totalScore
  }
  
  def accumulateGradientAndComputeObjective(ex: DocumentGraph, weights: AdagradWeightVector, gradient: IntCounter): Double = {
    val (featsChart, scores) = MentionRankingDocumentComputer.computeFeatsScores(ex, featurizer, weights)
    if (doSps) {
      val (predMax, predScore) = computeMax(ex, weights, scores, false)
      val (goldMax, goldScore) = computeMax(ex, weights, scores, true)
      for (i <- 0 until ex.size) {
        if (predMax(i) != goldMax(i)) {
          GeneralTrainer2.addToGradient(featsChart(i)(predMax(i)), -1.0, gradient)
          GeneralTrainer2.addToGradient(featsChart(i)(goldMax(i)), 1.0, gradient)
        }
      }
      predScore - goldScore
    } else {
      // N.B. pred and gold marginals live in the same marginals matrix so don't have them
      // both around at the same time
      val predMarginals = computeMarginals(ex, weights, scores, false);
      var totalLogProb = 0.0
      for (i <- 0 until ex.size) {
        val goldAntecedents = ex.getGoldAntecedentsUnderCurrentPruning(i);
        // Pred terms in gradient and likelihood computation
        var currProb = 0.0
        for (j <- 0 to i) {
          if (predMarginals(i)(j) > 1e-20) {
            GeneralTrainer2.addToGradient(featsChart(i)(j), -predMarginals(i)(j).toDouble, gradient);
            if (goldAntecedents.contains(j)) {
              currProb += predMarginals(i)(j)
            }
          }
        }
        var currLogProb = Math.log(currProb).toFloat;
        if (currLogProb.isInfinite()) {
          currLogProb = -30;
        }
        totalLogProb += currLogProb
      }
      // Gold terms in gradient
      val goldMarginals = computeMarginals(ex, weights, scores, true);
      for (i <- 0 until ex.size) {
        for (j <- 0 to i) {
          if (goldMarginals(i)(j) > 1e-20) {
            GeneralTrainer2.addToGradient(featsChart(i)(j), goldMarginals(i)(j).toDouble, gradient);
          }
        }
      }
      totalLogProb
    }
  }
  
  def computeObjective(ex: DocumentGraph, weights: AdagradWeightVector): Double = {
    accumulateGradientAndComputeObjective(ex, weights, new IntCounter)
  }
}

object MentionRankingDocumentComputer {
  
  def computeFeatsScores(ex: DocumentGraph, featurizer: PairwiseIndexingFeaturizer, weights: AdagradWeightVector): (Array[Array[Array[Int]]], Array[Array[Float]]) = {
    val docGraph = ex
    val featsChart = docGraph.featurizeIndexNonPrunedUseCache(featurizer)
    val scoreMat = docGraph.cachedScoreMatrix;
    for (i <- 0 until docGraph.size) {
      for (j <- 0 to i) {
        if (!docGraph.prunedEdges(i)(j)) {
          require(featsChart(i)(j).size > 0);
          scoreMat(i)(j) = weights.score(featsChart(i)(j)).toFloat;
        } else {
          scoreMat(i)(j) = Float.NegativeInfinity;
        }
      }
    }
    featsChart -> scoreMat
  }
  
  def viterbiDecode(ex: DocumentGraph, featurizer: PairwiseIndexingFeaturizer, weights: AdagradWeightVector): Array[Int] = {
    val docGraph = ex
    val scores = computeFeatsScores(ex, featurizer, weights)._2
    val results = Array.tabulate(docGraph.size)(i => {
      var bestIdx = -1
      var bestScore = Float.NegativeInfinity;
      for (j <- 0 to i) {
        // If this is a legal antecedent
        if (!docGraph.isPruned(i, j)) {
          val score = scores(i)(j)
          if (bestIdx == -1 || score > bestScore) {
            bestIdx = j
            bestScore = score
          }
        }
      }
      bestIdx
    })
    results
  }
}
