package edu.berkeley.nlp.entity.coref

import edu.berkeley.nlp.entity.LikelihoodAndGradientComputer
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.entity.GUtil

class MentionRankingComputer(val featIdx: Indexer[String],
                             val featurizer: PairwiseIndexingFeaturizer,
                             val lossFcn: PairwiseLossFunction,
                             val doSps: Boolean = false,
                             val useDownstreamLoss: Boolean = false) extends LikelihoodAndGradientComputer[(DocumentGraph,Int)] {

  def getInitialWeights(initialWeightsScale: Double): Array[Double] = Array.tabulate(featIdx.size)(i => 0.0)
  
  private def computeFeatsScores(ex: (DocumentGraph,Int), weights: Array[Double]): (Array[Array[Int]], Array[Float]) = {
    val docGraph = ex._1
    val i = ex._2
    val featsChart = docGraph.featurizeIndexNonPrunedUseCache(featurizer)(i)
    val scoreVec = docGraph.cachedScoreMatrix(i);
    for (j <- 0 to i) {
      if (!docGraph.prunedEdges(i)(j)) {
        require(featsChart(j).size > 0);
        scoreVec(j) = GUtil.scoreIndexedFeatsDouble(featsChart(j), weights).toFloat;
      } else {
        scoreVec(j) = Float.NegativeInfinity;
      }
    }
    featsChart -> scoreVec
  }
  
  private def computeMarginals(ex: (DocumentGraph,Int), scores: Array[Float], gold: Boolean) = {
    val docGraph = ex._1
    val i = ex._2
    val marginals = docGraph.cachedMarginalMatrix(i)
    var normalizer = 0.0F;
    // Restrict to gold antecedents if we're doing gold, but don't load the gold antecedents
    // if we're not.
    val goldAntecedents: Seq[Int] = if (gold) docGraph.getGoldAntecedentsUnderCurrentPruning(i) else null;
    val losses = lossFcn.loss(docGraph.corefDoc, i, docGraph.prunedEdges)
    for (j <- 0 to i) {
      // If this is a legal antecedent
      if (!docGraph.isPruned(i, j) && (!gold || goldAntecedents.contains(j))) {
        // N.B. Including lossFcn is okay even for gold because it should be zero
        val score = scores(j) + losses(j)
        val unnormalizedProb = Math.exp(score).toFloat
//        val unnormalizedProb = Math.exp(scores(j) + lossFcn.loss(docGraph.corefDoc, i, j)).toFloat;
        marginals(j) = unnormalizedProb;
        normalizer += unnormalizedProb;
      } else {
        marginals(j) = 0.0F;
      }
    }
    for (j <- 0 to i) {
      marginals(j) /= normalizer;
    }
    marginals
  }
  
  private def computeMax(ex: (DocumentGraph,Int), scores: Array[Float], gold: Boolean): (Int, Double) = {
    val docGraph = ex._1
    val i = ex._2
    var bestIdx = -1
    var bestScore = Float.NegativeInfinity;
    // Restrict to gold antecedents if we're doing gold, but don't load the gold antecedents
    // if we're not.
    val goldAntecedents: Seq[Int] = if (gold) docGraph.getGoldAntecedentsUnderCurrentPruning(i) else null;
    val losses = lossFcn.loss(docGraph.corefDoc, i, docGraph.prunedEdges)
    for (j <- 0 to i) {
      // If this is a legal antecedent
      if (!docGraph.isPruned(i, j) && (!gold || goldAntecedents.contains(j))) {
        // N.B. Including lossFcn is okay even for gold because it should be zero
        val score = (scores(j) + losses(j)).toFloat;
//        val score = scores(j) + lossFcn.loss(docGraph.corefDoc, i, j).toFloat;
        if (bestIdx == -1 || score > bestScore) {
          bestIdx = j
          bestScore = score
        }
      }
    }
    bestIdx -> bestScore
  }
  
  def accumulateGradientAndComputeObjective(ex: (DocumentGraph,Int), weights: Array[Double], gradient: Array[Double]): Double = {
    val docGraph = ex._1
    val i = ex._2
    val (featsChart, scores) = computeFeatsScores(ex, weights)
    if (doSps) {
      val (predMax, predScore) = computeMax(ex, scores, false)
      val (goldMax, goldScore) = computeMax(ex, scores, true)
      if (predMax != goldMax) {
        GUtil.addToGradientDouble(featsChart(predMax), -1.0, gradient)
        GUtil.addToGradientDouble(featsChart(goldMax), 1.0, gradient)
        predScore - goldScore
      } else {
        0.0 // no gap
      }
    } else {
      // N.B. pred and gold marginals live in the same marginals matrix so don't have them
      // both around at the same time
      val predMarginals = computeMarginals(ex, scores, false);
      val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(i);
      // Pred terms in gradient and likelihood computation
      var currProb = 0.0
      for (j <- 0 to i) {
        if (predMarginals(j) > 1e-20) {
          GUtil.addToGradientDouble(featsChart(j), -predMarginals(j).toDouble, gradient);
          if (goldAntecedents.contains(j)) {
            currProb += predMarginals(j)
          }
        }
      }
      var currLogProb = Math.log(currProb).toFloat;
      if (currLogProb.isInfinite()) {
        currLogProb = -30;
      }
      // Gold terms in gradient
      val goldMarginals = computeMarginals(ex, scores, true);
      for (j <- 0 to i) {
        if (goldMarginals(j) > 1e-20) {
          GUtil.addToGradientDouble(featsChart(j), goldMarginals(j).toDouble, gradient);
        }
      }
      currLogProb
    }
  }
  
  def computeObjective(ex: (DocumentGraph,Int), weights: Array[Double]): Double = {
    accumulateGradientAndComputeObjective(ex, weights, Array.tabulate(weights.size)(i => 0.0))
  }
}