package edu.berkeley.nlp.entity.coref
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.entity.Driver;
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.util.Counter
import scala.collection.JavaConverters._

class DocumentInferencerBasic extends DocumentInferencer {
  
  def getInitialWeightVector(featureIndexer: Indexer[String]): Array[Float] = Array.fill(featureIndexer.size())(0.0F);
  
  /**
   * N.B. always returns a reference to the same matrix, so don't call twice in a row and
   * attempt to use the results of both computations
   */
  def computeMarginals(docGraph: DocumentGraph,
                       gold: Boolean,
                       lossFcn: (CorefDoc, Int, Int) => Float,
                       pairwiseScorer: PairwiseScorer): Array[Array[Float]] = {
    computeMarginals(docGraph, gold, lossFcn, docGraph.featurizeIndexAndScoreNonPrunedUseCache(pairwiseScorer)._2)
  }
  
  def computeMarginals(docGraph: DocumentGraph,
                       gold: Boolean,
                       lossFcn: (CorefDoc, Int, Int) => Float,
                       scoresChart: Array[Array[Float]]): Array[Array[Float]] = {
    val marginals = docGraph.cachedMarginalMatrix;
    for (i <- 0 until docGraph.size) {
      var normalizer = 0.0F;
      // Restrict to gold antecedents if we're doing gold, but don't load the gold antecedents
      // if we're not.
      val goldAntecedents: Seq[Int] = if (gold) docGraph.getGoldAntecedentsUnderCurrentPruning(i) else null;
      for (j <- 0 to i) {
        // If this is a legal antecedent
        if (!docGraph.isPruned(i, j) && (!gold || goldAntecedents.contains(j))) {
          // N.B. Including lossFcn is okay even for gold because it should be zero
          val unnormalizedProb = Math.exp(scoresChart(i)(j) + lossFcn(docGraph.corefDoc, i, j)).toFloat;
          marginals(i)(j) = unnormalizedProb;
          normalizer += unnormalizedProb;
        } else {
          marginals(i)(j) = 0.0F;
        }
      }
      for (j <- 0 to i) {
        marginals(i)(j) /= normalizer;
      }
    }
    marginals;
  }
  
  def computeLikelihood(docGraph: DocumentGraph,
                        pairwiseScorer: PairwiseScorer,
                        lossFcn: (CorefDoc, Int, Int) => Float): Float = {
    var likelihood = 0.0F;
    val marginals = computeMarginals(docGraph, false, lossFcn, pairwiseScorer);
    for (i <- 0 until docGraph.size) {
      val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(i);
      var currProb = 0.0;
      for (j <- goldAntecedents) {
        currProb += marginals(i)(j);
      }
      var currLogProb = Math.log(currProb).toFloat;
      if (currLogProb.isInfinite()) {
        currLogProb = -30;
      }
      likelihood += currLogProb;
    }
    likelihood;
  }
  
  def addUnregularizedStochasticGradient(docGraph: DocumentGraph,
                                         pairwiseScorer: PairwiseScorer,
                                         lossFcn: (CorefDoc, Int, Int) => Float,
                                         gradient: Array[Float]) = {
    val (featsChart, scoresChart) = docGraph.featurizeIndexAndScoreNonPrunedUseCache(pairwiseScorer);
    // N.B. Can't have pred marginals and gold marginals around at the same time because
    // they both live in the same cached matrix
    val predMarginals = this.computeMarginals(docGraph, false, lossFcn, scoresChart);
    for (i <- 0 until docGraph.size) {
      for (j <- 0 to i) {
        if (predMarginals(i)(j) > 1e-20) {
          GUtil.addToGradient(featsChart(i)(j), -predMarginals(i)(j), gradient);
        }
      }
    }
    val goldMarginals = this.computeMarginals(docGraph, true, lossFcn, scoresChart);
    for (i <- 0 until docGraph.size) {
      for (j <- 0 to i) {
        if (goldMarginals(i)(j) > 1e-20) {
          GUtil.addToGradient(featsChart(i)(j), goldMarginals(i)(j), gradient);
        }
      }
    }
  }

  def viterbiDecode(docGraph: DocumentGraph, scorer: PairwiseScorer): Array[Int] = {
    val (featsChart, scoresChart) = docGraph.featurizeIndexAndScoreNonPrunedUseCache(scorer);
    viterbiDecode(scoresChart);
  }
  
  def viterbiDecode(scoresChart: Array[Array[Float]]) = {
    val scoreFcn = (idx: Int) => scoresChart(idx);
    DocumentInferencerBasic.decodeMax(scoresChart.size, scoreFcn);
  }
  
  def finishPrintStats() = {}
}

object DocumentInferencerBasic {
  
  def decodeMax(size: Int, scoreFcn: Int => Array[Float]): Array[Int] = {
    val backpointers = new Array[Int](size);
    for (i <- 0 until size) {
      val allScores = scoreFcn(i);
      var bestIdx = -1;
      var bestScore = Float.NegativeInfinity;
      for (j <- 0 to i) {
        val currScore = allScores(j);
        if (bestIdx == -1 || currScore > bestScore) {
          bestIdx = j;
          bestScore = currScore;
        }
      }
      backpointers(i) = bestIdx;
    }
    backpointers;
  }
  
}
