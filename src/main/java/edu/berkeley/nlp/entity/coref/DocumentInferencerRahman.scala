package edu.berkeley.nlp.entity.coref
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.fig.basic.Indexer

case class MentClusterMapping(val maxSize: Int) {
  val mentsToClusters = new Array[Int](maxSize);
  val clustersToMents = new ArrayBuffer[ArrayBuffer[Int]]();
  var nextClusterIdx = 0;
  
  def clear() {
    clustersToMents.clear;
    nextClusterIdx = 0;
  }
  
  def getCluster(mentIdx: Int): ArrayBuffer[Int] = {
    clustersToMents(mentsToClusters(mentIdx));
  }
  
  def populateWithGold(docGraph: DocumentGraph) {
    for (i <- 0 until docGraph.size) {
      val antecedent = docGraph.getGoldAntecedentsUnderCurrentPruning(i)(0);
      updateWithAssignment(i, antecedent);
    }
  }
  
  def updateWithAssignment(mentIdx: Int, assignmentIdx: Int) {
    if (assignmentIdx == mentIdx) {
      mentsToClusters(mentIdx) = nextClusterIdx;
      clustersToMents += ArrayBuffer[Int](mentIdx);
      nextClusterIdx += 1;
    } else {
      val clusterIdx = mentsToClusters(assignmentIdx)
      mentsToClusters(mentIdx) = clusterIdx;
      clustersToMents(clusterIdx) += mentIdx;
    }
  }
}

class DocumentInferencerRahman(val entityFeaturizer: EntityFeaturizer,
                               val entityFeatureIndexer: Indexer[String],
                               val rahmanTrainType: String) extends DocumentInferencer {
  
  var wCounter = 0;
  
  def getInitialWeightVector(featureIndexer: Indexer[String]): Array[Float] = Array.fill(featureIndexer.size())(0.0F);
  
  /**
   * N.B. always returns a reference to the same matrix, so don't call twice in a row and
   * attempt to use the results of both computations
   */
  private def computeMarginalsAndViterbi(docGraph: DocumentGraph,
                                         pairwiseScorer: PairwiseScorer,
                                         gold: Boolean,
                                         lossFcn: (CorefDoc, Int, Int) => Float,
                                         entityAntecedents: Array[Int]): (Array[Array[Float]], MentClusterMapping) = {
    computeMarginalsAndViterbi(docGraph, pairwiseScorer, gold, lossFcn, docGraph.featurizeIndexAndScoreNonPrunedUseCache(pairwiseScorer)._2, entityAntecedents)
  }
  
  private def computeMarginalsAndViterbi(docGraph: DocumentGraph,
                                         pairwiseScorer: PairwiseScorer,
                                         gold: Boolean,
                                         lossFcn: (CorefDoc, Int, Int) => Float,
                                         scoresChart: Array[Array[Float]],
                                         entityAntecedents: Array[Int]): (Array[Array[Float]], MentClusterMapping) = {
    val marginals = docGraph.cachedMarginalMatrix;
    val mentClusterMapping = docGraph.cachedMentClusterMapping;
    mentClusterMapping.clear();
    for (i <- 0 until docGraph.size) {
      var normalizer = 0.0F;
      // Restrict to gold antecedents if we're doing gold, but don't load the gold antecedents
      // if we're not.
      val goldAntecedents: Seq[Int] = if (gold) docGraph.getGoldAntecedentsUnderCurrentPruning(i) else null;
      var bestAntecedentScore = Float.NegativeInfinity;
      var bestAntecedentIdx = -1;
      for (j <- 0 to i) {
        // If this is a legal antecedent
        if (!docGraph.isPruned(i, j) && (!gold || goldAntecedents.contains(j))) {
          // N.B. Including lossFcn is okay even for gold because it should be zero
          val entityFeatures = entityFeaturizer.featurize(docGraph, i, j, mentClusterMapping.mentsToClusters, mentClusterMapping.clustersToMents);
//          if (j != i) {
//            Logger.logss("Prev ments for " + j + ": " + mentClusterMapping.getCluster(j).map(docGraph.getMention(_).headString))
//          }
//          Logger.logss("Features for " + i + " -> " + j + ": " + entityFeatures.map(_.name));
          val entityFeatsScore = entityFeatures.foldLeft(0.0)((score, feat) => {
            val featIdx = pairwiseScorer.featurizer.getIndexer.indexOf(feat.name);
            if (featIdx == -1) {
              throw new RuntimeException("Unknown feature: " + feat.name);
            }
            score + pairwiseScorer.weights(featIdx)
          });
          val unnormalizedProb = Math.exp(scoresChart(i)(j) + entityFeatsScore + lossFcn(docGraph.corefDoc, i, j)).toFloat;
          marginals(i)(j) = unnormalizedProb;
          normalizer += unnormalizedProb;
          if (unnormalizedProb > bestAntecedentScore) {
            bestAntecedentScore = unnormalizedProb;
            bestAntecedentIdx = j;
          }
        } else {
          marginals(i)(j) = 0.0F;
        }
      }
      for (j <- 0 to i) {
        marginals(i)(j) /= normalizer;
      }
//      Logger.logss("Best idx for " + i + " = " + bestAntecedentIdx);
      if (entityAntecedents != null) {
        mentClusterMapping.updateWithAssignment(i, entityAntecedents(i));
      } else {
        mentClusterMapping.updateWithAssignment(i, bestAntecedentIdx);
      }
    }
    (marginals, mentClusterMapping);
  }
  
  def computeLikelihood(docGraph: DocumentGraph,
                        pairwiseScorer: PairwiseScorer,
                        lossFcn: (CorefDoc, Int, Int) => Float): Float = {
    var likelihood = 0.0F;
    val marginals = computeMarginalsAndViterbi(docGraph, pairwiseScorer, false, lossFcn, null)._1;
    for (i <- 0 until docGraph.size) {
      val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(i);
      var currProb = 0.0;
      for (j <- goldAntecedents) {
        currProb += marginals(i)(j);
      }
      var currLogProb = Math.log(currProb).toFloat;
      if (currLogProb.isInfinite()) {
        Logger.logss("Negative infinity curr log prob (should only happen on dev set when we pruned away all gold guys)");
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
    val goldAntecedents = (0 until docGraph.size).map(i => docGraph.getGoldAntecedentsNoPruning(i)(0)).toArray;
    val isGoldFirstPass = rahmanTrainType.contains("predusegold");
    
    val predMarginals = this.computeMarginalsAndViterbi(docGraph, pairwiseScorer, false, lossFcn, scoresChart, if (isGoldFirstPass) goldAntecedents else null)._1;
    val predMentClusterMapping = docGraph.cachedMentClusterMapping;
    predMentClusterMapping.clear();
    // Used if we want to use predicted previous clusters for computing gold features
    val predAntecedents = new Array[Int](docGraph.size);
    for (i <- 0 until docGraph.size) {
      var bestAntecedentIdx = -1;
      var bestAntecedentScore = Float.NegativeInfinity;
      for (j <- 0 to i) {
        // Compute the features associated with this
        if (predMarginals(i)(j) > 1e-20) {
          addToGradient(featsChart(i)(j), -predMarginals(i)(j), gradient);
          val entityFeatures = entityFeaturizer.featurize(docGraph, i, j, predMentClusterMapping.mentsToClusters, predMentClusterMapping.clustersToMents);
          val indexedFeatures = entityFeaturizer.indexFeatures(entityFeatures, entityFeatureIndexer);
          addToGradient(indexedFeatures, -predMarginals(i)(j), gradient);
          if (predMarginals(i)(j) > bestAntecedentScore) {
            bestAntecedentIdx = j;
            bestAntecedentScore = predMarginals(i)(j);
          }
        }
      }
      require(bestAntecedentIdx != -1, "Couldn't find a good antecedent for mention " + i + ": " + predMarginals(i).toSeq);
      predAntecedents(i) = bestAntecedentIdx;
      if (isGoldFirstPass) {
        predMentClusterMapping.updateWithAssignment(i, goldAntecedents(i));
      } else {
        predMentClusterMapping.updateWithAssignment(i, bestAntecedentIdx);
      }
    }
    val isPredSecondPass = rahmanTrainType.contains("goldusepred");
    // TODO: To reconstruct original, uncomment this and comment the next
//    val goldMarginals = this.computeMarginalsAndViterbi(docGraph, pairwiseScorer, true, lossFcn, scoresChart, if (isPredSecondPass) predAntecedents else null)._1;
    val goldMarginals = this.computeMarginalsAndViterbi(docGraph, pairwiseScorer, true, lossFcn, scoresChart, if (isPredSecondPass) predAntecedents else goldAntecedents)._1;
    val goldMentClusterMapping = docGraph.cachedMentClusterMapping;
    goldMentClusterMapping.clear();
    for (i <- 0 until docGraph.size) {
      var bestAntecedentIdx = -1;
      var bestAntecedentScore = Float.NegativeInfinity;
      for (j <- 0 to i) {
        if (goldMarginals(i)(j) > 1e-20) {
          addToGradient(featsChart(i)(j), goldMarginals(i)(j), gradient);
          val entityFeatures = entityFeaturizer.featurize(docGraph, i, j, goldMentClusterMapping.mentsToClusters, goldMentClusterMapping.clustersToMents);
          val indexedFeatures = entityFeaturizer.indexFeatures(entityFeatures, entityFeatureIndexer);
          addToGradient(indexedFeatures, goldMarginals(i)(j), gradient);
          if (goldMarginals(i)(j) > bestAntecedentScore) {
            bestAntecedentIdx = j;
            bestAntecedentScore = goldMarginals(i)(j);
          }
        }
      }
      if (isPredSecondPass) {
        goldMentClusterMapping.updateWithAssignment(i, predAntecedents(i));
      } else {
        // TODO: To reconstruct original, uncomment this and comment the next
//        goldMentClusterMapping.updateWithAssignment(i, bestAntecedentIdx);
        goldMentClusterMapping.updateWithAssignment(i, goldAntecedents(i));
      }
    }
    if (wCounter == 50 || wCounter % 1000 == 999) {
      val featIndexer = pairwiseScorer.featurizer.getIndexer;
      for (featIdx <- 0 until featIndexer.size) {
        var numDisplayed = 0;
        if (numDisplayed < 50 && featIndexer.get(featIdx).startsWith("Entity")) {
          Logger.logss(featIndexer.get(featIdx) + ": " + pairwiseScorer.weights(featIdx));
          numDisplayed += 1;
        }
      }
    }
    wCounter += 1;
  }
  
  private def addToGradient(feats: Seq[Int], scale: Float, gradient: Array[Float]) {
    var i = 0;
    while (i < feats.size) {
      val feat = feats(i);
      gradient(feat) += scale;
      i += 1;
    }
  }

  def viterbiDecode(docGraph: DocumentGraph, scorer: PairwiseScorer): Array[Int] = {
    val (featsChart, scoresChart) = docGraph.featurizeIndexAndScoreNonPrunedUseCache(scorer);
    val zeroLoss = (doc: CorefDoc, ment: Int, ant: Int) => 0.0F;
    val predMarginals = this.computeMarginalsAndViterbi(docGraph, scorer, false, zeroLoss, scoresChart, null)._1;
    DocumentInferencerBasic.decodeMax(docGraph.size, (idx: Int) => predMarginals(idx));
  }
  
  def finishPrintStats() = {}
}

object DocumentInferencerRahman {
  
  def main(args: Array[String]) {
    val mcm = new MentClusterMapping(100);
    mcm.updateWithAssignment(0, 0);
    mcm.updateWithAssignment(1, 0);
    mcm.updateWithAssignment(2, 2);
    println(mcm.getCluster(0));
    println(mcm.getCluster(1));
    println(mcm.getCluster(2));
  }
}
