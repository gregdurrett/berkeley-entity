package edu.berkeley.nlp.entity.xdistrib

import scala.collection.mutable.HashMap
import scala.util.Random

import edu.berkeley.nlp.entity.coref.CorefDoc
import edu.berkeley.nlp.entity.coref.DocumentInferencerBasic
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.entity.coref.OrderedClustering
import edu.berkeley.nlp.entity.coref.PairwiseIndexingFeaturizer
import edu.berkeley.nlp.entity.joint.LikelihoodAndGradientComputer
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.util.Logger

class CorefComputerDistrib(val lossFcn: (CorefDoc, Int, Int) => Float,
                           val sparseFeatureIndexer: Indexer[String],
                           val componentFeatureIndexer: Indexer[String],
                           val distribSize: Int) extends LikelihoodAndGradientComputer[DocumentGraphComponents] {
  val numSparseFeatures = sparseFeatureIndexer.size;
  val numComponentFeatures = componentFeatureIndexer.size;
  
  val basicInferencer = new DocumentInferencerBasic;
  
  val numFeatures = numSparseFeatures + numComponentFeatures * distribSize;
  
  val rand = new Random(0);
  val weightOffsets: Array[Float] = Array.tabulate(numComponentFeatures * distribSize)(i => ((rand.nextDouble - 0.5) * 0.01).toFloat);
//  val weightOffsets: Array[Float] = Array.tabulate(numComponentFeatures * distribSize)(i => 0.0F);
  
  final def getRealParamIdx(componentIdx: Int, r: Int) = {
    numSparseFeatures + componentIdx * distribSize + r;
  }
  
  def accessComponentWeight(componentIdx: Int, r: Int, weights: Array[Float]): Float = accessComponentWeight(getRealParamIdx(componentIdx, r), weights);
  
  def accessComponentWeight(idx: Int, weights: Array[Float]): Float = weights(idx) + weightOffsets(idx - numSparseFeatures);
  
  def sum(feats: Array[Int], weights: Array[Float], destVect: Array[Float]) {
    var i = 0;
    var distribIdx = 0;
    while (distribIdx < distribSize) {
      destVect(distribIdx) = 0;
      distribIdx += 1;
    }
    while (i < feats.size) {
      var weightsStartIdx = getRealParamIdx(feats(i), 0);
      var distribIdx = 0;
      while (distribIdx < distribSize) {
        destVect(distribIdx) += accessComponentWeight(weightsStartIdx + distribIdx, weights);
        distribIdx += 1;
      }
      i += 1;
    }
  }
  
  def computeScores(ex: DocumentGraphComponents, weights: Array[Float]) = {
    val scores = ex.docGraph.scoreNonPrunedUseCache(weights);
    for (i <- 0 until ex.docGraph.size) {
      if (ex.cachedSummedVects(i) == null || ex.cachedSummedVects(i).size != distribSize) {
        ex.cachedSummedVects(i) = new Array[Float](distribSize);
      }
      sum(ex.components(i), weights, ex.cachedSummedVects(i));
    }
    // N.B. 0 until i so that we don't add scores for a guy and himself
    for (i <- 0 until ex.docGraph.size; j <- 0 until i) {
      val dotProduct = GUtil.dot(ex.cachedSummedVects(i), ex.cachedSummedVects(j)); 
      scores(i)(j) += dotProduct;
    }
    scores;
    
  }
  
  def addUnregularizedStochasticGradient(ex: DocumentGraphComponents, weights: Array[Float], gradient: Array[Float]) {
    
    val doTwiddleWeightsAndDoEmpiricalGradient = false;
//    val doTwiddleWeightsAndDoEmpiricalGradient = true;
    if (doTwiddleWeightsAndDoEmpiricalGradient) {
//      val rand = new Random(0);
//      Logger.logss("TWIDDLING WEIGHTS; REMOVE THIS");
//      for (i <- 0 until weights.size) {
//        weights(i) += (rand.nextDouble * 0.5).toFloat;
//      }
      val freshGradient = Array.fill(weights.size)(0.0F);
      addUnregularizedStochasticGradientHelper(ex, weights, freshGradient);
      val llBefore = computeLogLikelihood(ex, weights);
      val featsByTemplate = getRepresentativeFeatures(ex);
      Logger.logss("Checking empirical gradient on " + featsByTemplate.size + " representatives of templates");
      for (template <- featsByTemplate.keySet) {
        val (feat, i) = featsByTemplate(template);
        Logger.logss(i + " " + feat);
        // If delta is too small, the fact that the likelihood comes back as a float is a liability because
        // most of the bits disappear and then it gets scaled up
        val delta = 1e-2F;
        weights(i) += delta;
        val llAfter = computeLogLikelihood(ex, weights);
        weights(i) -= delta;
        val diff = Math.abs(freshGradient(i) - (llAfter - llBefore)/delta);
        if (diff > 1e-2) {
          Logger.logss((if (diff > 0.5) "BAD " else "") + "Bump test problem: " + template + " - " + i + ": gradient = " + freshGradient(i) + ", change = " + ((llAfter - llBefore)/delta));
        }
      }
      System.exit(0);
    }
    
    addUnregularizedStochasticGradientHelper(ex, weights, gradient);
    
  }
  
  def getRepresentativeFeatures(ex: DocumentGraphComponents) = {
    val featsByTemplate = new HashMap[String,(String,Int)];
    for (i <- 0 until ex.docGraph.size) {
      for (j <- 0 to i) {
        for (feat <- ex.docGraph.cachedFeats(i)(j)) {
          val featStr = sparseFeatureIndexer.getObject(feat);
          featsByTemplate.put(PairwiseIndexingFeaturizer.getTemplate(featStr), featStr -> feat);
        }
      }
      val currComponents = ex.components(i);
      for (component <- currComponents) {
        featsByTemplate += componentFeatureIndexer.getObject(component) -> (componentFeatureIndexer.getObject(component) + "-0" -> getRealParamIdx(component, 0));
      }
    }
    featsByTemplate;
  }

  def addUnregularizedStochasticGradientHelper(ex: DocumentGraphComponents, weights: Array[Float], gradient: Array[Float]) {
    val scoresChart = computeScores(ex, weights);
    val featsChart = ex.docGraph.cachedFeats;
    // N.B. Can't have pred marginals and gold marginals around at the same time because
    // they both live in the same cached matrix
    val predMarginals = basicInferencer.computeMarginals(ex.docGraph, false, lossFcn, scoresChart);
    for (i <- 0 until ex.docGraph.size) {
      for (j <- 0 to i) {
        if (predMarginals(i)(j) > 1e-20) { 
          GUtil.addToGradient(featsChart(i)(j), -predMarginals(i)(j), gradient);
          if (i != j) {
            addComponentsToGradient(ex, i, j, -predMarginals(i)(j), weights, gradient);
          }
        }
      }
    }
    val goldMarginals = basicInferencer.computeMarginals(ex.docGraph, true, lossFcn, scoresChart);
    for (i <- 0 until ex.docGraph.size) {
      for (j <- 0 to i) {
        if (goldMarginals(i)(j) > 1e-20) {
          GUtil.addToGradient(featsChart(i)(j), goldMarginals(i)(j), gradient);
          if (i != j) {
            addComponentsToGradient(ex, i, j, goldMarginals(i)(j), weights, gradient);
          }
        }
      }
    }
  }
  
  def addComponentsToGradient(ex: DocumentGraphComponents, currIdx: Int, antIdx: Int, scale: Float, weights: Array[Float], gradient: Array[Float]) {
    val currComponents = ex.components(currIdx);
    val currComponentsSummed = ex.cachedSummedVects(currIdx);
    val antComponents = ex.components(antIdx);
    val antComponentsSummed = ex.cachedSummedVects(antIdx);
    for (currComponent <- currComponents) {
      var k = 0;
      while (k < distribSize) {
        gradient(getRealParamIdx(currComponent, k)) += scale * antComponentsSummed(k);
        k += 1;
      }
    }
    for (antComponent <- antComponents) {
      var k = 0;
      while (k < distribSize) {
        gradient(getRealParamIdx(antComponent, k)) += scale * currComponentsSummed(k);
        k += 1;
      }
    }
  }
  
  def computeLogLikelihood(ex: DocumentGraphComponents,
                           weights: Array[Float]): Float = {
    var likelihood = 0.0F;
    val scores = computeScores(ex, weights);
    val marginals = basicInferencer.computeMarginals(ex.docGraph, false, lossFcn, scores);
    for (i <- 0 until ex.docGraph.size) {
      val goldAntecedents = ex.docGraph.getGoldAntecedentsUnderCurrentPruning(i);
      var currProb = 0.0;
      for (j <- goldAntecedents) {
        currProb += marginals(i)(j);
      }
      var currLogProb = Math.log(currProb).toFloat;
      if (currLogProb.isInfinite()) {
        Logger.logss("Curr log prob is infinite!");
        currLogProb = -30;
      }
      likelihood += currLogProb;
    }
    likelihood;
  }
  
  def viterbiDecode(ex: DocumentGraphComponents, weights: Array[Float]) = {
    val scores = computeScores(ex, weights);
    basicInferencer.viterbiDecode(scores);
  }
  
  def viterbiDecodeAllFormClusterings(exs: Seq[DocumentGraphComponents], weights: Array[Float]) = {
    val backptrs = Array.tabulate(exs.size)(i => viterbiDecode(exs(i), weights));
    val clusterings = Array.tabulate(exs.size)(i => OrderedClustering.createFromBackpointers(backptrs(i)));
    (backptrs, clusterings)
  }
}
