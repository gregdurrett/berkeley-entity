package edu.berkeley.nlp.entity.joint
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.entity.ner.NerFeaturizer
import edu.berkeley.nlp.entity.coref.CorefDoc
import edu.berkeley.nlp.entity.coref.PairwiseIndexingFeaturizer
import edu.berkeley.nlp.entity.coref.OrderedClustering
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.entity._
import edu.berkeley.nlp.entity.ner.NerSystemLabeled
import edu.berkeley.nlp.futile.util.Logger
import scala.collection.JavaConverters._
import edu.berkeley.nlp.entity.coref.PairwiseLossFunctions
import edu.berkeley.nlp.entity.Driver
import scala.util.Random
import edu.berkeley.nlp.entity.bp.UnaryFactorGeneral
import edu.berkeley.nlp.entity.ner.MCNerFeaturizer
import edu.berkeley.nlp.entity.wiki._

class JointComputerShared[D,G<:JointDocFactorGraph](factorGraphFactory: FactorGraphFactory[D,G]) extends LikelihoodAndGradientComputer[D] {
  
  var egCounter = 0;
  
  val NumBpIters = Driver.numBPItrs; // 15;
  
  def getInitialWeightVector(featureIndexer: Indexer[String]): Array[Float] = Array.fill(featureIndexer.size())(0.0F);
  
  def computeLogLikelihood(doc: D,
                           weights: Array[Float]): Float = {
    val goldGraph = factorGraphFactory.getDocFactorGraphHard(doc, true);
    val predGraph = factorGraphFactory.getDocFactorGraphHard(doc, false);
    computeLogLikelihood(goldGraph, predGraph, weights).toFloat;
  }
  
  private def computeLogLikelihood(goldDocFactorGraph: G,
                                   predDocFactorGraph: G,
                                   weights: Array[Float]): Double = {
    goldDocFactorGraph.computeAndStoreMarginals(weights, false, NumBpIters);
    val goldNormalizer = goldDocFactorGraph.computeLogNormalizerApprox;
    goldDocFactorGraph.scrubMessages();
    predDocFactorGraph.computeAndStoreMarginals(weights, false, NumBpIters);
    val predNormalizer = predDocFactorGraph.computeLogNormalizerApprox;
    predDocFactorGraph.scrubMessages();
    goldNormalizer - predNormalizer;
  }
  
  def addUnregularizedStochasticGradient(doc: D,
                                         weights: Array[Float],
                                         gradient: Array[Float]) = {
    val doTwiddleWeightsAndDoEmpiricalGradient = false;
//    val doTwiddleWeightsAndDoEmpiricalGradient = true;
//    if (doTwiddleWeightsAndDoEmpiricalGradient) {
//      val rand = new Random(0);
//      Logger.logss("TWIDDLING WEIGHTS; REMOVE THIS");
//      for (i <- 0 until weights.size) {
//        weights(i) += (rand.nextDouble * 0.5).toFloat;
//      }
//    }
    
    val predDocFactorGraph = factorGraphFactory.getDocFactorGraphHard(doc, false);
    predDocFactorGraph.computeAndStoreMarginals(weights, false, NumBpIters);
    predDocFactorGraph.addExpectedFeatureCountsToGradient(-1.0F, gradient);
    val goldDocFactorGraph = factorGraphFactory.getDocFactorGraphHard(doc, true);
    goldDocFactorGraph.computeAndStoreMarginals(weights, false, NumBpIters);
    goldDocFactorGraph.addExpectedFeatureCountsToGradient(1.0F, gradient);
//    Logger.logss("Gradient of ExactHeadMatch=True: " + gradient(featurizer.indexer.indexOf("ExactHeadMatch=true")));
//    Logger.logss("Value of ExactHeadMatch=true: " + weights(featurizer.indexer.indexOf("ExactHeadMatch=true")));
    
    // EMPIRICAL GRADIENT CHECK
//    if (doTwiddleWeightsAndDoEmpiricalGradient && goldDocFactorGraph.allFactorsEveryIter.size > 0) {
    if (doTwiddleWeightsAndDoEmpiricalGradient && egCounter > 40) {
      Logger.logss("DocumentInferencerLoopy: empirical gradient check");
      val freshGradient = Array.fill(weights.size)(0.0F);
      predDocFactorGraph.addExpectedFeatureCountsToGradient(-1.0F, freshGradient);
      goldDocFactorGraph.addExpectedFeatureCountsToGradient(1.0F, freshGradient);
      val llBefore = computeLogLikelihood(doc, weights);
//      Logger.logss("Base likelihood correct: " + llBefore);
//      // Pick one feat from each template and check the EG on that
      val featsByTemplate = predDocFactorGraph.getRepresentativeFeatures;
      Logger.logss("Checking empirical gradient on " + featsByTemplate.size + " representatives of templates");
      for (template <- featsByTemplate.keySet) {
        val feat = featsByTemplate(template);
        val i = factorGraphFactory.getIndexer.getIndex(feat);
        Logger.logss(i + " " + feat);
        // If delta is too small, the fact that the likelihood comes back as a float is a liability because
        // most of the bits disappear and then it gets scaled up
        val delta = 1e-2F;
        weights(i) += delta;
        val llAfter = computeLogLikelihood(doc, weights);
        weights(i) -= delta;
        val diff = Math.abs(freshGradient(i) - (llAfter - llBefore)/delta);
        if (diff > 1e-2) {
          Logger.logss((if (diff > 0.5) "BAD " else "") + "Bump test problem: " + template + " - " + i + ": gradient = " + freshGradient(i) + ", change = " + ((llAfter - llBefore)/delta));
        }
      }
      System.exit(0);
    }
    
    predDocFactorGraph.scrubMessages();
    goldDocFactorGraph.scrubMessages();
    egCounter += 1;
  }
  
  def viterbiDecodeProduceAnnotations(doc: D, weights: Array[Float]) = {
    val factorGraph = factorGraphFactory.getDocFactorGraph(doc, false, false, false, PairwiseLossFunctions.noLoss, JointLossFcns.noNerLossFcn, JointLossFcns.noWikiLossFcn);
    // We exponentiate messages here, but don't need to exponentiate them below because that doesn't
    // change the max.
    factorGraph.computeAndStoreMarginals(weights, false, NumBpIters);
//    computeAndStoreMarginals(factorGraph, weights, lossAugmented = false, exponentiateMessages = true);
    // MBR decoding on coref
    val predBackptrs = factorGraph.decodeCorefProduceBackpointers;
    // MBR decoding on NER as well
    val chunks = factorGraph.decodeNERProduceChunks;
    val wikiChunks = factorGraph.decodeWikificationProduceChunks;
    factorGraph.scrubMessages();
    (predBackptrs, OrderedClustering.createFromBackpointers(predBackptrs), chunks, wikiChunks);
  }
    
}
