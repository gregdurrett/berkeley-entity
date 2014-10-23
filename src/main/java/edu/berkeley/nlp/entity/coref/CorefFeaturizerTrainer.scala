package edu.berkeley.nlp.entity.coref
import scala.Array.canBuildFrom
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.mutable.HashSet
import edu.berkeley.nlp.futile.math.CachingDifferentiableFunction
import edu.berkeley.nlp.futile.math.LBFGSMinimizer
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.fig.basic.SysInfoUtils
import java.util.Arrays


class CorefFeaturizerTrainer() {
  var inferenceNanos = 0L;
  var adagradNanos = 0L;
  
  def featurizeBasic(docGraphs: Seq[DocumentGraph], pairwiseIndexingFeaturizer: PairwiseIndexingFeaturizer) {
    val featureIndexer = pairwiseIndexingFeaturizer.getIndexer;
    // Do all preprocessing of the training set necessary to compute features
    Logger.logss("Memory before featurization: " + SysInfoUtils.getUsedMemoryStr());
    Logger.startTrack("Featurizing (basic pass)");
    var idx = 0;
    for (docGraph <- docGraphs) {
      if (idx % 5 == 0) {
        Logger.logs("Featurizing (basic pass) " + idx + ", " + SysInfoUtils.getUsedMemoryStr() + ", " + featureIndexer.size());
      }
      docGraph.featurizeIndexNonPrunedUseCache(pairwiseIndexingFeaturizer);
      idx += 1;
    }
    Logger.endTrack();
    Logger.logss("Features after featurization: " + featureIndexer.size());
    Logger.logss("\"Topic\" features after featurization: " +
                 featureIndexer.getObjects().asScala.foldLeft(0)((currTotal, feat) => if (feat.contains("Topic")) currTotal + 1 else currTotal));
    Logger.logss("\"Distrib\" features after featurization: " +
                 featureIndexer.getObjects().asScala.foldLeft(0)((currTotal, feat) => if (feat.contains("Distrib")) currTotal + 1 else currTotal));
    docGraphs(0).printAverageFeatureCountInfo();
    Logger.logss("Memory after featurization: " + SysInfoUtils.getUsedMemoryStr());
  }
  
//  def featurizeRahmanAddToIndexer(docGraphs: Seq[DocumentGraph], featurizer: PairwiseIndexingFeaturizer, entityFeaturizer: EntityFeaturizer) {
//    // Could potentially do this for all DocumentGraphs but right now the
//    // only information used is the number of clusterers and the sizes of their domains
//    val feats = entityFeaturizer.getAllPossibleFeatures(docGraphs(0));
//    feats.map(feat => featurizer.getIndex(feat.name, true));
//  }

  def train(trainDocGraphs: Seq[DocumentGraph],
            pairwiseIndexingFeaturizer: PairwiseIndexingFeaturizer,
            eta: Float,
            reg: Float,
            batchSize: Int,
            lossFcn: (CorefDoc, Int, Int) => Float,
            numItrs: Int,
            inferencer: DocumentInferencer): Array[Float] = {
    trainAdagrad(trainDocGraphs, pairwiseIndexingFeaturizer, eta, reg, batchSize, lossFcn, numItrs, inferencer);
  }

  def trainAdagrad(trainDocGraphs: Seq[DocumentGraph],
                   pairwiseIndexingFeaturizer: PairwiseIndexingFeaturizer,
                   eta: Float,
                   lambda: Float,
                   batchSize: Int,
                   lossFcn: (CorefDoc, Int, Int) => Float,
                   numItrs: Int,
                   inferencer: DocumentInferencer): Array[Float] = {
//    val weights = Array.fill(pairwiseIndexingFeaturizer.featureIndexer.size)(0.0);
    val weights = inferencer.getInitialWeightVector(pairwiseIndexingFeaturizer.getIndexer);
    val reusableGradientArray = Array.fill(pairwiseIndexingFeaturizer.getIndexer.size)(0.0F);
    val diagGt = Array.fill(pairwiseIndexingFeaturizer.getIndexer.size)(0.0F);
    for (i <- 0 until numItrs) {
      Logger.logss("ITERATION " + i);
      val startTime = System.nanoTime();
      inferenceNanos = 0;
      adagradNanos = 0;
      Logger.startTrack("Computing gradient");
      var currIdx = 0;
      while (currIdx < trainDocGraphs.size) {
        if (currIdx % 100 == 0) {
          Logger.logs("Computing gradient on " + currIdx);
        }
//        if (batchSize == 1) {
//          takeAdagradStepL1R(trainDocGraphs(currIdx),
//                             inferencer,
//                             new PairwiseScorer(pairwiseIndexingFeaturizer, weights),
//                             reusableGradientArray,
//                             diagGt,
//                             eta,
//                             lambda,
//                             lossFcn,
//                             weights);
//        } else {
        takeAdagradStepL1R(trainDocGraphs.slice(currIdx, Math.min(trainDocGraphs.size, currIdx + batchSize)),
                           inferencer,
                           new PairwiseScorer(pairwiseIndexingFeaturizer, weights),
                           reusableGradientArray,
                           diagGt,
                           eta,
                           lambda,
                           lossFcn,
                           weights);
//        }
        currIdx += batchSize;
      }
      Logger.endTrack();
      Logger.logss("NONZERO WEIGHTS: " + weights.foldRight(0)((weight, count) => if (Math.abs(weight) > 1e-15) count + 1 else count));
      Logger.logss("WEIGHT VECTOR NORM: " + weights.foldRight(0.0)((weight, norm) => norm + weight * weight));
      if (i == 0 || i == 1 || i % 5 == 4 || i == numItrs - 1) {
        val pairwiseScorer = new PairwiseScorer(pairwiseIndexingFeaturizer, weights);
        Logger.startTrack("Evaluating objective on train");
        Logger.logss("TRAIN OBJECTIVE: " + computeObjectiveL1R(trainDocGraphs, inferencer, pairwiseScorer, lossFcn, lambda));
        Logger.endTrack();
//        Logger.startTrack("Decoding train");
//        val (allPredBackptrs, allPredClusterings) = inferencer.viterbiDecodeAllFormClusterings(trainDocGraphs, pairwiseScorer);
//        val trainAcc = CorefEvaluator.evaluateAndRenderShort(trainDocGraphs, allPredBackptrs, allPredClusterings, "TRAIN: ");
//        Logger.logss(trainAcc);
//        Logger.endTrack();
      }
      Logger.logss("MILLIS FOR ITER " + i + ": " + (System.nanoTime() - startTime) / 1000000.0);
      Logger.logss("MILLIS INFERENCE FOR ITER " + i + ": " + inferenceNanos / 1000000.0);
      Logger.logss("MILLIS ADAGRAD FOR ITER " + i + ": " + adagradNanos / 1000000.0);
      Logger.logss("MEMORY AFTER ITER " + i + ": " + SysInfoUtils.getUsedMemoryStr());
    }
    weights
  }
  
  def computeObjectiveL1R(trainDocs: Seq[DocumentGraph],
                          inferencer: DocumentInferencer,
                          pairwiseScorer: PairwiseScorer,
                          lossFcn: (CorefDoc, Int, Int) => Float,
                          lambda: Float): Float = {
    var objective = computeLikelihood(trainDocs, inferencer, pairwiseScorer, lossFcn);
    for (weight <- pairwiseScorer.weights) {
      objective -= lambda * Math.abs(weight);
    }
    objective;
  }

//  def computeObjectiveL2R(trainDocs: Seq[DocumentGraph],
//                          inferencer: DocumentInferencer,
//                          pairwiseScorer: PairwiseScorer,
//                          lossFcn: (CorefDoc, Int, Int) => Float,
//                          c: Float): Float = {
//    var objective = computeLikelihood(trainDocs, inferencer, pairwiseScorer, lossFcn);
//    for (weight <- pairwiseScorer.weights) {
//      objective -= c * weight * weight;
//    }
//    objective;
//  }

  def computeLikelihood(trainDocGraphs: Seq[DocumentGraph],
                        inferencer: DocumentInferencer,
                        pairwiseScorer: PairwiseScorer,
                        lossFcn: (CorefDoc, Int, Int) => Float): Float = {
    trainDocGraphs.foldRight(0.0F)((docGraph, likelihood) => likelihood + inferencer.computeLikelihood(docGraph, pairwiseScorer, lossFcn));
  }

  def takeAdagradStepL1R(docs: Seq[DocumentGraph],
                         inferencer: DocumentInferencer,
                         pairwiseScorer: PairwiseScorer,
                         reusableGradientArray: Array[Float],
                         diagGt: Array[Float],
                         eta: Float,
                         lambda: Float,
                         lossFcn: (CorefDoc, Int, Int) => Float,
                         weights: Array[Float]) {
    Arrays.fill(reusableGradientArray, 0.0F);
    var nanoTime = System.nanoTime();
    for (doc <- docs) {
      inferencer.addUnregularizedStochasticGradient(doc, pairwiseScorer, lossFcn, reusableGradientArray);
    }
    inferenceNanos += (System.nanoTime() - nanoTime);
    nanoTime = System.nanoTime();
    // Precompute this so dividing by batch size is a multiply and not a divide
    val batchSizeMultiplier = 1.0F/docs.size;
    var i = 0;
    while (i < reusableGradientArray.size) {
      val xti = pairwiseScorer.weights(i);
      // N.B. We negate the gradient here because the Adagrad formulas are all for minimizing
      // and we're trying to maximize, so think of it as minimizing the negative of the objective
      // which has the opposite gradient
      // Equation (25) in http://www.cs.berkeley.edu/~jduchi/projects/DuchiHaSi10.pdf
      // eta is the step size, lambda is the regularization
      val gti = -reusableGradientArray(i) * batchSizeMultiplier;
      // Update diagGt
      diagGt(i) += gti * gti;
      val Htii = 1F + Math.sqrt(diagGt(i)).toFloat;
      // Avoid divisions at all costs...
      val etaOverHtii = eta / Htii;
      val newXti = xti - etaOverHtii * gti;
      weights(i) = Math.signum(newXti) * Math.max(0, Math.abs(newXti) - lambda * etaOverHtii);
      i += 1;
    }
    adagradNanos += (System.nanoTime() - nanoTime);
  }
}
