package edu.berkeley.nlp.entity.joint
import scala.Array.canBuildFrom
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.mutable.HashSet
import edu.berkeley.nlp.futile.math.CachingDifferentiableFunction
import edu.berkeley.nlp.futile.math.LBFGSMinimizer
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.fig.basic.SysInfoUtils
import java.util.Arrays

trait LikelihoodAndGradientComputer[T] {
  def addUnregularizedStochasticGradient(ex: T, weights: Array[Float], gradient: Array[Float]);
  def computeLogLikelihood(ex: T, weights: Array[Float]): Float;
}

class GeneralTrainer[T] {
  
  var inferenceNanos = 0L;
  var adagradNanos = 0L;
  
  def train(trainExs: Seq[T],
            computer: LikelihoodAndGradientComputer[T],
            numFeats: Int,
            eta: Float,
            reg: Float,
            batchSize: Int,
            numItrs: Int): Array[Float] = {
    trainAdagrad(trainExs, computer, numFeats, eta, reg, batchSize, numItrs);
  }

  def trainAdagrad(trainExs: Seq[T],
                   computer: LikelihoodAndGradientComputer[T],
                   numFeats: Int,
                   eta: Float,
                   lambda: Float,
                   batchSize: Int,
                   numItrs: Int): Array[Float] = {
//    val weights = Array.fill(pairwiseIndexingFeaturizer.featureIndexer.size)(0.0);
    val weights = Array.fill(numFeats)(0.0F);
    val reusableGradientArray = Array.fill(numFeats)(0.0F);
    val diagGt = Array.fill(numFeats)(0.0F);
    for (i <- 0 until numItrs) {
      Logger.logss("ITERATION " + i);
      val startTime = System.nanoTime();
      inferenceNanos = 0;
      adagradNanos = 0;
      Logger.startTrack("Computing gradient");
      var currIdx = 0;
      var currBatchIdx = 0;
      val printFreq = (trainExs.size / batchSize) / 10 // Print progress 10 times per pass through the data
      while (currIdx < trainExs.size) {
        if (printFreq == 0 || currBatchIdx % printFreq == 0) {
          Logger.logs("Computing gradient on " + currIdx);
        }
        takeAdagradStepL1R(trainExs.slice(currIdx, Math.min(trainExs.size, currIdx + batchSize)),
                           computer,
                           weights,
                           reusableGradientArray,
                           diagGt,
                           eta,
                           lambda);
//        }
        currIdx += batchSize;
        currBatchIdx += 1;
      }
      Logger.endTrack();
      Logger.logss("NONZERO WEIGHTS: " + weights.foldRight(0)((weight, count) => if (Math.abs(weight) > 1e-15) count + 1 else count));
      Logger.logss("WEIGHT VECTOR NORM: " + weights.foldRight(0.0)((weight, norm) => norm + weight * weight));
      if (i == 0 || i == 1 || i % 5 == 4 || i == numItrs - 1) {
        Logger.startTrack("Evaluating objective on train");
        Logger.logss("TRAIN OBJECTIVE: " + computeObjectiveL1R(trainExs, computer, weights, lambda));
        Logger.endTrack();
      }
      Logger.logss("MILLIS FOR ITER " + i + ": " + (System.nanoTime() - startTime) / 1000000.0);
      Logger.logss("MILLIS INFERENCE FOR ITER " + i + ": " + inferenceNanos / 1000000.0);
      Logger.logss("MILLIS ADAGRAD FOR ITER " + i + ": " + adagradNanos / 1000000.0);
      Logger.logss("MEMORY AFTER ITER " + i + ": " + SysInfoUtils.getUsedMemoryStr());
    }
    weights
  }
  
  def computeObjectiveL1R(trainExs: Seq[T],
                          computer: LikelihoodAndGradientComputer[T],
                          weights: Array[Float],
                          lambda: Float): Float = {
    var objective = computeLikelihood(trainExs, computer, weights);
    for (weight <- weights) {
      objective -= lambda * Math.abs(weight);
    }
    objective;
  }

  def computeLikelihood(trainExs: Seq[T],
                        computer: LikelihoodAndGradientComputer[T],
                        weights: Array[Float]): Float = {
    (trainExs.foldRight(0.0)((ex, likelihood) => likelihood + computer.computeLogLikelihood(ex, weights))).toFloat;
  }

  def takeAdagradStepL1R(exs: Seq[T],
                         computer: LikelihoodAndGradientComputer[T],
                         weights: Array[Float],
                         reusableGradientArray: Array[Float],
                         diagGt: Array[Float],
                         eta: Float,
                         lambda: Float) {
    Arrays.fill(reusableGradientArray, 0.0F);
    var nanoTime = System.nanoTime();
    for (ex <- exs) {
      computer.addUnregularizedStochasticGradient(ex, weights, reusableGradientArray);
    }
    inferenceNanos += (System.nanoTime() - nanoTime);
    nanoTime = System.nanoTime();
    // Precompute this so dividing by batch size is a multiply and not a divide
    val batchSizeMultiplier = 1.0F/exs.size;
    var i = 0;
    while (i < reusableGradientArray.size) {
      val xti = weights(i);
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
