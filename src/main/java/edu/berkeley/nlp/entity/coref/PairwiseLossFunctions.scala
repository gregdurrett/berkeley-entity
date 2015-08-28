package edu.berkeley.nlp.entity.coref
import edu.berkeley.nlp.futile.util.Logger
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.AdagradWeightVector

trait PairwiseLossFunction {
  def loss(doc: CorefDoc, ment: Int, ant: Int): Double;
  
  def loss(doc: CorefDoc, ment: Int, prunedEdges: Array[Array[Boolean]]): Array[Double];
  
  def loss(doc: CorefDoc, prunedEdges: Array[Array[Boolean]]): Array[Array[Double]];
  
  def lossFromCurrPrediction(doc: CorefDoc, prunedEdges: Array[Array[Boolean]], prediction: Array[Int]): Array[Array[Double]];
}

class SimplePairwiseLossFunction(lossFcn: (CorefDoc, Int, Int) => Float) extends PairwiseLossFunction {
  
  def loss(doc: CorefDoc, ment: Int, ant: Int): Double = lossFcn(doc, ment, ant)
  
  def loss(doc: CorefDoc, ment: Int, prunedEdges: Array[Array[Boolean]]): Array[Double] = Array.tabulate(ment+1)(ant => lossFcn(doc, ment, ant).toDouble)
  
  def loss(doc: CorefDoc, prunedEdges: Array[Array[Boolean]]): Array[Array[Double]] = Array.tabulate(doc.predMentions.size)(ment => loss(doc, ment, prunedEdges))
  
  def lossFromCurrPrediction(doc: CorefDoc, prunedEdges: Array[Array[Boolean]], prediction: Array[Int]): Array[Array[Double]] = loss(doc, prunedEdges)
}

class DownstreamPairwiseLossFunction(spec: String,
                                     foldMapping: HashMap[UID,Int],
                                     models: ArrayBuffer[PairwiseScorer]) extends PairwiseLossFunction {
  val params = spec.split("-");
  val mucPrecWeight = params(1).toDouble
  val mucRecWeight = params(2).toDouble
  val mucF1Weight = params(3).toDouble
  val bcubPrecWeight = params(4).toDouble
  val bcubRecWeight = params(5).toDouble
  val bcubF1Weight = params(6).toDouble
  
  val inferencer = new DocumentInferencerBasic
  val cache = new HashMap[UID,Array[Array[Double]]]
  
  def loss(doc: CorefDoc, ment: Int, ant: Int): Double = {
    val uid = doc.rawDoc.uid
    if (!cache.contains(uid)) {
      Logger.logss("Caching computation for " + uid)
      val docGraph = new DocumentGraph(doc, false)
      val scorerThisFold = models(foldMapping(uid))
      val backpointers = inferencer.viterbiDecode(docGraph, scorerThisFold)
      cache.put(doc.rawDoc.uid, CorefEvaluator.getLosses(doc, backpointers, mucPrecWeight, mucRecWeight, mucF1Weight, bcubPrecWeight, bcubRecWeight, bcubF1Weight, None))
    }
    cache(uid)(ment)(ant)
  }
  
  def loss(doc: CorefDoc, ment: Int, prunedEdges: Array[Array[Boolean]]): Array[Double] = {
    val uid = doc.rawDoc.uid
    if (!cache.contains(uid)) {
      Logger.logss("Caching computation for " + uid)
      val docGraph = new DocumentGraph(doc, false)
      val scorerThisFold = models(foldMapping(uid))
      val backpointers = inferencer.viterbiDecode(docGraph, scorerThisFold)
      cache.put(doc.rawDoc.uid, CorefEvaluator.getLosses(doc, backpointers, mucPrecWeight, mucRecWeight, mucF1Weight, bcubPrecWeight, bcubRecWeight, bcubF1Weight, Some(prunedEdges)))
    }
    cache(uid)(ment)
  }
  
  def loss(doc: CorefDoc, prunedEdges: Array[Array[Boolean]]): Array[Array[Double]] = {
    val uid = doc.rawDoc.uid
    if (!cache.contains(uid)) {
      Logger.logss("Caching computation for " + uid)
      val docGraph = new DocumentGraph(doc, false)
      val scorerThisFold = models(foldMapping(uid))
      val backpointers = inferencer.viterbiDecode(docGraph, scorerThisFold)
      cache.put(doc.rawDoc.uid, CorefEvaluator.getLosses(doc, backpointers, mucPrecWeight, mucRecWeight, mucF1Weight, bcubPrecWeight, bcubRecWeight, bcubF1Weight, Some(prunedEdges)))
    }
    cache(uid)
  }
  
  def lossFromCurrPrediction(doc: CorefDoc, prunedEdges: Array[Array[Boolean]], prediction: Array[Int]): Array[Array[Double]] = {
    val uid = doc.rawDoc.uid
    val docGraph = new DocumentGraph(doc, false)
    CorefEvaluator.getLosses(doc, prediction, mucPrecWeight, mucRecWeight, mucF1Weight, bcubPrecWeight, bcubRecWeight, bcubF1Weight, Some(prunedEdges))
  }
}

class ScaledDownstreamPairwiseLossFunction(spec: String,
                                           baseLossFunction: PairwiseLossFunction) extends PairwiseLossFunction {
  
  def loss(doc: CorefDoc, ment: Int, ant: Int): Double = throw new RuntimeException("Unimplemented")
  
  def loss(doc: CorefDoc, ment: Int, prunedEdges: Array[Array[Boolean]]): Array[Double] = throw new RuntimeException("Unimplemented")
  
  def loss(doc: CorefDoc, prunedEdges: Array[Array[Boolean]]): Array[Array[Double]] = throw new RuntimeException("Unimplemented")
  
  def lossFromCurrPrediction(doc: CorefDoc, prunedEdges: Array[Array[Boolean]], prediction: Array[Int]): Array[Array[Double]] = {
    val uid = doc.rawDoc.uid
    val docGraph = new DocumentGraph(doc, false)
    val losses = CorefEvaluator.getLosses(doc, prediction, 0, 0, 1, 0, 0, 1, Some(prunedEdges))
    val avgLosses = Array.tabulate(losses.size)(i => {
      val nonzeroLosses = losses(i).toSeq.filter(_ > 0)
      nonzeroLosses.foldLeft(0.0)(_ + _)/nonzeroLosses.size
    })
    // 70th percentile right now
    val sortedLosses = avgLosses.toSeq.sorted
    val lossCutoff = sortedLosses((avgLosses.size * 0.7).toInt)
    Array.tabulate(losses.size)(i => {
      val isScaled = avgLosses(i) > lossCutoff
      Array.tabulate(losses(i).size)(j => {
        baseLossFunction.loss(doc, i, j) * (if (isScaled) 2.0 else 1.0)
      })
    })
    // Top 30% of lossy things in the document get scaled up by a factor of 2
  }
}

// This was written a while ago, hence why everything is vals and it's
// kinda weirdly non-object oriented
object PairwiseLossFunctions {
  
  val noLoss = (doc: CorefDoc, ment: Int, ant: Int) => 0.0F;
  
  val precisionLoss = (doc: CorefDoc, ment: Int, ant: Int) => {
    val oracleCluster = doc.getOraclePredClustering;
    // Only penalize if we predict a link and it's incorrect. N.B. because of our
    // conventions, ment == ant if we're predicting nonanaphoricity.
    if (!oracleCluster.areInSameCluster(ment, ant)) 1.0F else 0.0F;
  };
  
  val recallLoss = (doc: CorefDoc, ment: Int, ant: Int) => {
    val oracleCluster = doc.getOraclePredClustering;
    // Only penalize when we were supposed to make a prediction and we didn't or it
    // was wrong.
    if (!oracleCluster.startsCluster(ment) && !oracleCluster.areInSameCluster(ment, ant)) 1.0F else 0.0F;
  };
  
  // 1) Penalty when we link up someone who should start a new cluster (boosting this helps precision)
  // 2) Penalty when we start a new cluster with someone who should link up (boosting this helps recall)
  // 3) Penalty when we mess up a link
  val customLoss = (falseLinkScore: Float, falseNewScore: Float, wrongLinkScore: Float) => {
    (doc: CorefDoc, ment: Int, ant: Int) => {
      val oracleCluster = doc.getOraclePredClustering;
      if (oracleCluster.startsCluster(ment) && ment != ant) {
        falseLinkScore;
      } else if (!oracleCluster.startsCluster(ment) && ment == ant) {
        falseNewScore;
      } else if (!oracleCluster.startsCluster(ment) && !oracleCluster.areInSameCluster(ment, ant)) {
        wrongLinkScore;
      } else {
        0.0F;
      };
    }
  };
  
  // interpolationFactor interpolates between customLoss and a version of customLoss where
  // everything is weighted by the size of the gold cluster (should hypothetically be more
  // MUC-oriented than our current loss function)
  val weightedCustomLoss = (falseLinkScore: Float, falseNewScore: Float, wrongLinkScore: Float, interpolationFactor: Float) => {
    (doc: CorefDoc, ment: Int, ant: Int) => {
      val oracleCluster = doc.getOraclePredClustering;
      val oracleClusterSize = oracleCluster.getCluster(ment).size;
      val scalingFactor = (1 - interpolationFactor + interpolationFactor * oracleClusterSize);
      if (oracleCluster.startsCluster(ment) && ment != ant) {
        falseLinkScore * scalingFactor;
      } else if (!oracleCluster.startsCluster(ment) && ment == ant) {
        falseNewScore * scalingFactor;
      } else if (!oracleCluster.startsCluster(ment) && !oracleCluster.areInSameCluster(ment, ant)) {
        wrongLinkScore * scalingFactor;
      } else {
        0.0F;
      };
    }
  }
  
  
  
  def apply(x: String) = getLossFcn(x);
  
  def getLossFcn(name: String): (CorefDoc, Int, Int) => Float = {
    if (name == "noLoss") {
      noLoss;
    } else if (name == "precisionLoss") {
      precisionLoss;
    } else if (name == "recallLoss") {
      recallLoss;
    } else if (name.startsWith("customLoss")) {
      val params = name.split("-");
      require(params.size == 4);
      customLoss(params(1).toFloat, params(2).toFloat, params(3).toFloat);
    } else if (name.startsWith("weightedCustomLoss")) {
      val params = name.split("-");
      require(params.size == 5);
      weightedCustomLoss(params(1).toFloat, params(2).toFloat, params(3).toFloat, params(4).toFloat);
    } else {
      throw new RuntimeException("Unsupported");
    }
  }
}
