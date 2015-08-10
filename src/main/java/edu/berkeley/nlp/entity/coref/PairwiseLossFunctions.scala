package edu.berkeley.nlp.entity.coref
import edu.berkeley.nlp.futile.util.Logger
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

trait PairwiseLossFunction {
  def loss(doc: CorefDoc, ment: Int, ant: Int): Double;
  
  def loss(doc: CorefDoc, ment: Int, prunedEdges: Array[Array[Boolean]]): Array[Double];
}

class SimplePairwiseLossFunction(lossFcn: (CorefDoc, Int, Int) => Float) extends PairwiseLossFunction {
  
  def loss(doc: CorefDoc, ment: Int, ant: Int): Double = lossFcn(doc, ment, ant)
  
  def loss(doc: CorefDoc, ment: Int, prunedEdges: Array[Array[Boolean]]): Array[Double] = Array.tabulate(ment+1)(ant => lossFcn(doc, ment, ant).toDouble)
}

class DownstreamPairwiseLossFunction(spec: String,
                                     foldMapping: HashMap[UID,Int],
                                     models: ArrayBuffer[PairwiseScorer]) extends PairwiseLossFunction {
  val params = spec.split("-");
  val mucWeight = params(1).toDouble
  val bcubWeight = params(2).toDouble
  
  val inferencer = new DocumentInferencerBasic
  val cache = new HashMap[UID,Array[Array[Double]]]
  
  def loss(doc: CorefDoc, ment: Int, ant: Int): Double = {
    val uid = doc.rawDoc.uid
    if (!cache.contains(uid)) {
      Logger.logss("Caching computation for " + uid)
      val docGraph = new DocumentGraph(doc, false)
      val scorerThisFold = models(foldMapping(uid))
      val backpointers = inferencer.viterbiDecode(docGraph, scorerThisFold)
      cache.put(doc.rawDoc.uid, CorefEvaluator.getLosses(doc, backpointers, mucWeight, bcubWeight, None))
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
      cache.put(doc.rawDoc.uid, CorefEvaluator.getLosses(doc, backpointers, mucWeight, bcubWeight, Some(prunedEdges)))
    }
    cache(uid)(ment)
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
