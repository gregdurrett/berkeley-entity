package edu.berkeley.nlp.entity.coref

import scala.collection.mutable.HashMap
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.futile.util.Beam
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.fig.basic.Indexer

@SerialVersionUID(1L)
class PairwiseScorer(val featurizer: PairwiseIndexingFeaturizer, val weights: Array[Float]) extends Serializable {
  
  def score(docGraph: DocumentGraph, currMentIdx: Int, antMentIdx: Int, addToFeaturizer: Boolean = false) = {
    GUtil.scoreIndexedFeats(featurizer.featurizeIndex(docGraph, currMentIdx, antMentIdx, false), weights);
  }
  
  def numWeights = weights.size
  
  def computeTopFeatsPerTemplate(cutoff: Int): Map[String,Counter[String]] = {
    val topFeatsPerTemplate = new HashMap[String,Counter[String]];
    for (featIdx <- 0 until weights.size) {
      val featName = featurizer.getIndexer.getObject(featIdx);
      val featTemplate = PairwiseIndexingFeaturizer.getTemplate(featName);
      val weight = weights(featIdx);
      if (!topFeatsPerTemplate.contains(featTemplate)) {
        topFeatsPerTemplate.put(featTemplate, new Counter[String]);
      }
      topFeatsPerTemplate(featTemplate).incrementCount(featName, weight);
    }
    topFeatsPerTemplate.map(entry => {
      val counter = entry._2;
      counter.keepTopNKeysByAbsValue(cutoff);
      entry._1 -> counter
    }).toMap;
  }
  
  def pack: PairwiseScorer = {
    if (!featurizer.isInstanceOf[PairwiseIndexingFeaturizerJoint]) {
      Logger.logss("Can't pack");
      this;
    } else {
      val oldFeaturizer = featurizer.asInstanceOf[PairwiseIndexingFeaturizerJoint]
      val (newFeatureIndexer, newWeights) = GUtil.packFeaturesAndWeights(featurizer.getIndexer(), weights);
      val newFeaturizer = oldFeaturizer.replaceIndexer(newFeatureIndexer);
      new PairwiseScorer(newFeaturizer, newWeights);
    } 
  }
}
