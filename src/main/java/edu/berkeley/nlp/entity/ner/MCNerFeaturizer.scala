package edu.berkeley.nlp.entity.ner

import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.entity.wiki.WikipediaInterface
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.futile.util.Logger
import scala.collection.JavaConverters._
import scala.collection.mutable.HashSet

@SerialVersionUID(1L)
class MCNerFeaturizer(val featureSet: Set[String],
                      val featureIndexer: Indexer[String],
                      val labelIndexer: Indexer[String],
                      val corpusCounts: CorpusCounts,
                      val wikipediaDB: Option[WikipediaInterface] = None,
                      val brownClusters: Option[Map[String,String]] = None) extends Serializable {
  Logger.logss(labelIndexer.getObjects.asScala.toSeq + " label indices");
  val nerFeaturizer = new NerFeaturizer(featureSet, featureIndexer, labelIndexer, corpusCounts, wikipediaDB, brownClusters);
  
  def replaceIndexer(newIndexer: Indexer[String]) = {
    new MCNerFeaturizer(featureSet, newIndexer, labelIndexer, corpusCounts, wikipediaDB, brownClusters);
  }
  
  def featurize(ex: MCNerExample, addToIndexer: Boolean): Array[Array[Int]] = {
    // Go from the beginning to the head index only (drop trailing PPs, etc.)
    val featsEachTokenEachLabel = nerFeaturizer.featurize(new NerExample(ex.words, ex.poss, null), addToIndexer, Some(ex.startIdx -> (ex.headIdx + 1)));
    // Append all token features for each label
    (0 until labelIndexer.size).map(labelIdx => featsEachTokenEachLabel.map(_(labelIdx)).flatten).toArray;
  }
}

object MCNerFeaturizer {
  
  val TagSet = IndexedSeq("FAC", "GPE", "LOC", "PER", "ORG", "VEH", "WEA");
  val StdLabelIndexer = new Indexer[String]();
  for (tag <- TagSet) {
    StdLabelIndexer.add(tag);
  }
  
  def apply(featureSet: Set[String],
            featureIndexer: Indexer[String],
            labelIndexer: Indexer[String],
            rawSents: Seq[Seq[String]],
            wikipediaDB: Option[WikipediaInterface],
            brownClusters: Option[Map[String,String]],
            unigramThreshold: Int = 1,
            bigramThreshold: Int = 10,
            prefSuffThreshold: Int = 1) = {
    val corpusCounts = CorpusCounts.countUnigramsBigrams(rawSents, unigramThreshold, bigramThreshold, prefSuffThreshold);
    new MCNerFeaturizer(featureSet, featureIndexer, labelIndexer, corpusCounts, wikipediaDB, brownClusters);
  }
}
