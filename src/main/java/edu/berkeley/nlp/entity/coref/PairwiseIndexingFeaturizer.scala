package edu.berkeley.nlp.entity.coref
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.futile.util.Logger
import scala.collection.JavaConverters._
import edu.berkeley.nlp.entity.sem.QueryCountsBundle
import edu.berkeley.nlp.entity.wiki.WikipediaInterface

trait PairwiseIndexingFeaturizer {
  
  def getIndexer(): Indexer[String];
  
  def getQueryCountsBundle: Option[QueryCountsBundle];

  def featurizeIndex(docGraph: DocumentGraph, currMentIdx: Int, antecedentIdx: Int, addToFeaturizer: Boolean): Array[Int];
}

object PairwiseIndexingFeaturizer {
  
  def printFeatureTemplateCounts(indexer: Indexer[String]) {
    val templateCounts = new Counter[String]();
    for (i <- 0 until indexer.size) {
      val template = PairwiseIndexingFeaturizer.getTemplate(indexer.get(i));
      templateCounts.incrementCount(template, 1.0);
    }
    templateCounts.keepTopNKeys(200);
    if (templateCounts.size > 200) {
      Logger.logss("Not going to print more than 200 templates");
    }
    templateCounts.keySet().asScala.toSeq.sorted.foreach(template => Logger.logss(template + ": " + templateCounts.getCount(template).toInt));
    
    val conjCounts = new Counter[String]();
    for (i <- 0 until indexer.size) {
      val currFeatureName = indexer.get(i);
      val conjStart = currFeatureName.indexOf("&C");
      if (conjStart == -1) {
        conjCounts.incrementCount("No &C", 1.0);
      } else {
        conjCounts.incrementCount(currFeatureName.substring(conjStart), 1.0);
      }
    }
    conjCounts.keepTopNKeys(1000);
    if (conjCounts.size > 1000) {
      Logger.logss("Not going to print more than 1000 templates");
    }
    conjCounts.keySet().asScala.toSeq.sorted.foreach(conj => Logger.logss(conj + ": " + conjCounts.getCount(conj).toInt));
  }
  
  def getTemplate(feat: String) = {
    val currFeatureTemplateStop = feat.indexOf("=");
    if (currFeatureTemplateStop == -1) {
      "<none>";
    } else {
      feat.substring(0, currFeatureTemplateStop);
    }
  }
}
