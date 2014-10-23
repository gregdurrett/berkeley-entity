package edu.berkeley.nlp.entity.xdistrib

import scala.collection.mutable.ArrayBuffer

import edu.berkeley.nlp.entity.coref.DocumentGraph
import edu.berkeley.nlp.entity.coref.FeatureSetSpecification
import edu.berkeley.nlp.entity.coref.LexicalCountsBundle
import edu.berkeley.nlp.entity.wiki.WikipediaInterface
import edu.berkeley.nlp.entity.sem.QueryCountsBundle
import edu.berkeley.nlp.entity.sem.SemClasser
import edu.berkeley.nlp.futile.fig.basic.Indexer

class ComponentFeaturizer(val componentIndexer: Indexer[String],
                          val featureSet: FeatureSetSpecification,
                          val lexicalCounts: LexicalCountsBundle,
                          val queryCounts: Option[QueryCountsBundle],
                          val wikipediaInterface: Option[WikipediaInterface],
                          val semClasser: Option[SemClasser]) {

  def featurizeComponents(docGraph: DocumentGraph, idx: Int, addToFeaturizer: Boolean): Array[Int] = {
    val feats = new ArrayBuffer[Int];
    def addFeatureShortcut = (featName: String) => {
      // Only used in CANONICAL_ONLY_PAIR, so only compute the truth value in this case
      if (addToFeaturizer || componentIndexer.contains(featName)) {
        feats += componentIndexer.getIndex(featName);
      }
    }
    val ment = docGraph.getMention(idx);
    if (!ment.mentionType.isClosedClass) {
      // WORDS
      if (featureSet.featsToUse.contains("comp-word")) {
        if (lexicalCounts.commonHeadWordCounts.containsKey(ment.headStringLc)) {
          addFeatureShortcut("CHead=" + ment.headStringLc);
        } else {
          addFeatureShortcut("CHead=" + ment.headPos);
        }
      }
      // SEMCLASS
      if (featureSet.featsToUse.contains("comp-sc") && semClasser.isDefined) {
        addFeatureShortcut("CSC=" + semClasser.get.getSemClass(ment, docGraph.cachedWni));
      }
      // WIKIPEDIA
      if (featureSet.featsToUse.contains("comp-wiki") && wikipediaInterface.isDefined) {
        val title = wikipediaInterface.get.disambiguate(ment);
        val topCategory = wikipediaInterface.get.getTopKCategoriesByFrequency(title, 1);
        if (topCategory.size >= 1) {
          addFeatureShortcut("CCategory=" + wikipediaInterface.get.getInfoboxHead(title));
        }
        addFeatureShortcut("CInfo=" + wikipediaInterface.get.getInfoboxHead(title));
        addFeatureShortcut("CApp=" + wikipediaInterface.get.getAppositive(title));
      }
    }
    feats.toArray;
  }
}
