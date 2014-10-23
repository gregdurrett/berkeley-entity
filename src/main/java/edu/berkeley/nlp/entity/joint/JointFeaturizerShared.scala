package edu.berkeley.nlp.entity.joint

import edu.berkeley.nlp.entity.coref.PairwiseIndexingFeaturizer
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.entity.ner.NerSystemLabeled
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.coref.DocumentGraph
import edu.berkeley.nlp.entity.coref.PairwiseIndexingFeaturizerJoint
import edu.berkeley.nlp.entity.wiki._
import edu.berkeley.nlp.entity.Driver
import edu.berkeley.nlp.entity.ner.MCNerFeaturizer
import edu.berkeley.nlp.entity.ner.NerFeaturizer

@SerialVersionUID(1L)
class JointFeaturizerShared[T](val corefFeaturizer: PairwiseIndexingFeaturizer,
                               val nerFeaturizer: T,
                               val maybeBrownClusters: Option[Map[String,String]],
                               val corefNerFeatures: String,
                               val corefWikiFeatures: String,
                               val wikiNerFeatures: String,
                               val indexer: Indexer[String]) extends Serializable {
  
  val BrownLen = Driver.corefNerBrownLength
  
  def canReplaceIndexer = {
    corefFeaturizer.isInstanceOf[PairwiseIndexingFeaturizerJoint] && (nerFeaturizer.isInstanceOf[MCNerFeaturizer] || nerFeaturizer.isInstanceOf[NerFeaturizer]);
  }
  
  def replaceIndexer(newIndexer: Indexer[String]): JointFeaturizerShared[T] = {
    if (!canReplaceIndexer) {
      throw new RuntimeException("Error: can't replace indexer in JointFeaturizerShared with the given type parameters, " +
                                 "so don't try to. Catch and prevent this with canReplaceIndexer");
    }
    val newCorefFeaturizer = if (corefFeaturizer.isInstanceOf[PairwiseIndexingFeaturizerJoint]) {
      corefFeaturizer.asInstanceOf[PairwiseIndexingFeaturizerJoint].replaceIndexer(newIndexer)
    } else {
      throw new RuntimeException("Can't replace for " + corefFeaturizer.getClass());
    }
    val newNerFeaturizer = if (nerFeaturizer.isInstanceOf[MCNerFeaturizer]) {
      nerFeaturizer.asInstanceOf[MCNerFeaturizer].replaceIndexer(newIndexer);
    } else if (nerFeaturizer.isInstanceOf[NerFeaturizer]) {
      nerFeaturizer.asInstanceOf[NerFeaturizer].replaceIndexer(newIndexer);
    } else {
      throw new RuntimeException("Can't replace for " + nerFeaturizer.getClass());
    }
    new JointFeaturizerShared(newCorefFeaturizer, newNerFeaturizer.asInstanceOf[T], maybeBrownClusters, corefNerFeatures, corefWikiFeatures, wikiNerFeatures, newIndexer);
  }
  
  def indexFeature(feat: String, addToIndexer: Boolean): Int = {
    if (addToIndexer) indexer.getIndex(feat); else indexer.indexOf(feat);
  }
  
  def indexFeatures(feats: Array[String], addToIndexer: Boolean): Array[Int] = {
    if (addToIndexer) {
      feats.map(indexer.getIndex(_));
    } else {
      feats.map(indexer.indexOf(_)).filter(_ != -1);
    }
  }
  
  def maybeAddFeat(indexedFeats: ArrayBuffer[Int], feat: String, addToIndexer: Boolean) {
    if (addToIndexer) {
      indexedFeats += indexer.getIndex(feat)
    } else {
      val idx = indexer.indexOf(feat)
      if (idx != -1) indexedFeats += idx;
    }
  }
  
  def maybeAddFeats(indexedFeats: ArrayBuffer[Int], feats: Seq[String], addToIndexer: Boolean) {
    feats.map(maybeAddFeat(indexedFeats, _, addToIndexer));
  }
  
  def getCorefNerFeatures(docGraph: DocumentGraph, currIdx: Int, antIdx: Int, nerSymbolCurr: String, nerSymbolPrev: String, addToIndexer: Boolean) = {
    val feats = new ArrayBuffer[Int];
    val currSemType = NerSystemLabeled.getSemanticType(nerSymbolCurr);
    val antSemType = NerSystemLabeled.getSemanticType(nerSymbolPrev);
    val corefFeaturizerCast = corefFeaturizer.asInstanceOf[PairwiseIndexingFeaturizerJoint];
    val ment = docGraph.getMention(currIdx);
    val antMent = docGraph.getMention(antIdx);
    val mentIsClosedClass = ment.mentionType.isClosedClass();
    val antMentIsClosedClass = antMent.mentionType.isClosedClass();
    val restrictCC = corefNerFeatures.contains("restrictcc");
    val useBrownClusters = corefNerFeatures.contains("brown");
//    val mentIsClosedClass = false
//    val antMentIsClosedClass = false
    if (corefNerFeatures.contains("indicators")) {
      maybeAddFeat(feats, "TagPair=" + currSemType + "-" + antSemType, addToIndexer);
    }
    if (corefNerFeatures.contains("currlex")) {
      if (restrictCC && mentIsClosedClass) {
        maybeAddFeat(feats, "PrevHeadCurrSC=" + antMent.headStringLc + "-" + currSemType, addToIndexer);
      } else {
        maybeAddFeats(feats, Array("PrevHeadCurrSC=" + corefFeaturizerCast.fetchHeadWordOrPos(antMent) + "-" + currSemType,
                                   "PrevFirstCurrSC=" + corefFeaturizerCast.fetchFirstWordOrPos(antMent) + "-" + currSemType), addToIndexer);
      }
      maybeAddFeats(feats, Array("PrevPrecedingCurrSC=" + corefFeaturizerCast.fetchPrecedingWordOrPos(antMent) + "-" + currSemType,
                                 "PrevFollowingCurrSC=" + corefFeaturizerCast.fetchFollowingWordOrPos(antMent) + "-" + currSemType), addToIndexer);
      if (useBrownClusters) {
        maybeAddFeats(feats, Array("PrevHeadBrownCurrSC=" + fetchBrownCluster(antMent.headString) + "-" + currSemType,
                                   "PrevFirstBrownCurrSC=" + fetchBrownCluster(antMent.words(0)) + "-" + currSemType,
                                   "PrevPrecedingBrownCurrSC=" + fetchBrownCluster(antMent.contextWordOrPlaceholder(-1)) + "-" + currSemType,
                                   "PrevFollowingBrownCurrSC=" + fetchBrownCluster(antMent.contextWordOrPlaceholder(antMent.words.size), BrownLen) + "-" + currSemType), addToIndexer);
      }
    }
    if (corefNerFeatures.contains("antlex")) {
      if (restrictCC && mentIsClosedClass) {
        maybeAddFeat(feats, "CurrHeadPrevSC=" + ment.headStringLc + "-" + antSemType, addToIndexer);
      } else {
        maybeAddFeats(feats, Array("CurrHeadPrevSC=" + corefFeaturizerCast.fetchHeadWordOrPos(ment) + "-" + antSemType,
                                   "CurrFirstPrevSC=" + corefFeaturizerCast.fetchFirstWordOrPos(ment) + "-" + antSemType), addToIndexer);
      }
      maybeAddFeats(feats, Array("CurrPrecedingPrevSC=" + corefFeaturizerCast.fetchPrecedingWordOrPos(ment) + "-" + antSemType,
                                 "CurrFollowingPrevSC=" + corefFeaturizerCast.fetchFollowingWordOrPos(ment) + "-" + antSemType), addToIndexer);
      if (useBrownClusters) {
        maybeAddFeats(feats, Array("CurrHeadBrownPrevSC=" + fetchBrownCluster(ment.headString) + "-" + antSemType,
                                   "CurrFirstBrownPrevSC=" + fetchBrownCluster(ment.words(0)) + "-" + antSemType,
                                   "CurrPrecedingBrownPrevSC=" + fetchBrownCluster(ment.contextWordOrPlaceholder(-1)) + "-" + antSemType,
                                   "CurrFollowingBrownPrevSC=" + fetchBrownCluster(ment.contextWordOrPlaceholder(ment.words.size), BrownLen) + "-" + antSemType), addToIndexer);
      }
    }
    if (corefNerFeatures.contains("hmc") && !mentIsClosedClass && !antMentIsClosedClass) {
      maybeAddFeats(feats, Array("ThisHeadContainedAndTypes=" + (antMent.spanToString.contains(ment.headString) + "-" + currSemType + "-" + antSemType),
                                 "AntHeadContainedAndTypes=" + (ment.spanToString.contains(antMent.headString) + "-" + currSemType + "-" + antSemType)), addToIndexer);
    }
    feats.toArray;
  }
  
  private def fetchBrownCluster(word: String): String = fetchBrownCluster(word, BrownLen);
  
  private def fetchBrownCluster(word: String, length: Int): String = {
    if (maybeBrownClusters.isDefined && maybeBrownClusters.get.contains(word)) {
      val cluster = maybeBrownClusters.get(word);
      cluster.slice(0, Math.min(cluster.size, length));
    } else {
      ""
    }
  }
  
//  def getCorefWikiFeatures(docGraph: DocumentGraph, currIdx: Int, antIdx: Int, wikiTitle: String, addToIndexer: Boolean) = {
//    val feats = new ArrayBuffer[Int];
//    val currMent = docGraph.getMention(currIdx);
//    val antMent = docGraph.getMention(antIdx);
//    if (corefWikiFeatures.contains("categories")) {
//      val corefFeaturizerCast = corefFeaturizer.asInstanceOf[PairwiseIndexingFeaturizerJoint];
//      if (wikiTitle != NilToken) {
//        for (category <- wikiFeaturizer.wikiCategoryDB.get.getCategories(wikiTitle)) {
//          maybeAddFeat(feats, "PrevHeadCurrCategory=" + corefFeaturizerCast.fetchHeadWordOrPos(antMent) + "-" + category, addToIndexer);
//        }
//      }
//    }
//    feats.toArray;
//  }
  
  def getWikiNerFeatures(docGraph: DocumentGraph, currIdx: Int, wikiTitle: String, nerSymbol: String, wikiDB: Option[WikipediaInterface], addToIndexer: Boolean) = {
    val feats = new ArrayBuffer[Int];
    val currMent = docGraph.getMention(currIdx);
    if (wikiTitle != NilToken) {
      if (wikiNerFeatures.contains("categories")) {
        for (category <- wikiDB.get.getCategories(wikiTitle)) {
          maybeAddFeat(feats, "SemTypeAndCategory=" + category + "-" + nerSymbol, addToIndexer);
        }
      }
      if (wikiNerFeatures.contains("infoboxes")) {
        val infobox = wikiDB.get.getInfobox(wikiTitle)
        if (infobox != "") {
          maybeAddFeat(feats, "SemTypeAndInfobox=" + infobox + "-" + nerSymbol, addToIndexer);
          val infoboxHead = wikiDB.get.getInfoboxHead(wikiTitle)
          maybeAddFeat(feats, "SemTypeAndInfoboxHead=" + infoboxHead + "-" + nerSymbol, addToIndexer);
        }
      }
      if (wikiNerFeatures.contains("appositives")) {
        val appositive = wikiDB.get.getAppositive(wikiTitle)
        if (appositive != "") {
          maybeAddFeat(feats, "SemTypeAndAppositive =" + appositive + "-" + nerSymbol, addToIndexer);
        }
      }
    } else {
      if (wikiNerFeatures.contains("niltype")) {
        maybeAddFeat(feats, "SemTypeAndNil=" + nerSymbol, addToIndexer);
      }
      if (wikiNerFeatures.contains("nillexical")) {
        val corefFeaturizerCast = corefFeaturizer.asInstanceOf[PairwiseIndexingFeaturizerJoint];
        maybeAddFeat(feats, "HeadAndNil=" + corefFeaturizerCast.fetchHeadWordOrPos(docGraph.getMention(currIdx)), addToIndexer);
      }
    }
    feats.toArray;
  }
  
  def getCorefWikiFeatures(docGraph: DocumentGraph, currIdx: Int, antIdx: Int, wikiTitle: String, prevWikiTitle: String, wikiDB: Option[WikipediaInterface], addToIndexer: Boolean) = {
    val feats = new ArrayBuffer[Int];
    val currMent = docGraph.getMention(currIdx);
    val antMent = docGraph.getMention(antIdx);
    if (corefWikiFeatures.contains("basic")) {
      if (wikiTitle != NilToken && prevWikiTitle != NilToken) {
        if (wikiTitle == prevWikiTitle) {
          maybeAddFeat(feats, "SameWikTitle=true", addToIndexer);
        } else {
          maybeAddFeat(feats, "ShareOutLink=" + wikiDB.get.linksDB.doPagesShareOutLink(wikiTitle, prevWikiTitle), addToIndexer);
          maybeAddFeat(feats, "LinkToEachOther=" + wikiDB.get.linksDB.doesOneLinkToOther(wikiTitle, prevWikiTitle), addToIndexer);
        }
      }
    }
    if (corefWikiFeatures.contains("lastnames")) {
      maybeAddFeat(feats, "CurrentAntNil=" + (wikiTitle != NilToken) + "," + (prevWikiTitle != NilToken), addToIndexer);
      if (JointFeaturizerShared.isPotentialSharedWikificationPair(docGraph, currIdx, antIdx)) {
        maybeAddFeat(feats, "STCurrentAntNil=" + (wikiTitle != NilToken) + "," + (prevWikiTitle != NilToken), addToIndexer);
        maybeAddFeat(feats, "STSameWikiTitle=" + (wikiTitle == prevWikiTitle), addToIndexer);
      }
    }
    feats.toArray;
  }
}

object JointFeaturizerShared {
  
  /**
   * A shared Wikification target is a mention that is a possible antecedent which is 
   * a superstring of the current  mention; it therefore has more information and its
   * options should be copied to the current mention.
   */
  def isPotentialSharedWikificationPair(docGraph: DocumentGraph, currIdx: Int, antIdx: Int): Boolean = {
    val antWords = docGraph.getMention(antIdx).words
    val currWords = docGraph.getMention(currIdx).words
    !docGraph.isPruned(currIdx, antIdx) && antWords.containsSlice(currWords) && antWords.size > currWords.size;
  }
}
