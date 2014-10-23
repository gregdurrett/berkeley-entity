package edu.berkeley.nlp.entity.joint

import scala.collection.mutable.HashMap
import edu.berkeley.nlp.entity.coref.UID
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.entity.ner.NerFeaturizer
import edu.berkeley.nlp.entity.ner.MCNerFeaturizer
import edu.berkeley.nlp.entity.coref.CorefDoc
import edu.berkeley.nlp.entity.wiki.WikipediaInterface

trait FactorGraphFactory[D,G<:JointDocFactorGraph] {
  
  def getIndexer: Indexer[String];

  def getDocFactorGraph(obj: D,
                        isGold: Boolean,
                        addToIndexer: Boolean,
                        useCache: Boolean,
                        corefLossFcn: (CorefDoc, Int, Int) => Float,
                        nerLossFcn: (String, String) => Float,
                        wikiLossFcn: (Seq[String], String) => Float): G;
  
  def getDocFactorGraphHard(obj: D, isGold: Boolean) = {
    getDocFactorGraph(obj, isGold, false, true, null, null, null);
  }
}

class FactorGraphFactoryOnto(val featurizer: JointFeaturizerShared[NerFeaturizer],
                             val wikiDB: Option[WikipediaInterface]) extends FactorGraphFactory[JointDoc,JointDocFactorGraphOnto] {
  val goldFactorGraphCache = new HashMap[UID, JointDocFactorGraphOnto]();
  val guessFactorGraphCache = new HashMap[UID, JointDocFactorGraphOnto]();
  
  private def fetchGraphCache(gold: Boolean) = {
    if (gold) {
      goldFactorGraphCache
    } else {
      guessFactorGraphCache;
    }
  }
  
  def getIndexer = featurizer.indexer;
  
  def getDocFactorGraph(doc: JointDoc,
                        gold: Boolean,
                        addToIndexer: Boolean,
                        useCache: Boolean,
                        corefLossFcn: (CorefDoc, Int, Int) => Float,
                        nerLossFcn: (String, String) => Float,
                        wikiLossFcn: (Seq[String], String) => Float): JointDocFactorGraphOnto = {
    if (useCache) {
      val cache = fetchGraphCache(gold);
      if (!cache.contains(doc.rawDoc.uid)) {
        cache.put(doc.rawDoc.uid, new JointDocFactorGraphOnto(doc, featurizer, wikiDB, gold, addToIndexer, corefLossFcn, nerLossFcn, wikiLossFcn));
      }
      cache(doc.rawDoc.uid);
    } else {
      if (corefLossFcn == null) {
        throw new RuntimeException("You called getDocFactorGraphHard but it wasn't in the cache...")
      }
      new JointDocFactorGraphOnto(doc, featurizer, wikiDB, gold, addToIndexer, corefLossFcn, nerLossFcn, wikiLossFcn)
    }
  }
}

class FactorGraphFactoryACE(val featurizer: JointFeaturizerShared[MCNerFeaturizer],
                            val wikiDB: Option[WikipediaInterface]) extends FactorGraphFactory[JointDocACE,JointDocFactorGraphACE] {
  val goldFactorGraphCache = new HashMap[UID, JointDocFactorGraphACE]();
  val guessFactorGraphCache = new HashMap[UID, JointDocFactorGraphACE]();
  
  private def fetchGraphCache(gold: Boolean) = {
    if (gold) {
      goldFactorGraphCache
    } else {
      guessFactorGraphCache;
    }
  }
  
  def getIndexer = featurizer.indexer;
  
  def getDocFactorGraph(doc: JointDocACE,
                        gold: Boolean,
                        addToIndexer: Boolean,
                        useCache: Boolean,
                        corefLossFcn: (CorefDoc, Int, Int) => Float,
                        nerLossFcn: (String, String) => Float,
                        wikiLossFcn: (Seq[String], String) => Float): JointDocFactorGraphACE = {
    if (useCache) {
      val cache = fetchGraphCache(gold);
      if (!cache.contains(doc.rawDoc.uid)) {
        cache.put(doc.rawDoc.uid, new JointDocFactorGraphACE(doc, featurizer, wikiDB, gold, addToIndexer, corefLossFcn, nerLossFcn, wikiLossFcn));
      }
      cache(doc.rawDoc.uid);
    } else {
      if (corefLossFcn == null) {
        throw new RuntimeException("You called getDocFactorGraphHard but it wasn't in the cache...")
      }
      new JointDocFactorGraphACE(doc, featurizer, wikiDB, gold, addToIndexer, corefLossFcn, nerLossFcn, wikiLossFcn)
    }
  }
}
