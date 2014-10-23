package edu.berkeley.nlp.entity.coref
import scala.collection.JavaConverters._
import edu.berkeley.nlp.entity.GUtil
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.util.Random
import edu.berkeley.nlp.entity.sem.SemClass._
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.futile.util.Iterators
import edu.berkeley.nlp.futile.util.Logger
import edu.mit.jwi.item.Pointer
import edu.berkeley.nlp.entity.sem.SemClass
import edu.berkeley.nlp.entity.WordNetInterfacer

class DocumentGraph(val corefDoc: CorefDoc,
                    val addToFeaturizer: Boolean) {
  // addToFeaturizer should be true for train documents (if a feature is unseen on
  // these, we add it to the featurizer) and false for dev/test documents
  // By convention: a feature vector is empty if it has been pruned
  val emptyIntArray = Array[Int]();
  var cachedFeats = new Array[Array[Array[Int]]](corefDoc.numPredMents);
  for (i <- 0 until corefDoc.numPredMents) {
    cachedFeats(i) = Array.fill(i+1)(emptyIntArray);
  }
  // These are just here so we don't have to reinstantiate them; they should
  // be overwritten every time the weights change (which is all the time)
  val cachedScoreMatrix = new Array[Array[Float]](corefDoc.numPredMents);
  val cachedMarginalMatrix = new Array[Array[Float]](corefDoc.numPredMents);
  for (i <- 0 until corefDoc.numPredMents) {
    cachedScoreMatrix(i) = Array.fill(i+1)(0.0F);
    cachedMarginalMatrix(i) = Array.fill(i+1)(0.0F);
  }
  // Only used for DocumentInferencerRahman
  val cachedMentClusterMapping = new MentClusterMapping(corefDoc.numPredMents);
  
  var cachedFeaturizer: PairwiseIndexingFeaturizer = null;
  var cacheEmpty = true;
  // If an edge is pruned, it will never be featurized
  var prunedEdges = new Array[Array[Boolean]](corefDoc.numPredMents);
  for (i <- 0 until prunedEdges.size) {
    prunedEdges(i) = Array.fill(i+1)(false);
  }
  
  // Cached information for feature computation
  val storedClusterPosteriors = new ArrayBuffer[Array[Array[Float]]]();
  val storedDistributedLabels = new ArrayBuffer[Array[Array[Int]]]();
  val storedSemClass: Array[Option[SemClass]] = Array.tabulate(this.size)(i => None);
  val storedRelsBetter = new Array[HashMap[Seq[Pointer],Set[String]]](this.size);
  val storedRelsBetterCumulative = new Array[HashMap[Seq[Pointer],Set[String]]](this.size);
  val cachedMentionHeadMatchStatus: Array[Option[Boolean]] = Array.tabulate(this.size)(i => None);
  
  // WordNetInterfacer so the featurizer can find it if it needs to
  var cachedWni: WordNetInterfacer = null;
  
  def size() = corefDoc.numPredMents
  
  def getMention(idx: Int) = corefDoc.predMentions(idx);
  
  def getMentions() = corefDoc.predMentions;
  
  def getOraclePredClustering() = corefDoc.getOraclePredClustering;
  
  def getMentionStrAndContext(idx: Int): String = {
    val ment = getMention(idx);
    val mentionStart = ment.startIdx;
    val mentionEnd = ment.endIdx;
    val sentence = corefDoc.rawDoc.words(ment.sentIdx);
    val contextStart = Math.max(0, mentionStart - 3);
    val contextEnd = Math.min(mentionEnd + 3, sentence.size);
    (sentence.slice(contextStart, mentionStart).foldLeft("")(_ + " " + _) + " [" + sentence.slice(mentionStart, mentionEnd).foldLeft("")(_ + " " + _) +
      "] " + sentence.slice(mentionEnd, contextEnd).foldLeft("")(_ + " " + _)).trim(); 
  }
  
  def isGoldNoPruning(currIdx: Int, antecedentIdx: Int) = getGoldAntecedentsNoPruning(currIdx).contains(antecedentIdx);
  
  def isGoldCurrentPruning(currIdx: Int, antecedentIdx: Int) = getGoldAntecedentsUnderCurrentPruning(currIdx).contains(antecedentIdx);
  
  def isPruned(currIdx: Int, antecedentIdx: Int): Boolean = prunedEdges(currIdx)(antecedentIdx);
  
  def getPrunedDomain(idx: Int, gold: Boolean): Array[Int] = {
    val currAntecedents = getGoldAntecedentsUnderCurrentPruning(idx);
    val domainSeq = new ArrayBuffer[Int]();
    for (j <- 0 to idx) {
      if (!isPruned(idx, j) && (!gold || currAntecedents.contains(j))) {
        domainSeq += j;
      }
    }
    domainSeq.toArray;
  }
  
  def pruneEdgesMentDistanceSentDistance(maxBackptrMentDistance: Int, maxPronounSentDistance: Int) {
    for (i <- 0 until prunedEdges.size) {
      val iSentIdx = getMention(i).sentIdx;
      for (j <- 0 to i) {
        val jSentIdx = getMention(j).sentIdx;
        if (j < i - maxBackptrMentDistance || (getMention(i).mentionType == MentionType.PRONOMINAL && iSentIdx - jSentIdx > maxPronounSentDistance)) {
          prunedEdges(i)(j) = true;
          cachedFeats(i)(j) = emptyIntArray;
        }
      }
    }
  }
  
  def pruneEdgesModel(model: PairwiseScorer, logPruningThreshold: Double) {
    for (i <- 0 until prunedEdges.size) {
      val scores = (0 to i).map(j => model.score(this, i, j, false));
      val bestIdx = GUtil.argMaxIdxFloat(scores);
      for (j <- 0 to i) {
        if (scores(j) < scores(bestIdx) + logPruningThreshold) {
          prunedEdges(i)(j) = true;
          cachedFeats(i)(j) = emptyIntArray;
        }
      }
    }
  }
  
  def computePruningStats(): PruningStats = {
    var totalMentions = 0;
    var totalAnaphoricMentions = 0;
    var totalEdges = 0;
    var edgesPruned = 0;
    var numGoldBackptrs = 0;
    var numGoldBackptrsPruned = 0;
    var numAllBackptrsPruned = 0;
    var numAnaphoricAllBackptrsPruned = 0;
    for (i <- 0 until this.size) {
      totalMentions += 1;
      val thisAntecedentsNoPruning = getGoldAntecedentsNoPruning(i);
      val thisAntecedentsWithPruning = getGoldAntecedentsUnderCurrentPruningOrEmptySet(i);
      totalEdges += (i+1);
      edgesPruned += prunedEdges(i).foldRight(0)((pruned: Boolean, value: Int) => if (pruned) value + 1 else value);
      val goldAntecedentIsSelf = thisAntecedentsNoPruning.size == 1 && thisAntecedentsNoPruning(0) == i;
      val allAntecedentsPruned = thisAntecedentsWithPruning.size == 0;
      totalAnaphoricMentions += (if (goldAntecedentIsSelf) 0 else 1);
      numGoldBackptrs += thisAntecedentsNoPruning.size;
      val numAntecedentsPruned = thisAntecedentsNoPruning.size - thisAntecedentsWithPruning.size; 
      numGoldBackptrsPruned += numAntecedentsPruned;
      numAllBackptrsPruned += (if (allAntecedentsPruned) 1 else 0);
      numAnaphoricAllBackptrsPruned += (if (!goldAntecedentIsSelf && allAntecedentsPruned) 1 else 0);
    }
    new PruningStats(totalMentions, totalAnaphoricMentions, totalEdges, edgesPruned, numGoldBackptrs, numGoldBackptrsPruned, numAllBackptrsPruned, numAnaphoricAllBackptrsPruned);
  }
  
  def getGoldClustersNoPruning(): Seq[Seq[Mention]] = {
    val allClusters = new ArrayBuffer[Seq[Mention]]();
    val oracleClustering = corefDoc.getOraclePredClustering
    for (cluster <- oracleClustering.clusters) {
//      val clusterIndices = cluster.asScala.map(_.mentionID).toSeq;
//      val clusterIndices2 = cluster.asScala.map(doc.predMentions.indexOf(_)).toSeq;
//      require(clusterIndices == clusterIndices2);
      allClusters += cluster.map(getMention(_));
    }
    allClusters;
  }
  
  def getAllAntecedentsCurrentPruning(idx: Int): Seq[Int] = {
    val antecedents = new ArrayBuffer[Int];
    for (i <- 0 to idx) {
      if (!prunedEdges(idx)(i)) {
        antecedents += i;
      }
    }
    antecedents;
  }
  
  def getGoldAntecedentsNoPruning(): Array[Seq[Int]] = {
    (0 until this.size).map(getGoldAntecedentsNoPruning(_)).toArray;
  }
  
  def getGoldAntecedentsNoPruning(idx: Int): Seq[Int] = {
    val oracleClustering = corefDoc.getOraclePredClustering
    val antecedents = oracleClustering.getAllAntecedents(idx);
    if (antecedents.isEmpty) Seq(idx) else antecedents;
  }
  
  // This and the following return the set of allowed antecedents if all gold
  // antecedents have been pruned; effectively this ignores examples where
  // there is no gold. Always returns nonempty.
  def getGoldAntecedentsUnderCurrentPruning(): Array[Seq[Int]] = {
    (0 until this.size).map(getGoldAntecedentsUnderCurrentPruning(_)).toArray;
  }
  
  def getGoldAntecedentsUnderCurrentPruning(idx: Int): Seq[Int] = {
    val oracleClustering = corefDoc.getOraclePredClustering
    val antecedentsRaw = oracleClustering.getAllAntecedents(idx);
    val antecedents = if (antecedentsRaw.isEmpty) Seq(idx) else antecedentsRaw;
    val unprunedAntecedents = antecedents.filter(j => !prunedEdges(idx)(j))
    if (unprunedAntecedents.isEmpty) {
      // This is a little inefficient but this code isn't called that much (extremely rare in coarse pass
      // and generally not called for nonanaphoric guys, and most things are nonanaphoric)
      val allUnprunedBackptrs = prunedEdges(idx).zipWithIndex.filter((prunedAndIdx) => !prunedAndIdx._1).map(_._2).toSeq;
      allUnprunedBackptrs
    } else {
      unprunedAntecedents;
    }
  }
  
  // This and the following return the set of unpruned antecedents, possibly empty
  def getGoldAntecedentsUnderCurrentPruningOrEmptySet(): Array[Seq[Int]] = {
    (0 until this.size).map(getGoldAntecedentsUnderCurrentPruningOrEmptySet(_)).toArray;
  }
  
  def getGoldAntecedentsUnderCurrentPruningOrEmptySet(idx: Int): Seq[Int] = {
    val oracleClustering = corefDoc.getOraclePredClustering
    val antecedentsRaw = oracleClustering.getAllAntecedents(idx);
    val antecedents = if (antecedentsRaw.isEmpty) Seq(idx) else antecedentsRaw;
    val unprunedAntecedents = antecedents.filter(j => !prunedEdges(idx)(j))
    unprunedAntecedents;
  }

  // N.B. The matrices returned by this method are volatile. The feats one hangs around
  // unless you refeaturize, but the other one gets mutated every time you call this
  // method (though obviously it's only different if you prune or if the weights have changed).
  def featurizeIndexAndScoreNonPrunedUseCache(scorer: PairwiseScorer): (Array[Array[Array[Int]]], Array[Array[Float]]) = {
    val featsChart = featurizeIndexNonPrunedUseCache(scorer.featurizer);
    val scoreChart = cachedScoreMatrix;
    for (i <- 0 until corefDoc.numPredMents) {
      for (j <- 0 to i) {
        if (!prunedEdges(i)(j)) {
          require(featsChart(i)(j).size > 0);
          scoreChart(i)(j) = GUtil.scoreIndexedFeats(featsChart(i)(j), scorer.weights);
        } else {
          scoreChart(i)(j) = Float.NegativeInfinity;
        }
      }
    }
    (featsChart, scoreChart)
  }
  
  // How does this know whether or not to add features? The private variable addToFeatures...
  // a bit of a hack...
  def featurizeIndexNonPrunedUseCache(featurizer: PairwiseIndexingFeaturizer): Array[Array[Array[Int]]] = {
    if (cacheEmpty || featurizer != cachedFeaturizer) {
      cachedFeats = featurizeIndexNonPruned(featurizer);
      cachedFeaturizer = featurizer;
      cacheEmpty = false;
    }
    cachedFeats;
  }

  private def featurizeIndexNonPruned(featurizer: PairwiseIndexingFeaturizer): Array[Array[Array[Int]]] = {
    val featsChart = new Array[Array[Array[Int]]](corefDoc.numPredMents);
    for (i <- 0 until corefDoc.numPredMents) {
      featsChart(i) = new Array[Array[Int]](i+1);
      for (j <- 0 to i) {
        if (!prunedEdges(i)(j)) {
          featsChart(i)(j) = featurizer.featurizeIndex(this, i, j, addToFeaturizer);
        }
      }
//      Logger.logss(i + ": " + featsChart(i).map(_.head).toSeq);
    }
    featsChart;
  }
  
  def scoreNonPrunedUseCache(weights: Array[Float]): Array[Array[Float]] = {
    val featsChart = cachedFeats;
    val scoreChart = cachedScoreMatrix;
    for (i <- 0 until corefDoc.numPredMents) {
      for (j <- 0 to i) {
        if (!prunedEdges(i)(j)) {
          require(featsChart(i)(j).size > 0);
          scoreChart(i)(j) = GUtil.scoreIndexedFeats(featsChart(i)(j), weights);
        } else {
          scoreChart(i)(j) = Float.NegativeInfinity;
        }
      }
    }
    scoreChart
  }
  
  def setPrunedEdges(prunedEdges: Array[Array[Boolean]]) {
    this.prunedEdges = prunedEdges;
    for (i <- 0 until prunedEdges.size) {
      for (j <- 0 until prunedEdges(i).size) {
        if (prunedEdges(i)(j)) {
          cachedFeats(i)(j) = emptyIntArray;
        }
      }
    }
  }
  
  def clearFeatureCache() {
    for (i <- 0 until cachedFeats.size) {
      for (j <- 0 until cachedFeats(i).size) {
        cachedFeats(i)(j) = emptyIntArray;
      }
    }
  }
  
  def printAverageFeatureCountInfo() {
    var numerAnaphoric = 0;
    var denomAnaphoric = 0;
    var numerNonanaphoric = 0;
    var denomNonanaphoric = 0;
    for (i <- 0 until cachedFeats.size) {
      for (j <-0 until cachedFeats(i).size) {
        if (!prunedEdges(i)(j)) {
          if (i != j) {
            numerAnaphoric += cachedFeats(i)(j).size;
            denomAnaphoric += 1;
          } else {
            numerNonanaphoric += cachedFeats(i)(j).size;
            denomNonanaphoric += 1;
          }
        }
      }
    }
    Logger.logss("Avg feature counts anaphoric: " + numerAnaphoric.toDouble/denomAnaphoric.toDouble);
    Logger.logss("Avg feature counts nonanaphoric: " + numerNonanaphoric.toDouble/denomNonanaphoric.toDouble);
  }
  
  // Caching various information that we might want to use later
  
//  def computeAndStoreClusterPosteriors(clusterer: Clusterer) {
//    val clusterPosteriors = new Array[Array[Float]](this.size);
//    for (i <- 0 until size) {
////      val currMent = doc.predMentions.get(i);
////      clusterPosteriors(i) = clusterer.computeClusterPosteriors(getMentionInContext(currMent.sentNum, currMent.startIndex, currMent.endIndex, currMent.headIndex));
//      
//      val currMent = getMention(i);
//      clusterPosteriors(i) = clusterer.computeClusterPosteriors(getMentionInContext(currMent.sentIdx, currMent.startIdx, currMent.endIdx, currMent.headIdx));
//      // Do a little smoothing on the posteriors
//      (0 until clusterPosteriors(i).size).foreach(j => clusterPosteriors(i)(j) += 1e-10);
//    }
//    this.storedClusterPosteriors += clusterPosteriors;
//  }
//  
//  def computeAndStoreDistributedLabels(clustererIdx: Int, lowerThresholds: Array[Float], upperThresholds: Array[Float]) {
//    val distributedLabels = new Array[Array[Int]](this.size);
//    for (i <- 0 until size) {
//      distributedLabels(i) = new Array[Int](this.storedClusterPosteriors(clustererIdx)(i).size);
//      for (j <- 0 until this.storedClusterPosteriors(clustererIdx)(i).size) {
//        val posterior = this.storedClusterPosteriors(clustererIdx)(i)(j);
//        distributedLabels(i)(j) = if (posterior > upperThresholds(j)) 1 else if (posterior < lowerThresholds(j)) 0 else -1;
//      }
//    }
//    this.storedDistributedLabels += distributedLabels;
//  }
  
  def numClusterers = storedClusterPosteriors.size;
  
  def numClusters(clustererIdx: Int) = storedClusterPosteriors(clustererIdx)(0).size;
  
  def getClusterPosteriors(clustererIdx: Int, mentIdx: Int): Array[Float] = {
    storedClusterPosteriors(clustererIdx)(mentIdx);
  }
  
  def getBestCluster(clustererIdx: Int, mentIdx: Int): Int = {
    var bestScore = Float.NegativeInfinity;
    var bestIdx = -1;
    for (i <- 0 until storedClusterPosteriors(clustererIdx)(mentIdx).length) {
      if (storedClusterPosteriors(clustererIdx)(mentIdx)(i) > bestScore) {
        bestScore = storedClusterPosteriors(clustererIdx)(mentIdx)(i);
        bestIdx = i;
      }
    }
    bestIdx;
  }
  
  def computeAndStorePhiPosteriors(useNumber: Boolean, useGender: Boolean, useNert: Boolean) {
    if (useNumber) {
      computeAndStorePhiPosterior((ment: Mention) => ment.number.ordinal(), Number.values().size - 1, Number.UNKNOWN.ordinal())
    }
    if (useGender) {
      computeAndStorePhiPosterior((ment: Mention) => ment.gender.ordinal(), Gender.values().size - 1, Gender.UNKNOWN.ordinal())
    }
  }
  
  def computeAndStorePhiPosterior(fcn: (Mention => Int), domainSize: Int, unknown: Int) {
    val EstimatorConfidence = 0.75F;
    val posteriors = new Array[Array[Float]](this.size);
    for (i <- 0 until size) {
      val idx = fcn(getMention(i));
      if (idx == unknown || idx == -1) {
        posteriors(i) = Array.tabulate(domainSize)(j => 1.0F/domainSize);
      } else if (idx >= domainSize) {
        throw new RuntimeException("Bad idx: " + idx + " for domain size " + domainSize + " " + getMention(i).nerString);
      } else {
        posteriors(i) = Array.tabulate(domainSize)(j => (1.0F - EstimatorConfidence)/domainSize);
        posteriors(i)(idx) += EstimatorConfidence;
      }
    }
    this.storedClusterPosteriors += posteriors;
  }
  
  def getSemClassUseCache(wordNetInterfacer: WordNetInterfacer, idx: Int) = {
    if (!storedSemClass(idx).isDefined) {
      storedSemClass(idx) = Some(SemClass.getSemClass(getMention(idx).headStringLc, getMention(idx).nerString, wordNetInterfacer))
    }
    storedSemClass(idx).getOrElse(SemClass.Other);
  }
  
  def getWordNetRelsBetterUseCache(wordNetInterfacer: WordNetInterfacer, rels: Seq[Pointer], idx: Int) = {
    if (storedRelsBetter(idx) == null) {
      storedRelsBetter(idx) = new HashMap[Seq[Pointer],Set[String]];
    }
    if (!storedRelsBetter(idx).contains(rels)) {
      storedRelsBetter(idx).put(rels, wordNetInterfacer.getWordsOnSynsetRelation(getMention(idx), rels).toSet);
    }
    storedRelsBetter(idx)(rels);
  }
  
  def getWordNetRelsBetterCumulativeUseCache(wordNetInterfacer: WordNetInterfacer, rels: Seq[Pointer], idx: Int) = {
    if (storedRelsBetterCumulative(idx) == null) {
      storedRelsBetterCumulative(idx) = new HashMap[Seq[Pointer],Set[String]];
    }
    if (!storedRelsBetterCumulative(idx).contains(rels)) {
      storedRelsBetterCumulative(idx).put(rels, wordNetInterfacer.getWordsUpToSynsetRelation(getMention(idx), rels).toSet);
    }
    storedRelsBetterCumulative(idx)(rels);
  }
  
  def getHeadMatchStatus(idx: Int) = {
    if (!cachedMentionHeadMatchStatus(idx).isDefined) {
      cachedMentionHeadMatchStatus(idx) = Some((0 until idx).map(i => (getMention(i).headStringLc == getMention(idx).headStringLc)).foldLeft(false)(_ || _));
    }
    cachedMentionHeadMatchStatus(idx).getOrElse(false);
  }
  
  def cacheWordNetInterfacer(wni: WordNetInterfacer) = {
    this.cachedWni = wni;
  }
}

case class PruningStats(val totalMentions: Int,
                        val totalAnaphoricMentions: Int,
                        val totalEdges: Int,
                        val edgesPruned: Int,
                        val numGoldBackptrs: Int,
                        val numGoldBackptrsPruned: Int,
                        val numAllBackptrsPruned: Int,
                        val numAnaphoricAllBackptrsPruned: Int) {
  def add(other: PruningStats) = { new PruningStats(this.totalMentions + other.totalMentions,
                                                    this.totalAnaphoricMentions + other.totalAnaphoricMentions,
                                                    this.totalEdges + other.totalEdges,
                                                    this.edgesPruned + other.edgesPruned,
                                                    this.numGoldBackptrs + other.numGoldBackptrs,
                                                    this.numGoldBackptrsPruned + other.numGoldBackptrsPruned,
                                                    this.numAllBackptrsPruned + other.numAllBackptrsPruned,
                                                    this.numAnaphoricAllBackptrsPruned + other.numAnaphoricAllBackptrsPruned); }
  
  override def toString(): String = {
    "totalMentions: " + this.totalMentions + ", totalAnaphoricMentions: " + this.totalAnaphoricMentions + ", totalEdges: " + this.totalEdges +
        ", edgesPruned: " + this.edgesPruned + ", numGoldBackptrs: " + this.numGoldBackptrs +
        ", numGoldBackptrsPruned: " + this.numGoldBackptrsPruned + ", numAllBackptrsPruned: " +
        this.numAllBackptrsPruned + ", numAnaphoricAllBackptrsPruned: " + this.numAnaphoricAllBackptrsPruned;
  }
};

object DocumentGraph {
  
//  def pruneEdgesAll(docGraphs: Seq[DocumentGraph], pruningStrategy: PruningStrategy, scorer: PairwiseScorer) {
//    if (pruningStrategy.strategy.startsWith("distance")) {
//      val args = pruningStrategy.getDistanceArgs();
//      pruneEdgesAll(docGraphs, (doc: DocumentGraph) => doc.pruneEdgesMentDistanceSentDistance(args._1, args._2));
//    } else if (pruningStrategy.strategy.startsWith("models")) {
//      val models = GUtil.load(pruningStrategy.getModelPath).asInstanceOf[(HashMap[UID,Int], ArrayBuffer[PairwiseScorer])];
//      val threshold = pruningStrategy.getModelLogRatio;
//      pruneEdgesAll(docGraphs, (doc: DocumentGraph) => {
//        if (models._1.contains(doc.corefDoc.rawDoc.uid)) {
//          doc.pruneEdgesModel(models._2(models._1(doc.corefDoc.rawDoc.uid)), threshold);
//        } else {
//          doc.pruneEdgesModel(models._2.head, threshold);
//        }
//      });
//    } else {
//      throw new RuntimeException("Unrecognized pruning strategy: " + pruningStrategy);
//    }
//  }
//  
//  def pruneEdgesAll(docGraphs: Seq[DocumentGraph], docsToFolds: HashMap[UID,Int], foldModels: ArrayBuffer[PairwiseScorer], threshold: Float) {
//    pruneEdgesAll(docGraphs, (doc: DocumentGraph) => {
//      if (docsToFolds.contains(doc.corefDoc.rawDoc.uid)) {
//        doc.pruneEdgesModel(foldModels(docsToFolds(doc.corefDoc.rawDoc.uid)), threshold);
//      } else {
//        doc.pruneEdgesModel(foldModels.head, threshold);
//      }
//    });
//  }
//  
//  private def pruneEdgesAll(docGraphs: Seq[DocumentGraph], pruningFcn: (DocumentGraph) => Unit) {
//    var pruningStats = new PruningStats(0, 0, 0, 0, 0, 0, 0, 0);
//    for (docGraph <- docGraphs) {
//      pruningFcn(docGraph);
//      pruningStats = pruningStats.add(docGraph.computePruningStats());
//    }
//    Logger.logss("Pruning result: " + pruningStats);
//  }
}
