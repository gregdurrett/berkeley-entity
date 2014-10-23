package edu.berkeley.nlp.entity.coref
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.util.Logger

case class Feature(context: String, event: String, value: Double, basic: Boolean) {
  val name = context + " >> " + event;
  val contextAndTemplate = context + ":" + (if (basic) "basic" else "conj");
};

class EntityFeaturizer(val featsToUse: String) {
  
  val AddPrevConjunctions = false;
  
  def indexFeatures(feats: Seq[Feature], featureIndexer: Indexer[String]): Seq[Int] = {
    feats.map(feat => featureIndexer.indexOf(feat.name));
  }
  
  // Assuming is that partialClusters contains at least up to and including
  // the mention *before* the current mention
  def featurize(docGraph: DocumentGraph, currMentIdx: Int, antecedentIdx: Int, mentsToClusters: Seq[Int], clustersToMents: ArrayBuffer[ArrayBuffer[Int]]): Seq[Feature] = {
    val currMent = docGraph.getMention(currMentIdx);
    val antecedentMent = docGraph.getMention(antecedentIdx);
    val feats: ArrayBuffer[Feature] = featurizeBasic(docGraph, currMentIdx, antecedentIdx, mentsToClusters, clustersToMents);
    val finalFeats = if (featsToUse.contains("+conjfine")) {
      EntityFeaturizer.addCanonicalConjunctions(feats, currMent, antecedentMent, AddPrevConjunctions);
      feats;
    } else if (featsToUse.contains("+conjcoarse")) {
      EntityFeaturizer.addCoarseConjunctions(feats, currMent, antecedentMent, AddPrevConjunctions);
      feats;
    } else {
      feats;
    }
    finalFeats;
  }
  
  private def featurizeBasic(docGraph: DocumentGraph, currMentIdx: Int, antecedentIdx: Int, mentsToClusters: Seq[Int], clustersToMents: ArrayBuffer[ArrayBuffer[Int]]): ArrayBuffer[Feature] = {
    val currMent = docGraph.getMention(currMentIdx);
    val antecedentMent = docGraph.getMention(antecedentIdx);
    val feats = new ArrayBuffer[Feature]();
    if (antecedentIdx != currMentIdx) {
      for (clusterIdx <- 0 until docGraph.numClusterers) {
        val currClusterValue = docGraph.getBestCluster(clusterIdx, currMentIdx);
        if (featsToUse.contains("+entityagree")) {
          var agreeCount = 0;
          var disagreeCount = 0;
          var totalCount = 0;
          for (clusterMember <- clustersToMents(mentsToClusters(antecedentIdx))) {
            if (docGraph.getBestCluster(clusterIdx, clusterMember) == currClusterValue) {
              agreeCount += 1;
            } else {
              disagreeCount += 1;
            }
            totalCount += 1;
          }
          if (featsToUse.contains("+entityagreecustom")) {
            feats += EntityFeaturizer.createBasicFeature("EntityAgreeC" + clusterIdx + "-" + currClusterValue + "=" + bucketCount(agreeCount, totalCount), "EntityAgree")
            feats += EntityFeaturizer.createBasicFeature("EntityDisagreeC" + clusterIdx + "-" + currClusterValue + "=" + bucketCount(disagreeCount, totalCount), "EntityDisagree")
          } else {
            feats += EntityFeaturizer.createBasicFeature("EntityAgreeC" + clusterIdx + "=" + bucketCount(agreeCount, totalCount), "EntityAgree")
            feats += EntityFeaturizer.createBasicFeature("EntityDisagreeC" + clusterIdx + "=" + bucketCount(disagreeCount, totalCount), "EntityDisagree")
          }
        }
        if (featsToUse.contains("+entityfine")) {
          for (potentialValue <- 0 until docGraph.numClusters(clusterIdx)) {
            var thisOneAgreeCount = 0;
            var thisOneDisagreeCount = 0;
            var thisOneTotalCount = 0;
            for (clusterMember <- clustersToMents(mentsToClusters(antecedentIdx))) {
              if (docGraph.getBestCluster(clusterIdx, clusterMember) == currClusterValue) {
                thisOneAgreeCount += 1;
              } else {
                thisOneDisagreeCount += 1;
              }
              thisOneTotalCount += 1;
            }
            feats += EntityFeaturizer.createBasicFeature("EntityAgreeC" + clusterIdx + ":" + currClusterValue + "-" + potentialValue + "=" + bucketCount(thisOneAgreeCount, thisOneTotalCount), "EntityAgree")
            feats += EntityFeaturizer.createBasicFeature("EntityDisagreeC" + clusterIdx + ":" + currClusterValue + "-" + potentialValue + "=" + bucketCount(thisOneDisagreeCount, thisOneTotalCount), "EntityDisagree")
          }
        }
      }
    }
    feats;
  }
  
  // Must return *all* possible features that featurizeBasic could return on the
  // given DocumentGraph. The easiest way to do this is to simply featurize it
  // but because the entities can change at inference time and we don't do exact
  // search, we actually have to explicitly write them out.
  def getAllPossibleFeatures(docGraph: DocumentGraph): Seq[Feature] = {
    val feats = new ArrayBuffer[Feature]();
    for (clusterIdx <- 0 until docGraph.numClusterers) {
      for (bucketVal <- getAllBucketTypes) {
        if (featsToUse.contains("+entityagree")) {
          if (featsToUse.contains("+entityagreecustom")) {
            for (currClusterValue <- 0 until docGraph.numClusters(clusterIdx)) {
              feats += EntityFeaturizer.createBasicFeature("EntityAgreeC" + clusterIdx + "-" + currClusterValue + "=" + bucketVal, "EntityAgree")
              feats += EntityFeaturizer.createBasicFeature("EntityDisagreeC" + clusterIdx + "-" + currClusterValue + "=" + bucketVal, "EntityDisagree")
            }
          } else {
            feats += EntityFeaturizer.createBasicFeature("EntityAgreeC" + clusterIdx + "=" + bucketVal, "EntityAgree")
            feats += EntityFeaturizer.createBasicFeature("EntityDisagreeC" + clusterIdx + "=" + bucketVal, "EntityDisagree")
          }
        } else if (featsToUse.contains("+entityfine")) {
          for (currClusterValue <- 0 until docGraph.numClusters(clusterIdx)) {
            for (potentialValue <- 0 until docGraph.numClusters(clusterIdx)) {
              feats += EntityFeaturizer.createBasicFeature("EntityAgreeC" + clusterIdx + ":" + currClusterValue + "-" + potentialValue + "=" + bucketVal, "EntityAgree")
              feats += EntityFeaturizer.createBasicFeature("EntityDisagreeC" + clusterIdx + ":" + currClusterValue + "-" + potentialValue + "=" + bucketVal, "EntityDisagree")
            }
          }
        }
      }
    }
    val allPossibleConjunctions = if (featsToUse.contains("+conjfine")) {
      EntityFeaturizer.computeAllPossibleCanonicalConjunctionStrs();
    } else if (featsToUse.contains("+conjcoarse")) {
      EntityFeaturizer.computeAllPossibleCoarseConjunctionStrs();
    } else {
      Set[String]();
    }
    // Add all possible conjunction strings
    for (currConjStr <- allPossibleConjunctions) {
      for (prevConjStr <- allPossibleConjunctions) {
        EntityFeaturizer.addConjunctions(feats, currConjStr, prevConjStr, AddPrevConjunctions);
      }
    }
    feats;
  }
  
  private def getAllBucketTypes() = Seq("0/1", "1/1", "0/2", "1/2", "2/2", "NONE", "AL1", "MANY", "ALL");
  
  private def bucketCount(count: Int, denom: Int) = {
    if (denom <= 2) {
      count + "/" + denom; 
    } else {
      if (count == 0) "NONE" else if (count <= denom/2) "AL1" else if (count < denom) "MANY" else "ALL";
    }
  }
}

object EntityFeaturizer {
  
  // Only used by DocumentInferencerRahman, is somewhat brittle since it'll break
  // if the conjunction scheme changes
  def computeAllPossibleCanonicalConjunctionStrs(): Set[String] = {
    MentionType.values.map(_.toString).toSet ++ PronounDictionary.canonicalizations.values.toSet
  }
  
  def computeAllPossibleCoarseConjunctionStrs(): Set[String] = {
    MentionType.values.map(_.toString).toSet;
  }
  
  def isBasicFeature(feat: Feature): Boolean = {
    feat.basic;
  }
  
  def createBasicFeature(context: String, templateName: String): Feature = {
    new Feature(context, "", 1.0, true);
  }
  
  def createNonbasicFeature(context: String, templateName: String): Feature = {
    new Feature(context, "", 1.0, false);
  }
  
  def addCoarseConjunctions(basicFeats: ArrayBuffer[Feature],
                               currMent: Mention,
                               antecedentMent: Mention,
                               addPrevConjunctions: Boolean) {
    addConjunctions(basicFeats, currMent.mentionType.toString, antecedentMent.mentionType.toString, currMent != antecedentMent && addPrevConjunctions);
  }
  
  def addCanonicalConjunctions(basicFeats: ArrayBuffer[Feature],
                               currMent: Mention,
                               antecedentMent: Mention,
                               addPrevConjunctions: Boolean) {
    addConjunctions(basicFeats, currMent.computeConjStr(ConjFeatures.TYPE_OR_CANONICAL_PRON, None, None), antecedentMent.computeConjStr(ConjFeatures.TYPE_OR_CANONICAL_PRON, None, None), currMent != antecedentMent && addPrevConjunctions);
  }
  
  def addConjunctions(basicFeats: ArrayBuffer[Feature],
                      currRawConjStr: String,
                      prevRawConjStr: String,
                      prevMentEqualsCurrent: Boolean) {
    val currConjunction = "&Curr=" + currRawConjStr;
    val prevConjunction = "&Prev=" + prevRawConjStr;
    val originalFeatsSize = basicFeats.size;
    for (i <- 0 until originalFeatsSize) {
      val feat = basicFeats(i);
      if (isBasicFeature(feat)) {
        basicFeats += createNonbasicFeature(feat.context + currConjunction, "")
        // Only add the conjunction with previous type if different
        if (prevMentEqualsCurrent) {
          basicFeats += createNonbasicFeature(feat.context + currConjunction + prevConjunction, "")
        }
      }
    }
  }
}
