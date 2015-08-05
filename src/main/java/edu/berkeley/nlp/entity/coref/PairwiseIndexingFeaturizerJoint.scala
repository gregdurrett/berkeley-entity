package edu.berkeley.nlp.entity.coref
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.util.Logger
import scala.collection.mutable.HashMap
import scala.collection.JavaConverters._
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.entity.sem.QueryCountsBundle
import edu.berkeley.nlp.entity.ner.NerExample
import edu.berkeley.nlp.futile.syntax.Trees.PennTreeRenderer
import edu.berkeley.nlp.entity.wiki.WikipediaInterface
import edu.mit.jwi.item.Pointer
import edu.berkeley.nlp.entity.sem.FancyHeadMatcher
import edu.berkeley.nlp.entity.sem.AbbreviationHandler
import edu.berkeley.nlp.entity.sem.SemClass
import edu.berkeley.nlp.entity.sem.SemClasser
import edu.berkeley.nlp.entity.ner.NerFeaturizer
import edu.berkeley.nlp.entity.WordNetInterfacer

/**
 * DO NOT try to add WordNetInterfacer here! It is not serializable and so
 * everything will explode when we try to serialize the model. So we choose
 * to cache it on the documents even though this is pretty hacky.
 */
@SerialVersionUID(1L)
class PairwiseIndexingFeaturizerJoint(val featureIndexer: Indexer[String],
                                      val featureSet: FeatureSetSpecification,
                                      val lexicalCounts: LexicalCountsBundle,
                                      val queryCounts: Option[QueryCountsBundle],
                                      val semClasser: Option[SemClasser],
                                      val auxFeaturizers: Seq[AuxiliaryFeaturizer] = Seq[AuxiliaryFeaturizer]()) extends PairwiseIndexingFeaturizer with Serializable {
  import featureSet.featsToUse
  
  def getIndexer = featureIndexer;
  
//  private def getIndex(feature: String, addToFeaturizer: Boolean): Int = {
//    if (!addToFeaturizer) {
//      if (!featureIndexer.contains(feature)) {
//        val idx = featureIndexer.getIndex(PairwiseIndexingFeaturizerJoint.UnkFeatName);
//        require(idx == 0);
//        idx;
//      } else {
//        featureIndexer.getIndex(feature);
//      }
//    } else {
//      featureIndexer.getIndex(feature);
//    }
//  }
  
  private def maybeAddFeat(indexedFeats: ArrayBuffer[Int], feat: String, addToIndexer: Boolean) {
    if (addToIndexer) {
      indexedFeats += featureIndexer.getIndex(feat)
    } else {
      val idx = featureIndexer.indexOf(feat)
      if (idx != -1) indexedFeats += idx;
    }
  }
  
  def getQueryCountsBundle = queryCounts;
  
  def featurizeIndex(docGraph: DocumentGraph, currMentIdx: Int, antecedentIdx: Int, addToFeaturizer: Boolean): Array[Int] = {
    featurizeIndexStandard(docGraph, currMentIdx, antecedentIdx, addToFeaturizer);
  }
  
  def replaceIndexer(newIndexer: Indexer[String]) = {
    new PairwiseIndexingFeaturizerJoint(newIndexer, featureSet, lexicalCounts, queryCounts, semClasser);
  }
  
  private def addFeatureAndConjunctions(feats: ArrayBuffer[Int],
                                        featName: String,
                                        docGraph: DocumentGraph,
                                        currMent: Mention,
                                        antecedentMent: Mention,
                                        addToFeaturizer: Boolean) {
    if (featureSet.conjScheme == ConjScheme.BOTH) {
      val currConjunction = "&C=" + currMent.computeConjStr(featureSet.conjFeatures, Some(docGraph.cachedWni), semClasser);
      val prevConjunction = if (currMent != antecedentMent) {
        "&P=" + antecedentMent.computeConjStr(featureSet.conjFeatures, Some(docGraph.cachedWni), semClasser);
      } else {
        "";
      }
      maybeAddFeat(feats, featName + currConjunction + prevConjunction, addToFeaturizer);
//      feats += getIndex(featName + currConjunction + prevConjunction, addToFeaturizer);
    } else if (featureSet.conjScheme == ConjScheme.COARSE_CURRENT_BOTH) {
      // Note that this one behaves differently than the one above because BOTH is not always fired.
      // If it were always fired, it would often double with CURRENT.
      maybeAddFeat(feats, featName, addToFeaturizer);
//      feats += getIndex(featName, addToFeaturizer);
      val currConjunction = "&C=" + currMent.computeConjStr(featureSet.conjFeatures, Some(docGraph.cachedWni), semClasser);
      val featAndCurrConjunction = featName + currConjunction;
      maybeAddFeat(feats, featAndCurrConjunction, addToFeaturizer)
//      feats += getIndex(featAndCurrConjunction, addToFeaturizer);
      if (currMent != antecedentMent) {
        val prevConjunction = "&P=" + antecedentMent.computeConjStr(featureSet.conjFeatures, Some(docGraph.cachedWni), semClasser);
        maybeAddFeat(feats, featAndCurrConjunction + prevConjunction, addToFeaturizer);
//        feats += getIndex(featAndCurrConjunction + prevConjunction, addToFeaturizer);
      }
    } else { // All varieties of COARSE_BOTH
      // N.B. WHITELIST has the behavior that if no templates (type pairs) are specified,
      // then all templates (type pairs) are assumed to be whitelisted
      maybeAddFeat(feats, featName, addToFeaturizer);
//      feats += getIndex(featName, addToFeaturizer);
      val validPair = (featureSet.conjScheme == ConjScheme.COARSE_BOTH ||
                       (featureSet.conjScheme == ConjScheme.COARSE_BOTH_WHITELIST && (featureSet.conjListedTypePairs.isEmpty || featureSet.conjListedTypePairs.contains(antecedentMent.mentionType -> currMent.mentionType))) ||
                       (featureSet.conjScheme == ConjScheme.COARSE_BOTH_BLACKLIST && !featureSet.conjListedTypePairs.contains(antecedentMent.mentionType -> currMent.mentionType)));
      val validTemplate = (featureSet.conjScheme == ConjScheme.COARSE_BOTH ||
                          (featureSet.conjScheme == ConjScheme.COARSE_BOTH_WHITELIST && (featureSet.conjListedTemplates.isEmpty || featureSet.conjListedTemplates.contains(PairwiseIndexingFeaturizer.getTemplate(featName)))) ||
                          (featureSet.conjScheme == ConjScheme.COARSE_BOTH_BLACKLIST && !featureSet.conjListedTemplates.contains(PairwiseIndexingFeaturizer.getTemplate(featName))));
      if (validPair && validTemplate) {
        val currConjunction = "&C=" + currMent.computeConjStr(featureSet.conjFeatures, Some(docGraph.cachedWni), semClasser);
        val prevConjunction = if (currMent != antecedentMent) {
          "&P=" + antecedentMent.computeConjStr(featureSet.conjFeatures, Some(docGraph.cachedWni), semClasser);
        } else {
          "";
        }
        maybeAddFeat(feats, featName + currConjunction + prevConjunction, addToFeaturizer);
//        feats += getIndex(featName + currConjunction + prevConjunction, addToFeaturizer);
      } else { // Back off to the EMNLP conjunctions
        val currConjunction = "&C=" + currMent.computeConjStr(ConjFeatures.TYPE_OR_CANONICAL_PRON, Some(docGraph.cachedWni), semClasser);
        val prevConjunction = if (currMent != antecedentMent) {
          "&P=" + antecedentMent.computeConjStr(ConjFeatures.TYPE_OR_CANONICAL_PRON, Some(docGraph.cachedWni), semClasser);
        } else {
          "";
        }
        maybeAddFeat(feats, featName + currConjunction + prevConjunction, addToFeaturizer);
//        feats += getIndex(featName + currConjunction + prevConjunction, addToFeaturizer);
      }
    }
  }
  
  def featurizeIndexStandard(docGraph: DocumentGraph, currMentIdx: Int, antecedentIdx: Int, addToFeaturizer: Boolean): Array[Int] = {
    val currMent = docGraph.getMention(currMentIdx);
    val antecedentMent = docGraph.getMention(antecedentIdx);
//    val feats = new ArrayBuffer[Int]();
    val feats = new ArrayBuffer[Int]();
    def addFeatureShortcut = (featName: String) => {
      // Only used in CANONICAL_ONLY_PAIR, so only compute the truth value in this case
      addFeatureAndConjunctions(feats, featName, docGraph, currMent, antecedentMent, addToFeaturizer);
    }
    // FEATURES ON THE CURRENT MENTION (mostly targeting anaphoricity)
    val mentType = currMent.mentionType;
    val startingNew = antecedentIdx == currMentIdx;
    // When using very minimal feature sets, you might need to include this so every decision
    // has at least one feature over it.
    val snStr = "SN=" + startingNew;
    if (featsToUse.contains("bias")) {
      addFeatureShortcut(snStr);
    }
    // N.B. INCLUDED IN SURFACE
    if (!featsToUse.contains("nomentlen")) {
//      addFeatureShortcut("SNMentLen=" + currMent.spanToString.split("\\s+").size + snStr);
      addFeatureShortcut("SNMentLen=" + currMent.words.size + snStr);
    }
    // N.B. INCLUDED IN SURFACE
    if (!featsToUse.contains("nolexanaph") && !currMent.mentionType.isClosedClass) {
      addFeatureShortcut("SNMentHead=" + fetchHeadWordOrPos(currMent) + snStr);
    }
    // N.B. INCLUDED IN SURFACE
    if (!featsToUse.contains("nolexfirstword") && !currMent.mentionType.isClosedClass) {
      addFeatureShortcut("SNMentFirst=" + fetchFirstWordOrPos(currMent) + snStr);
    }
    // N.B. INCLUDED IN SURFACE
    if (!featsToUse.contains("nolexlastword") && !currMent.mentionType.isClosedClass) {
      addFeatureShortcut("SNMentLast=" + fetchLastWordOrPos(currMent) + snStr);
    }
    // N.B. INCLUDED IN SURFACE
    if (!featsToUse.contains("nolexprecedingword")) {
      addFeatureShortcut("SNMentPreceding=" + fetchPrecedingWordOrPos(currMent) + snStr);
    }
    // N.B. INCLUDED IN SURFACE
    if (!featsToUse.contains("nolexfollowingword")) {
      addFeatureShortcut("SNMentFollowing=" + fetchFollowingWordOrPos(currMent) + snStr);
    }
    // FEATURES ON THE ANTECEDENT
    if (!startingNew) {
      // N.B. INCLUDED IN SURFACE
      if (!featsToUse.contains("nomentlen")) {
//        addFeatureShortcut("PrevMentLen=" + antecedentMent.spanToString.split("\\s+").size);
        addFeatureShortcut("PrevMentLen=" + antecedentMent.words.size);
      }
      // N.B. INCLUDED IN SURFACE
      if (!featsToUse.contains("nolexanaph") && !antecedentMent.mentionType.isClosedClass) {
        addFeatureShortcut("PrevMentHead=" + fetchHeadWordOrPos(antecedentMent));
      }
      // N.B. INCLUDED IN SURFACE
      if (!featsToUse.contains("nolexfirstword") && !antecedentMent.mentionType.isClosedClass) {
        addFeatureShortcut("PrevMentFirst=" + fetchFirstWordOrPos(antecedentMent));
      }
      // N.B. INCLUDED IN SURFACE
      if (!featsToUse.contains("nolexlastword") && !antecedentMent.mentionType.isClosedClass) {
        addFeatureShortcut("PrevMentLast=" + fetchLastWordOrPos(antecedentMent));
      }
      // N.B. INCLUDED IN SURFACE
      if (!featsToUse.contains("nolexprecedingword")) {
        addFeatureShortcut("PrevMentPreceding=" + fetchPrecedingWordOrPos(antecedentMent));
      }
      // N.B. INCLUDED IN SURFACE
      if (!featsToUse.contains("nolexfollowingword")) {
        addFeatureShortcut("PrevMentFollowing=" + fetchFollowingWordOrPos(antecedentMent));
      }
    }
    
    // FEATURES ON ALL PAIRS
    if (!startingNew) {
      // Distance to antecedent
      // N.B. INCLUDED IN SURFACE
      if (!featsToUse.contains("nomentdist")) {
        addFeatureShortcut("Dist=" + Math.min(currMentIdx - antecedentIdx, 10));
      }
      // N.B. INCLUDED IN SURFACE
      if (!featsToUse.contains("nosentdist")) {
        addFeatureShortcut("SentDist=" + Math.min(currMent.sentIdx - antecedentMent.sentIdx, 10));
      }
      // N.B. INCLUDED IN FINAL
      if (featsToUse.contains("FINAL") || featsToUse.contains("iwi")) {
        addFeatureShortcut("iWi=" + currMent.iWi(antecedentMent));
      }
    }
    // Closed class (mostly pronoun) specific features
    if (mentType.isClosedClass) {
      // Pronominal features
      // N.B. INCLUDED IN FINAL
      if ((featsToUse.contains("FINAL") || featsToUse.contains("prongendnum")) && !startingNew) {
        addFeatureShortcut("AntGend=" + antecedentMent.gender);
        addFeatureShortcut("AntNumb=" + antecedentMent.number);
      }
      // N.B. INCLUDED IN FINAL
      if ((featsToUse.contains("FINAL") || featsToUse.contains("speaker")) && !startingNew) {
        if (antecedentMent.mentionType == MentionType.PRONOMINAL) {
          addFeatureShortcut("SameSpeaker=" + (if (docGraph.corefDoc.rawDoc.isConversation) "CONVERSATION" else "ARTICLE") +
                             "-" + (currMent.speaker == antecedentMent.speaker));
        }
      }
    }
    // Nominal and proper-specific features
    if (!mentType.isClosedClass) {
      if (!startingNew) {
        // Nominal and proper features
        // String match
        val exactStrMatch = (currMent.spanToString.toLowerCase.equals(antecedentMent.spanToString.toLowerCase));
        // N.B. INCLUDED IN SURFACE
        if (!featsToUse.contains("noexactmatch")) {
          addFeatureShortcut("ExactStrMatch=" + exactStrMatch);
        }
        // N.B. INCLUDED IN FINAL
        if (featsToUse.contains("FINAL") || featsToUse.contains("emcontained")) {
          addFeatureShortcut("ThisContained=" + (antecedentMent.spanToString.contains(currMent.spanToString)));
          addFeatureShortcut("AntContained=" + (currMent.spanToString.contains(antecedentMent.spanToString)));
        }
        // Head match
        val headMatch = currMent.headStringLc.equals(antecedentMent.headStringLc);
        // N.B. INCLUDED IN SURFACE
        if (!featsToUse.contains("noheadmatch")) {
          addFeatureShortcut("ExactHeadMatch=" + headMatch);
        }
        // N.B. INCLUDED IN FINAL
        if (featsToUse.contains("FINAL") || featsToUse.contains("hmcontained")) {
          addFeatureShortcut("ThisHeadContained=" + (antecedentMent.spanToString.contains(currMent.headString)));
          addFeatureShortcut("AntHeadContained=" + (currMent.spanToString.contains(antecedentMent.headString)));
        }
        // HEAD CONTAINED VARIANTS
        if (featsToUse.contains("morehmcontained")) {
          addFeatureShortcut("ThisLcHeadContained=" + (antecedentMent.spanToStringLc.contains(currMent.headStringLc)));
          addFeatureShortcut("AntLcHeadContained=" + (currMent.spanToStringLc.contains(antecedentMent.headStringLc)));
          addFeatureShortcut("ThisStrictHeadContained=" + (antecedentMent.words.contains(currMent.headString)));
          addFeatureShortcut("AntStrictHeadContained=" + (currMent.words.contains(antecedentMent.headString)));
          addFeatureShortcut("ThisStrictLcHeadContained=" + (antecedentMent.wordsLc.contains(currMent.headStringLc)));
          addFeatureShortcut("AntStrictLcHeadContained=" + (currMent.wordsLc.contains(antecedentMent.headStringLc)));
        }
        if (featsToUse.contains("bilexical")) {
          if (!antecedentMent.mentionType.isClosedClass) {
            addFeatureShortcut("Heads=" + fetchHeadWordOrPos(currMent) + "-" + fetchHeadWordOrPos(antecedentMent));
          }
        }
        if (featsToUse.contains("semcombo")) {
          val hmStatus = docGraph.getHeadMatchStatus(currMentIdx);
          val currHeadSanitized = WordNetInterfacer.sanitize(currMent.headStringLc);
          val antecedentHeadSanitized = WordNetInterfacer.sanitize(antecedentMent.headStringLc);
          val rel1 = docGraph.getWordNetRelsBetterCumulativeUseCache(docGraph.cachedWni, Seq(Pointer.HYPERNYM, Pointer.HYPERNYM, Pointer.HYPERNYM, Pointer.HYPERNYM, Pointer.HYPERNYM), antecedentIdx).contains(currHeadSanitized);
          val rel2 = docGraph.getWordNetRelsBetterCumulativeUseCache(docGraph.cachedWni, Seq(Pointer.HYPERNYM, Pointer.HYPERNYM, Pointer.HYPERNYM, Pointer.HYPERNYM, Pointer.HYPERNYM), currMentIdx).contains(antecedentHeadSanitized);
          val rel3 = docGraph.getWordNetRelsBetterCumulativeUseCache(docGraph.cachedWni, Seq(Pointer.HYPERNYM_INSTANCE, Pointer.HYPERNYM, Pointer.HYPERNYM), antecedentIdx).contains(currHeadSanitized);
          val rel4 = docGraph.getWordNetRelsBetterCumulativeUseCache(docGraph.cachedWni, Seq(Pointer.HYPERNYM_INSTANCE, Pointer.HYPERNYM, Pointer.HYPERNYM), currMentIdx).contains(antecedentHeadSanitized);
          val fhc = FancyHeadMatcher.isFancyHeadContained(docGraph, antecedentIdx, currMentIdx);
          val fhcBackwards = FancyHeadMatcher.isFancyHeadContained(docGraph, currMentIdx, antecedentIdx);
          val abbr = AbbreviationHandler.isAbbreviation(docGraph, antecedentIdx, currMentIdx);
          addFeatureShortcut("WordNetRel1=" + rel1 + "-HM=" + hmStatus);
          addFeatureShortcut("WordNetRel2=" + rel2 + "-HM=" + hmStatus);
          addFeatureShortcut("WordNetRel3=" + rel3 + "-HM=" + hmStatus);
          addFeatureShortcut("WordNetRel4=" + rel4 + "-HM=" + hmStatus);
          addFeatureShortcut("FancyHC=" + fhc + "-HM=" + hmStatus);
          addFeatureShortcut("FancyHCBackward=" + fhcBackwards + "-HM=" + hmStatus);
          addFeatureShortcut("Abbreviation=" + abbr + "-HM=" + hmStatus);
        }
        if (featsToUse.contains("semclass")) {
          val currSc = docGraph.getSemClassUseCache(docGraph.cachedWni, currMentIdx);
          val antecedentSc = docGraph.getSemClassUseCache(docGraph.cachedWni, antecedentIdx);
          addFeatureShortcut("SemClasses=" + currSc + "-" + antecedentSc);
        }
        if (featsToUse.contains("semclassandheads")) {
          val currSc = docGraph.getSemClassUseCache(docGraph.cachedWni, currMentIdx);
          val antecedentSc = docGraph.getSemClassUseCache(docGraph.cachedWni, antecedentIdx);
          addFeatureShortcut("AntHeadCurrSem=" + currSc + "-" + fetchHeadWordOrPos(antecedentMent));
          addFeatureShortcut("CurrHeadAntSem=" + fetchHeadWordOrPos(currMent) + "-" + antecedentSc);
        }
        if (featsToUse.contains("hearst")) {
          // Only fire on referring linkages that don't have head match
          if (antecedentMent.mentionType != MentionType.PRONOMINAL && currMent.headStringLc != antecedentMent.headStringLc) {
            // Non lower-cased
            val currHead = currMent.headString;
            val antHead = antecedentMent.headString;
            // Order of this call shouldn't matter because we symmetrized the counts
            val pairCount = queryCounts.get.pairCounts.getCount(antHead -> currHead);
            val present = pairCount > 0.5;
            addFeatureShortcut("HearstPresent=" + present);
            if (present) {
              val logBinnedCountUnnorm = (Math.log(pairCount)/Math.log(10) + 0.5).toInt;
              val logBinnedCountUnnormFine = (Math.log(pairCount)/Math.log(10) * 4 + 0.5).toInt;
              addFeatureShortcut("HearstUnnormBin=" + logBinnedCountUnnorm);
              addFeatureShortcut("HearstUnnormFineBin=" + logBinnedCountUnnormFine);
              var currHeadCount = queryCounts.get.wordCounts.getCount(currHead);
              var antHeadCount = queryCounts.get.wordCounts.getCount(antHead);
              // Watch out for divide-by-zero when doing the normalized counts...
              if (currHeadCount == 0 || antHeadCount == 0) {
                Logger.logss("WARNING: Inexplicably, count for " + currHead + " or " + antHead + " is less than the pair count: " +
                             currHeadCount + " " + antHeadCount + " " + pairCount);
                currHeadCount = Math.max(currHeadCount, pairCount);
                antHeadCount = Math.max(antHeadCount, pairCount);
              }
              val logBinnedCountNorm = (Math.log(pairCount/(currHeadCount * antHeadCount))/Math.log(10) + 0.5).toInt;
              val logBinnedCountNormFine = (Math.log(pairCount/(currHeadCount * antHeadCount))/Math.log(10) * 4 + 0.5).toInt;
              addFeatureShortcut("HearstNormBin=" + logBinnedCountNorm);
              addFeatureShortcut("HearstNormFineBin=" + logBinnedCountNormFine);
//              Logger.logss(currHead + " " + antHead + ": " + currHeadCount + " " + antHeadCount + " " + pairCount + " " + logBinnedCountUnnorm + " " + logBinnedCountNorm)
            }
          }
        }
      }
    }
    
    ////////////////////////
    // SEM CLASS FEATURES //
    ////////////////////////
    // Simple sem class
    if (featsToUse.contains("pairwisesemclass") && semClasser.isDefined) {
      val currDescriptors = currMent.computeSemanticDescriptors(featsToUse, docGraph.cachedWni, semClasser.get);
      for (currDescriptor <- currDescriptors) {
        addFeatureShortcut("SNSemClass=" + currDescriptor + "-" + startingNew);
      }
      if (!startingNew) {
        val antecedentDescriptors = antecedentMent.computeSemanticDescriptors(featsToUse, docGraph.cachedWni, semClasser.get);
        for (antDescriptor <- antecedentDescriptors) {
          for (currDescriptor <- currDescriptors) {
            addFeatureShortcut("SemClasses=" + antDescriptor + "-" + currDescriptor);
          }
        }
      }
    }
    if (featsToUse.contains("pairwiselexsemclass") && semClasser.isDefined) {
      val currDescriptors = currMent.computeSemanticDescriptors(featsToUse, docGraph.cachedWni, semClasser.get);
      if (!currMent.mentionType.isClosedClass()) {
        for (currDescriptor <- currDescriptors) {
          addFeatureShortcut("SNSemClass=" + currDescriptor + "-" + startingNew);
        }
      }
      if (!startingNew) {
        val antecedentDescriptors = antecedentMent.computeSemanticDescriptors(featsToUse, docGraph.cachedWni, semClasser.get);
        // Only use non-closed class because otherwise we already include this with
        // TYPE_OR_CANONICAL_PRON conjunctions
        if (!antecedentMent.mentionType.isClosedClass) {
          for (descriptor <- antecedentDescriptors) {
            if (!featsToUse.contains("noschead")) addFeatureShortcut("CurrHeadPrevSC=" + fetchHeadWordOrPos(currMent) + "-" + descriptor);
            if (!featsToUse.contains("noscfirst")) addFeatureShortcut("CurrFirstPrevSC=" + fetchFirstWordOrPos(currMent) + "-" + descriptor);
            if (!featsToUse.contains("nosclast")) addFeatureShortcut("CurrLastPrevSC=" + fetchLastWordOrPos(currMent) + "-" + descriptor);
            if (!featsToUse.contains("noscprec")) addFeatureShortcut("CurrPrecedingPrevSC=" + fetchPrecedingWordOrPos(currMent) + "-" + descriptor);
            if (!featsToUse.contains("noscfol")) addFeatureShortcut("CurrFollowingPrevSC=" + fetchFollowingWordOrPos(currMent) + "-" + descriptor);
          }
        }
        if (!currMent.mentionType.isClosedClass) {
          for (descriptor <- currDescriptors) {
            if (!featsToUse.contains("noschead")) addFeatureShortcut("PrevHeadCurrSC=" + fetchHeadWordOrPos(antecedentMent) + "-" + descriptor);
            if (!featsToUse.contains("noscfirst")) addFeatureShortcut("PrevFirstCurrSC=" + fetchFirstWordOrPos(antecedentMent) + "-" + descriptor);
            if (!featsToUse.contains("nosclast")) addFeatureShortcut("PrevLastCurrSC=" + fetchLastWordOrPos(antecedentMent) + "-" + descriptor);
            if (!featsToUse.contains("noscprec")) addFeatureShortcut("PrevPrecedingCurrSC=" + fetchPrecedingWordOrPos(antecedentMent) + "-" + descriptor);
            if (!featsToUse.contains("noscfol")) addFeatureShortcut("PrevFollowingCurrSC=" + fetchFollowingWordOrPos(antecedentMent) + "-" + descriptor);
          }
        }
      }
    }
    if (featsToUse.contains("pairwiselexconj") && semClasser.isDefined) {
      val conjTypeTheseFeats = if (featsToUse.contains("plccoarse")) {
        ConjFeatures.CUSTOM_NER_OR_CANONICAL_PRON
      } else if (featsToUse.contains("plcmed")) {
        ConjFeatures.CUSTOM_NERMED_OR_CANONICAL_PRON
      } else {
        ConjFeatures.CUSTOM_NERFINE_OR_CANONICAL_PRON
      }
      val descriptor = currMent.computeConjStr(conjTypeTheseFeats, Some(docGraph.cachedWni), semClasser);
      if (!currMent.mentionType.isClosedClass()) {
        addFeatureShortcut("SNSemClass=" + descriptor + "-" + startingNew);
      }
      if (!startingNew) {
        val antecedentDescriptor = antecedentMent.computeConjStr(conjTypeTheseFeats, Some(docGraph.cachedWni), semClasser);
        // Only use non-closed class because otherwise we already include this with
        // TYPE_OR_CANONICAL_PRON conjunctions
        if (!antecedentMent.mentionType.isClosedClass) {
          if (!featsToUse.contains("noschead")) addFeatureShortcut("CurrHeadPrevSC=" + fetchHeadWordOrPos(currMent) + "-" + antecedentDescriptor);
          if (!featsToUse.contains("noscfirst")) addFeatureShortcut("CurrFirstPrevSC=" + fetchFirstWordOrPos(currMent) + "-" + antecedentDescriptor);
          if (!featsToUse.contains("nosclast")) addFeatureShortcut("CurrLastPrevSC=" + fetchLastWordOrPos(currMent) + "-" + antecedentDescriptor);
          if (!featsToUse.contains("noscprec")) addFeatureShortcut("CurrPrecedingPrevSC=" + fetchPrecedingWordOrPos(currMent) + "-" + antecedentDescriptor);
          if (!featsToUse.contains("noscfol")) addFeatureShortcut("CurrFollowingPrevSC=" + fetchFollowingWordOrPos(currMent) + "-" + antecedentDescriptor);
        }
        if (!currMent.mentionType.isClosedClass) {
          if (!featsToUse.contains("noschead")) addFeatureShortcut("PrevHeadCurrSC=" + fetchHeadWordOrPos(antecedentMent) + "-" + descriptor);
          if (!featsToUse.contains("noscfirst")) addFeatureShortcut("PrevFirstCurrSC=" + fetchFirstWordOrPos(antecedentMent) + "-" + descriptor);
          if (!featsToUse.contains("nosclast")) addFeatureShortcut("PrevLastCurrSC=" + fetchLastWordOrPos(antecedentMent) + "-" + descriptor);
          if (!featsToUse.contains("noscprec")) addFeatureShortcut("PrevPrecedingCurrSC=" + fetchPrecedingWordOrPos(antecedentMent) + "-" + descriptor);
          if (!featsToUse.contains("noscfol")) addFeatureShortcut("PrevFollowingCurrSC=" + fetchFollowingWordOrPos(antecedentMent) + "-" + descriptor);
        }
      }
    }
    /////////////////////////////////////////
    // FEATURES FROM AUXILIARY FEATURIZERS //
    /////////////////////////////////////////
    // Null check is for backwards compatibility with previous serialized objects
    if (auxFeaturizers != null) {
      for (featurizer <- auxFeaturizers) {
        featurizer.featurize(docGraph, currMentIdx, antecedentIdx).foreach(addFeatureShortcut(_))
      }
    }
    ////////////////////////////////////////////////////////////////////////////////////////////////
    // ADD YOUR OWN FEATURES HERE!                                                                //
    //   See above for examples of how to do this. Typically use addFeatureShortcut since this    //
    // gives you your feature as well as conjunctions, but you can also directly call             //
    // feats += getIndex(feat, addToFeaturizer);                                                  //
    //                                                                                            //
    // To control feature sets, featsToUse is passed down from pairwiseFeats (the command line    //
    // argument). We currently use magic words all starting with +, but you do have to make       //
    // sure that you don't make a magic word that's a prefix of another, or else both will be     //
    // added when the longer one is.                                                              //
    //                                                                                            //
    // Happy feature engineering!                                                                 //
    ////////////////////////////////////////////////////////////////////////////////////////////////
    feats.toArray;
  }
  
  def fetchHeadWordOrPos(ment: Mention) = fetchWordOrPosDefault(ment.headStringLc, ment.pos(ment.headIdx - ment.startIdx), lexicalCounts.commonHeadWordCounts);
  def fetchFirstWordOrPos(ment: Mention) = fetchWordOrPosDefault(ment.words(0).toLowerCase, ment.pos(0), lexicalCounts.commonFirstWordCounts);
  
  def fetchLastWordOrPos(ment: Mention) = {
    if (ment.words.size == 1 || ment.endIdx - 1 == ment.headIdx) {
      ""
    } else {
      fetchWordOrPosDefault(ment.words(ment.words.size - 1).toLowerCase, ment.pos(ment.pos.size - 1), lexicalCounts.commonLastWordCounts);
    }
  }
  private def fetchPenultimateWordOrPos(ment: Mention) = {
    if (ment.words.size <= 2) {
      ""
    } else {
      fetchWordOrPosDefault(ment.words(ment.words.size - 2).toLowerCase, ment.pos(ment.pos.size - 2), lexicalCounts.commonPenultimateWordCounts);
    }
  }
  private def fetchSecondWordOrPos(ment: Mention) = {
    if (ment.words.size <= 3) {
      ""
    } else {
      fetchWordOrPosDefault(ment.words(1).toLowerCase, ment.pos(1), lexicalCounts.commonSecondWordCounts);
    }
  }
  
  def fetchPrecedingWordOrPos(ment: Mention) = fetchWordOrPosDefault(ment.contextWordOrPlaceholder(-1).toLowerCase, ment.contextPosOrPlaceholder(-1), lexicalCounts.commonPrecedingWordCounts);
  def fetchFollowingWordOrPos(ment: Mention) = fetchWordOrPosDefault(ment.contextWordOrPlaceholder(ment.words.size).toLowerCase, ment.contextPosOrPlaceholder(ment.words.size), lexicalCounts.commonFollowingWordCounts);
  private def fetchPrecedingBy2WordOrPos(ment: Mention) = fetchWordOrPosDefault(ment.contextWordOrPlaceholder(-2).toLowerCase, ment.contextPosOrPlaceholder(-2), lexicalCounts.commonPrecedingBy2WordCounts);
  private def fetchFollowingBy2WordOrPos(ment: Mention) = fetchWordOrPosDefault(ment.contextWordOrPlaceholder(ment.words.size + 1).toLowerCase, ment.contextPosOrPlaceholder(ment.words.size + 1), lexicalCounts.commonFollowingBy2WordCounts);
  private def fetchGovernorWordOrPos(ment: Mention) = fetchWordOrPosDefault(ment.governor.toLowerCase, ment.governorPos, lexicalCounts.commonGovernorWordCounts);
  
  
  private def fetchWordOrPosDefault(word: String, pos: String, counter: Counter[String]) = {
    if (counter.containsKey(word)) {
      word;
    } else if (featsToUse.contains("NOPOSBACKOFF")) {
      ""
    } else {
      pos;
    }
  }
  
  private def fetchPrefix(word: String) = {
    if (word.size >= 3 && lexicalCounts.commonPrefixCounts.containsKey(word.substring(0, 3))) {
      word.substring(0, 3);
    } else if (word.size >= 2 && lexicalCounts.commonPrefixCounts.containsKey(word.substring(0, 2))) {
      word.substring(0, 2);
    } else if (lexicalCounts.commonPrefixCounts.containsKey(word.substring(0, 1))) {
      word.substring(0, 1);
    } else {
      "";
    }
  }
  
  private def fetchSuffix(word: String) = {
    if (word.size >= 3 && lexicalCounts.commonSuffixCounts.containsKey(word.substring(word.size - 3))) {
      word.substring(word.size - 3);
    } else if (word.size >= 2 && lexicalCounts.commonSuffixCounts.containsKey(word.substring(word.size - 2))) {
      word.substring(word.size - 2);
    } else if (lexicalCounts.commonSuffixCounts.containsKey(word.substring(word.size - 1))) {
      word.substring(word.size - 1);
    } else {
      "";
    }
  }
  
  def fetchShape(word: String) = {
    if (lexicalCounts.commonShapeCounts.containsKey(NerFeaturizer.shapeFor(word))) {
      NerFeaturizer.shapeFor(word);
    } else {
      "";
    }
  }
  
  def fetchClass(word: String) = {
    if (lexicalCounts.commonClassCounts.containsKey(NerFeaturizer.classFor(word))) {
      NerFeaturizer.classFor(word);
    } else {
      "";
    }
  }
  
  private def fetchHeadWord(ment: Mention) = ment.words(ment.headIdx - ment.startIdx);
  private def fetchFirstWord(ment: Mention) = ment.words(0);
  private def fetchLastWord(ment: Mention) = ment.words(ment.pos.size - 1);
  private def fetchPrecedingWord(ment: Mention) = ment.contextWordOrPlaceholder(-1);
  private def fetchFollowingWord(ment: Mention) = ment.contextWordOrPlaceholder(ment.pos.size);
  
  private def fetchHeadPos(ment: Mention) = ment.pos(ment.headIdx - ment.startIdx);
//  private def fetchFirstPos(ment: Mention) = ment.pos(0);
//  private def fetchLastPos(ment: Mention) = ment.pos(ment.pos.size - 1);
//  private def fetchPrecedingPos(ment: Mention) = ment.contextPosOrPlaceholder(-1);
//  private def fetchFollowingPos(ment: Mention) = ment.contextPosOrPlaceholder(ment.pos.size);
  
  private def computeDefiniteness(ment: Mention) = {
    val firstWord = ment.words(0).toLowerCase;
    if (firstWord.equals("the")) {
      "DEF"
    } else if (firstWord.equals("a") || firstWord.equals("an")) {
      "INDEF"
    } else {
      "NONE"
    }
  }
  
  private def computePronNumber(ment: Mention) = {
    val firstWord = ment.words(0).toLowerCase;
    if (PronounDictionary.singularPronouns.contains(ment.headStringLc)) {
      "SING"
    } else if (PronounDictionary.pluralPronouns.contains(ment.headStringLc)) {
      "PLU"
    } else {
      "UNKNOWN"
    }
  }
  
  private def computePronGender(ment: Mention) = {
    val firstWord = ment.words(0).toLowerCase;
    if (PronounDictionary.malePronouns.contains(ment.headStringLc)) {
      "MALE"
    } else if (PronounDictionary.femalePronouns.contains(ment.headStringLc)) {
      "FEMALE"
    } else if (PronounDictionary.neutralPronouns.contains(ment.headStringLc)) {
      "NEUTRAL"
    } else {
      "UNKNOWN"
    }
  }
  
  private def computePronPerson(ment: Mention) = {
    val firstWord = ment.words(0).toLowerCase;
    if (PronounDictionary.firstPersonPronouns.contains(ment.headStringLc)) {
      "1st"
    } else if (PronounDictionary.secondPersonPronouns.contains(ment.headStringLc)) {
      "2nd"
    } else if (PronounDictionary.firstPersonPronouns.contains(ment.headStringLc)) {
      "3rd"
    } else {
      "OTHER"
    }
  }
  
  private def computeSentMentIdx(docGraph: DocumentGraph, ment: Mention) = {
    var currIdx = ment.mentIdx - 1;
    while (currIdx >= 0 && docGraph.getMention(currIdx).sentIdx == ment.sentIdx) {
      currIdx -= 1;
    }
    ment.mentIdx - currIdx + 1;
  }

  def computeTopicLabel(docGraph: DocumentGraph, clustererIdx: Int, mentIdx: Int): String = {
    val ment = docGraph.getMention(mentIdx);
    if (ment.mentionType == MentionType.PRONOMINAL && featsToUse.contains("noprons")) {
      "PRON"
    } else if ((ment.mentionType == MentionType.NOMINAL || ment.mentionType == MentionType.PROPER) && featsToUse.contains("nonomsprops")) {
      "NOMPROP"
    } else {
      docGraph.getBestCluster(clustererIdx, mentIdx) + ""
    }
  }
  
  def computeDistribLabel(docGraph: DocumentGraph, clustererIdx: Int, mentIdx: Int, valIdx: Int): Int = {
    docGraph.storedDistributedLabels(clustererIdx)(mentIdx)(valIdx);
  }
  
  def numDistribLabels(docGraph: DocumentGraph, clustererIdx: Int): Int = {
    docGraph.numClusters(clustererIdx);
  }
}
  
object PairwiseIndexingFeaturizerJoint {
  val UnkFeatName = "UNK_FEAT";
}
