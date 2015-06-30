package edu.berkeley.nlp.entity.sem

import edu.berkeley.nlp.entity.coref.DocumentGraph
import edu.berkeley.nlp.entity.coref.MentionType

object MentionFilter {
  
  def isCanonicalProperNominalCase(docGraph: DocumentGraph, idx: Int) = {
    val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(idx);
    goldAntecedents(0) != idx && !hasHeadMatchWithAntecedent(docGraph, idx) && ((isNominal(docGraph, idx) && hasProperAntecedents(docGraph, idx)) ||
                                                   (isProper(docGraph, idx) && hasReferringAntecedents(docGraph, idx)));
  }
  
  def isStricterProperNominalCase(docGraph: DocumentGraph, idx: Int) = {
    val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(idx);
    goldAntecedents(0) != idx && !hasHeadMatchWithAntecedent(docGraph, idx) && isNominal(docGraph, idx) && hasProperAntecedents(docGraph, idx);
  }
  
  def isStricterProperNominalCaseNoHc(docGraph: DocumentGraph, idx: Int) = {
    val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(idx);
    goldAntecedents(0) != idx && !hasHeadMatchWithAntecedent(docGraph, idx) && !hasLcHeadContainedWithAntecedent(docGraph, idx) &&
        isNominal(docGraph, idx) && hasProperAntecedents(docGraph, idx);
  }
  
  def unaryIsProperNominalNoContainmentCase(docGraph: DocumentGraph, idx: Int) = {
    val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(idx);
    goldAntecedents(0) != idx && !hasHeadMatchWithAntecedent(docGraph, idx) && !hasLcHeadContainedWithAntecedent(docGraph, idx) &&
        ((isNominal(docGraph, idx) && hasProperAntecedents(docGraph, idx)) || (isProper(docGraph, idx) && hasReferringAntecedents(docGraph, idx)));
  }
  
  def isCanonicalHardReferringCase(docGraph: DocumentGraph, idx: Int) = {
    val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(idx);
    goldAntecedents(0) != idx && !hasHeadMatchWithAntecedent(docGraph, idx) && isReferring(docGraph, idx) && hasReferringAntecedents(docGraph, idx);
  }
  
  def unaryIsCanonicalHardestReferringCase(docGraph: DocumentGraph, idx: Int) = {
    val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(idx);
    goldAntecedents(0) != idx && !hasHeadMatchWithAntecedent(docGraph, idx) && !hasLcHeadContainedWithAntecedent(docGraph, idx) &&
        isReferring(docGraph, idx) && hasReferringAntecedents(docGraph, idx);
  }
  
  def unaryIsCanonicalHardestReferringCaseNoPruning(docGraph: DocumentGraph, idx: Int) = {
    val goldAntecedents = docGraph.getGoldAntecedentsNoPruning(idx);
    goldAntecedents(0) != idx && !hasHeadMatchWithAntecedent(docGraph, idx) && !hasLcHeadContainedWithAntecedent(docGraph, idx) &&
        isReferring(docGraph, idx) && hasReferringAntecedents(docGraph, idx);
  }
  
  def isCanonicalHeadContainedCase(docGraph: DocumentGraph, idx: Int) = {
    val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(idx);
    goldAntecedents(0) != idx && !hasHeadMatchWithAntecedent(docGraph, idx) && hasLcHeadContainedWithAntecedent(docGraph, idx) && isReferring(docGraph, idx);
  }
  
  def isCanonicalStrictHeadContainedCase(docGraph: DocumentGraph, idx: Int) = {
    val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(idx);
    !hasHeadMatchWithAntecedent(docGraph, idx) && hasLcStrictHeadContainedWithAntecedent(docGraph, idx) && isReferring(docGraph, idx);
  }
  
  def unaryIsNoHarderThanHeadMatch(docGraph: DocumentGraph, idx: Int) = {
    val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(idx);
    isReferring(docGraph, idx) && (goldAntecedents(0) == idx || hasHeadMatchWithAntecedent(docGraph, idx));
  }
  
  def unaryIsHardReferringAndSomeSatisfiesSignal(docGraph: DocumentGraph, idx: Int, signal: (DocumentGraph, Int, Int) => Boolean) = {
    val someSatisfies = (0 until idx).map(signal(docGraph, _, idx)).foldLeft(false)(_ || _);
    isCanonicalHardReferringCase(docGraph, idx) && someSatisfies;
  }
  
  def isReferringSomeHeadMatchCase(docGraph: DocumentGraph, idx: Int) = {
    isReferring(docGraph, idx) && hasHeadMatchWithAntecedent(docGraph, idx);
  }
  
  def binaryIsReferringHeadMatchCase(docGraph: DocumentGraph, idx: Int, antIdx: Int) = {
    isReferring(docGraph, idx) && isReferring(docGraph, antIdx) && isLcHeadMatch(docGraph, idx, antIdx);
  }
  
  def binaryIsReferringHeadContainedCase(docGraph: DocumentGraph, idx: Int, antIdx: Int) = {
    isReferring(docGraph, idx) && isReferring(docGraph, antIdx) && !isLcHeadMatch(docGraph, idx, antIdx) && isLcStrictHeadContained(docGraph, idx, antIdx);
  }
  
  def binaryIsHardestReferringCase(docGraph: DocumentGraph, idx: Int, antIdx: Int) = {
    isReferring(docGraph, idx) && isReferring(docGraph, antIdx) && !isLcHeadMatch(docGraph, idx, antIdx) && !isLcStrictHeadContained(docGraph, idx, antIdx);
  }
  
  def isProper(docGraph: DocumentGraph, idx: Int) = {
    docGraph.getMention(idx).mentionType == MentionType.PROPER;
  }
  
  def isNominal(docGraph: DocumentGraph, idx: Int) = {
    docGraph.getMention(idx).mentionType == MentionType.NOMINAL;
  }
  
  def isPronominal(docGraph: DocumentGraph, idx: Int) = {
    docGraph.getMention(idx).mentionType == MentionType.PRONOMINAL;
  }
  
  def isDemonstrative(docGraph: DocumentGraph, idx: Int) = {
    docGraph.getMention(idx).mentionType == MentionType.DEMONSTRATIVE;
  }
  
  // N.B. Referring here means nominal or proper, not coreferent
  def isReferring(docGraph: DocumentGraph, idx: Int) = {
    !docGraph.getMention(idx).mentionType.isClosedClass();
  }
  
  def isIncorrect(docGraph: DocumentGraph, idx: Int, backptr: Int) = {
    !docGraph.getGoldAntecedentsUnderCurrentPruning(idx).contains(backptr);
  }
  
  def isPredictedNewCluster(docGraph: DocumentGraph, idx: Int, backptr: Int) = {
    backptr == idx;
  }
  
  def hasProperAntecedents(docGraph: DocumentGraph, idx: Int) = {
    val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(idx);
    goldAntecedents.filter(i => docGraph.getMention(i).mentionType == MentionType.PROPER).size > 0;
  }
  
  def hasNominalAntecedents(docGraph: DocumentGraph, idx: Int) = {
    val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(idx);
    goldAntecedents.filter(i => docGraph.getMention(i).mentionType == MentionType.NOMINAL).size > 0;
  }
  
  def hasReferringAntecedents(docGraph: DocumentGraph, idx: Int) = {
    val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(idx);
    goldAntecedents.filter(i => !docGraph.getMention(i).mentionType.isClosedClass).size > 0;
  }
  
  def hasHeadMatchWithAntecedent(docGraph: DocumentGraph, idx: Int) = {
    val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(idx).filter(i => i != idx);
    goldAntecedents.filter(i => docGraph.getMention(i).headStringLc == docGraph.getMention(idx).headStringLc).size > 0;
  }
  
  def hasLcHeadContainedWithAntecedent(docGraph: DocumentGraph, idx: Int) = {
    val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(idx).filter(i => i != idx);
    goldAntecedents.filter(i => docGraph.getMention(i).spanToStringLc.contains(docGraph.getMention(idx).headStringLc) ||
                                docGraph.getMention(idx).spanToStringLc.contains(docGraph.getMention(i).headStringLc)).size > 0;
  }
  
  def hasLcStrictHeadContainedWithAntecedent(docGraph: DocumentGraph, idx: Int) = {
    val goldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(idx).filter(i => i != idx);
    goldAntecedents.filter(i => docGraph.getMention(i).words.map(_.toLowerCase).contains(docGraph.getMention(idx).headStringLc) ||
                                docGraph.getMention(idx).words.map(_.toLowerCase).contains(docGraph.getMention(i).headStringLc)).size > 0;
  }
  
  def isLcHeadMatch(docGraph: DocumentGraph, idx: Int, antecedent: Int) = {
    docGraph.getMention(antecedent).headStringLc == docGraph.getMention(idx).headStringLc;
  }
  
  def isLcStrictHeadContained(docGraph: DocumentGraph, idx: Int, antecedent: Int) = {
    docGraph.getMention(antecedent).wordsLc.contains(docGraph.getMention(idx).headStringLc);
  }
  
  def hasHeadMatchWithPrediction(docGraph: DocumentGraph, idx: Int, backptr: Int) = {
    backptr != idx && docGraph.getMention(idx).headStringLc == docGraph.getMention(backptr).headStringLc;
  }
}
