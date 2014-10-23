package edu.berkeley.nlp.entity.sem

import edu.berkeley.nlp.entity.coref.DocumentGraph
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.coref.Mention
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.WordNetInterfacer
import scala.collection.JavaConverters._

object FancyHeadMatcher {

  val VacuousSuffixesCore = Set("Ltd.", "Inc.", "Co.", "Jr.", "Sr.")
  val VacuousSuffixes = VacuousSuffixesCore ++ VacuousSuffixesCore.map(_.replaceAll("\\.", ""));
  
  
  
  // BASIC HEAD CONTAINMENT
  
  def isBasicHeadContainedBidirectional(docGraph: DocumentGraph, antIdx: Int, mentIdx: Int): Boolean = {
     isBasicHeadContained(docGraph, antIdx, mentIdx) || isBasicHeadContained(docGraph, mentIdx, antIdx);
  }
  
  def isBasicHeadContained(docGraph: DocumentGraph, antIdx: Int, mentIdx: Int): Boolean = {
     docGraph.getMention(antIdx).wordsLc.contains(docGraph.getMention(mentIdx).headStringLc);
  }
  
  // CLEAN CONTAINMENT
  // "Clean" head containment: non-nested, containment site is an immediate child of the head
  // (and not the head)
  
  def isCleanContained(container: Mention, containeeLc: String) = {
    val idx = getCleanContainmentSentenceIdx(container, containeeLc);
    idx != -1 && container.contextTree.isChild(idx, container.headIdx);
  }
  
  def getCleanContainmentSentenceIdx(container: Mention, containeeLc: String): Int = {
    val hcIdxRaw = container.wordsLc.indexOf(containeeLc);
    val hcIdx = hcIdxRaw + container.startIdx;
    val isContained = hcIdxRaw != -1;
    val isContainedAndNotHead = isContained && hcIdx != container.headIdx
    if (isContainedAndNotHead) hcIdx else -1;
  }
  
  def getCleanContainmentSentenceIdxStrict(container: Mention, containee: Mention): Int = {
    if (isCleanContained(container, containee.headStringLc) && !areNested(container, containee) &&
        getCleanContainmentSyntacticType(container, containee) != "GENITIVE" && !isCoordinatedHacky(container) && !isCoordinatedHacky(containee)) {
      getCleanContainmentSentenceIdx(container, containee.headStringLc);
    } else {
      -1;
    }
  }
  
  def getCleanContainmentSyntacticType(antecedent: Mention, curr: Mention): String = {
    val containmentSentenceIdx = getCleanContainmentSentenceIdx(antecedent, curr.headStringLc);
    getCleanContainmentSyntacticType(antecedent, containmentSentenceIdx);
  }
  
  def getCleanContainmentSyntacticType(mention: Mention, containmentSentenceIdx: Int): String = {
    val headIdx = mention.headIdx;
    if (containmentSentenceIdx == -1) {
      "NONE";
    } else if (containmentSentenceIdx < headIdx && (interposed(mention.wordsLc, containmentSentenceIdx - mention.startIdx, headIdx - mention.startIdx, "'s")
                                                 || interposed(mention.wordsLc, containmentSentenceIdx - mention.startIdx, headIdx - mention.startIdx, "'"))) {
      // Genitive: before, separated by a 's
      "GENITIVE";
    } else if (headIdx < containmentSentenceIdx && interposed(mention.pos, headIdx - mention.startIdx, containmentSentenceIdx - mention.startIdx, "CC")) {
      // Coordination: after, separated by a CC
      // (comes before appositive so that serial lists don't get mislabeled)
      "COORDINATION";
    } else if (headIdx < containmentSentenceIdx && interposed(mention.wordsLc, headIdx - mention.startIdx, containmentSentenceIdx - mention.startIdx, ",")) {
      // Appositive: after, separated by a comma
      "APPOSITIVE";
    } else {
      "MODIFIER";
    }
  }
  
  def getCleanContainmentSemanticType(antecedent: Mention, curr: Mention, wni: WordNetInterfacer): String = {
    val containmentSentenceIdx = getCleanContainmentSentenceIdx(antecedent, curr.headStringLc);
    getCleanContainmentSemanticType(antecedent, containmentSentenceIdx, wni: WordNetInterfacer);
  }
  
  def getCleanContainmentSemanticType(mention: Mention, containmentSentenceIdx: Int, wni: WordNetInterfacer): String = {
    SemClass.getSemClass(mention.wordsLc(containmentSentenceIdx - mention.startIdx), wni).toString;
  }
  
  private def interposed(items: Seq[String], startIdx: Int, endIdx: Int, item: String) = {
    var contains = false;
    for (i <- startIdx until endIdx) {
      if (items(i) == item) {
        contains = true;
      }
    }
    contains;
  }
  
  def areNested(antecedent: Mention, curr: Mention) = {
    antecedent.sentIdx == curr.sentIdx && ((antecedent.startIdx <= curr.headIdx && curr.headIdx < antecedent.endIdx) ||
                                           (curr.startIdx <= antecedent.headIdx && antecedent.headIdx < curr.endIdx));
  }
  
  def isBasicCleanHeadContainedBidirectional(antecedent: Mention, curr: Mention): Boolean = {
     isBasicCleanHeadContained(antecedent, curr) || isBasicCleanHeadContained(curr, antecedent);
  }
  
  def isBasicCleanHeadContained(antecedent: Mention, curr: Mention): Boolean = {
    val containmentSentenceIdx = getCleanContainmentSentenceIdx(antecedent, curr.headStringLc);
    !areNested(antecedent, curr) && containmentSentenceIdx != -1 && antecedent.contextTree.isChild(containmentSentenceIdx, antecedent.headIdx);
  }

  def isCoordinatedHacky(ment: Mention) = {
    val tree = ment.contextTree.constTree;
    var isCoordinated = false;
    val dominatingNodes = tree.getSpanMap.get(new edu.berkeley.nlp.futile.fig.basic.Pair[Integer,Integer](ment.startIdx, ment.endIdx));
    if (dominatingNodes != null && !dominatingNodes.isEmpty) {
      for (dominatingNode <- dominatingNodes.asScala) {
        if (dominatingNode.getChildren.asScala.map(_.getLabel).contains("CC")) {
          isCoordinated = true;
        }
      }
    }
    isCoordinated;
  }
  
  // FANCY HEADS
  
  def isFancyHeadContainedBidirectional(docGraph: DocumentGraph, antIdx: Int, mentIdx: Int): Boolean = {
    isFancyHeadContained(docGraph, antIdx, mentIdx) || isFancyHeadContained(docGraph, mentIdx, antIdx)
  }
  
  def isFancyHeadContained(docGraph: DocumentGraph, antIdx: Int, mentIdx: Int): Boolean = {
//    val antWordsLc = docGraph.getMention(antIdx).wordsLc;
    val antWordsLc = identifyHeadContentLc(docGraph.getMention(antIdx));
    val currMent = docGraph.getMention(mentIdx);
    val nerString = currMent.nerString;
    var headOffset = currMent.headIdx - currMent.startIdx;
    if (headOffset > 0 && VacuousSuffixes.contains(currMent.words(headOffset))) {
      headOffset -= 1;
    }
    val words = currMent.words;
    val wordsLc = currMent.wordsLc;
    // If it's a company, take the prefix
    val hc = if (nerString == "GPE" || nerString == "ORG") {
      // Find the first capitalized non-determiner
      var startIdx = 0;
      while (startIdx < headOffset && (Character.isLowerCase(words(startIdx).charAt(0)) ||
                                       words(startIdx).toLowerCase == "The" ||
                                       words(startIdx).toLowerCase == "A")) {
        startIdx += 1;
      }
      antWordsLc.contains(wordsLc(startIdx)) || antWordsLc.contains(wordsLc(headOffset));
    }
    // If it's a person, consider a match if either the first name or the last name matches
    else if (nerString == "PER") {
      var firstNameOrTitleOffset = if (headOffset == 0) headOffset else headOffset - 1;
      antWordsLc.contains(wordsLc(firstNameOrTitleOffset)) || antWordsLc.contains(wordsLc(headOffset));
    } else {
      // Back up one from suffixes and return what's there
      antWordsLc.contains(wordsLc(headOffset));
    }
    hc;
  }
  
  
  def identifyHeadContentLc(ment: Mention): Seq[String] = {
    // Take the span given by dependents of the head. If they're not contiguous, print them out.
    // Block with PRP$ or a genitive marker
    val contextTree = ment.contextTree;
    val deps = ment.contextTree.childParentDepMap;
    val headChildren = new ArrayBuffer[Int];
    for (i <- ment.startIdx until ment.endIdx) {
      if (deps(i) == ment.headIdx || i == ment.headIdx) {
        headChildren += i;
      }
    }
    if (headChildren.size == 0) {
      Seq(ment.headString);
    } else {
      val min = headChildren.reduce(Math.min);
      val max = headChildren.reduce(Math.max);
//      if (!headChildren.sameElements(min to max)) {
//        Logger.logss("Conflict: " + ment.wordsLc +
//                     "\n  head children: " + headChildren.map(i => ment.wordsLc(i - ment.startIdx)) +
//                     "\n  min and max: " + (min to max).map(i => ment.wordsLc(i - ment.startIdx)));
//      }
      // Block things past a genitive marker
      var blockIndex = -1;
      for (idx <- min to ment.headIdx) {
        if (ment.wordsLc(idx - ment.startIdx) == "'s") {
          blockIndex = idx;
        }
      }
      var filteredHeadChildren = headChildren.filter(childIdx => childIdx > blockIndex);
      filteredHeadChildren.map(i => ment.wordsLc(i - ment.startIdx));
    }
  }
}
