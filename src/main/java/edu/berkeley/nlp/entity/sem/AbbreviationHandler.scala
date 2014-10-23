package edu.berkeley.nlp.entity.sem

import edu.berkeley.nlp.entity.coref.DocumentGraph

object AbbreviationHandler {

  def isAbbreviation(docGraph: DocumentGraph, antIdx: Int, mentIdx: Int): Boolean = {
    val antWords = docGraph.getMention(antIdx).words;
    val mentWords = docGraph.getMention(antIdx).words;
    isAbbreviation(docGraph.getMention(antIdx).headString, docGraph.getMention(mentIdx).words, docGraph.getMention(mentIdx).headIdx - docGraph.getMention(mentIdx).startIdx) ||
      isAbbreviation(docGraph.getMention(mentIdx).headString, docGraph.getMention(antIdx).words, docGraph.getMention(antIdx).headIdx - docGraph.getMention(antIdx).startIdx)
  }
  
  def testAndCanonicalizeAbbreviation(abbrev: String) = {
    if (abbrev.size >= 2 && abbrev.size <= 4) {
      val abbrevFiltered = abbrev.filter(c => c != '.');
      if (abbrevFiltered.size > 0 && abbrevFiltered.map(c => Character.isUpperCase(c)).reduce(_ && _)) {
        abbrevFiltered;
      } else {
        "";
      }
    } else {
      "";
    }
  }
  
  def isAbbreviation(abbrev: String, other: Seq[String], headOffset: Int): Boolean = {
    val abbrevCanonical = testAndCanonicalizeAbbreviation(abbrev);
    if (abbrevCanonical == "") {
      false
    } else {
      val isAbbrev1 = isAbbreviationType1(abbrevCanonical, other, headOffset);
      val isAbbrev2 = isAbbreviationType2(abbrevCanonical, other, headOffset);
      val isAbbrev = isAbbrev1 || isAbbrev2;
      isAbbrev;
    }
  }
  
  def isAbbreviationType1(abbrev: String, other: Seq[String], headOffset: Int): Boolean = {
    // Abbreviation type 1: consecutive characters of words (African National Congress => ANC) containing the head.
    // TODO: Maybe don't uppercase (helps sometimes, hurts sometimes)
    val firstPossible = Math.max(0, headOffset - abbrev.size + 1);
    val lastPossible = Math.min(other.size, headOffset + abbrev.size);
    var isAbbrev = false;
    for (i <- firstPossible to lastPossible) {
      isAbbrev = isAbbrev || abbrev == other.slice(i, i + abbrev.size).map(word => Character.toUpperCase(word.charAt(0))).foldLeft("")(_ + _);
    }
    isAbbrev;
  }
  
  def isAbbreviationType2(abbrev: String, other: Seq[String], headOffset: Int): Boolean = {
    var otherFirstLettersNoLc = "";
    var headIdxInFinal = -1;
    for (i <- 0 until other.size) {
      if (Character.isUpperCase(other(i).charAt(0))) {
        otherFirstLettersNoLc += other(i).charAt(0);
      }
      if (i == headOffset) {
        headIdxInFinal = otherFirstLettersNoLc.size - 1;
      }
    }
    val firstPossible = Math.max(0, headIdxInFinal - abbrev.size + 1);
    val lastPossible = Math.min(other.size, headIdxInFinal + abbrev.size);
    otherFirstLettersNoLc.slice(firstPossible, lastPossible).contains(abbrev);
  }
  
  def main(args: Array[String]) {
    println(isAbbreviation("ANC", Seq("African", "National", "Congress"), 2));
  }
}
