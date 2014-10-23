package edu.berkeley.nlp.entity.ner

import scala.collection.mutable.HashSet
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.futile.util.Logger

@SerialVersionUID(1L)
class CorpusCounts(val unigramCounts: Counter[String],
                   val bigramCounts: Counter[(String,String)],
                   val prefixCounts: Counter[String],
                   val suffixCounts: Counter[String],
                   val mostCommonUnigrams: HashSet[String]) extends Serializable { 
}

object CorpusCounts {
  
  def countUnigramsBigrams(rawSents: Seq[Seq[String]],
                           unigramThreshold: Int,
                           bigramThreshold: Int,
                           prefSuffThreshold: Int,
                           commonUnigramCount: Int = 100): CorpusCounts = {
    val unigramCounts = new Counter[String];
    val prefixCounts = new Counter[String];
    val suffixCounts = new Counter[String];
    def countWord(w: String) = {
      unigramCounts.incrementCount(w, 1.0);
      prefixCounts.incrementCount(NerFeaturizer.prefixFor(w), 1.0);
      suffixCounts.incrementCount(NerFeaturizer.prefixFor(w), 1.0);
    }
    val bigramCounts = new Counter[(String,String)];
    for (rawSent <- rawSents) {
      // Be sure to increment the start- and end-of-word labels
      countWord(NerExample.wordAt(rawSent, -1));
      for (i <- 0 to rawSent.size) {
        countWord(NerExample.wordAt(rawSent, i));
        bigramCounts.incrementCount(NerExample.wordAt(rawSent, i-1) -> NerExample.wordAt(rawSent, i), 1.0);
      }
    }
    unigramCounts.pruneKeysBelowThreshold(unigramThreshold);
    bigramCounts.pruneKeysBelowThreshold(bigramThreshold);
    prefixCounts.pruneKeysBelowThreshold(prefSuffThreshold);
    suffixCounts.pruneKeysBelowThreshold(prefSuffThreshold);
    val mostCommonUnigrams = new HashSet[String];
    val unigramPq = unigramCounts.asPriorityQueue();
    while (unigramPq.hasNext && mostCommonUnigrams.size < commonUnigramCount) {
      val unigram = unigramPq.next;
      mostCommonUnigrams += unigram;
    }
    Logger.logss(unigramCounts.size + " unigrams kept above " + unigramThreshold);
    Logger.logss(bigramCounts.size + " bigrams kept above " + bigramThreshold);
    Logger.logss(prefixCounts.size + " prefixes kept above " + prefSuffThreshold);
    Logger.logss(suffixCounts.size + " suffixes kept above " + prefSuffThreshold);
    new CorpusCounts(unigramCounts, bigramCounts, prefixCounts, suffixCounts, mostCommonUnigrams);
  }
}
