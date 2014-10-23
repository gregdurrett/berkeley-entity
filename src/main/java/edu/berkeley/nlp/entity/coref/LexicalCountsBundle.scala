package edu.berkeley.nlp.entity.coref
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.entity.Driver;
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.futile.util.Logger
import scala.collection.JavaConverters._
import scala.collection.mutable.HashSet
import edu.berkeley.nlp.entity.ner.NerExample
import edu.berkeley.nlp.entity.ner.NerFeaturizer

@SerialVersionUID(1L)
class LexicalCountsBundle(val commonHeadWordCounts: Counter[String],
                          val commonFirstWordCounts: Counter[String],
                          val commonLastWordCounts: Counter[String],
                          val commonPenultimateWordCounts: Counter[String],
                          val commonSecondWordCounts: Counter[String],
                          val commonPrecedingWordCounts: Counter[String],
                          val commonFollowingWordCounts: Counter[String],
                          val commonPrecedingBy2WordCounts: Counter[String],
                          val commonFollowingBy2WordCounts: Counter[String],
                          val commonGovernorWordCounts: Counter[String],
                          val commonPrefixCounts: Counter[String],
                          val commonSuffixCounts: Counter[String],
                          val commonShapeCounts: Counter[String],
                          val commonClassCounts: Counter[String]) extends Serializable {
}

object LexicalCountsBundle {
  
  // Used for definiteness ablations
//  val dets1 = Set("the", "a", "an")
//  val dets2 = Set("some", "all", "more", "no", "one", "two", "three", "any", "other", "many", "such", "both")
//  val dems = Set("this", "that", "these", "those");
//  val poss = Set("his", "their", "its", "our", "her", "my", "your");
//  val mods = Set("new", "last", "former", "public", "political", "vice");
//  val nats = Set ("china", "u.s.", "foreign", "taiwan", "israeli", "national", "american", "palestinian", "chinese", "federal", "japan");
//  val roles = Set("mr.", "reporter", "president");

  private def countAndPrune(words: Seq[String], cutoff: Int): Counter[String] = {
    val counts = new Counter[String];
    words.foreach(counts.incrementCount(_, 1.0));
    counts.pruneKeysBelowThreshold(cutoff);
    counts;
  }
  
  def countLexicalItems(trainDocs: Seq[CorefDoc], cutoff: Int) = {
    val allMents = trainDocs.flatMap(_.predMentions);
    val allNonPronominalMents = trainDocs.flatMap(doc => doc.predMentions.filter(!_.mentionType.isClosedClass));
    // HEAD WORDS
    val allHeadWordsInTrain = allNonPronominalMents.map(_.headStringLc);
    val headWordCounts = countAndPrune(allHeadWordsInTrain, cutoff);
    Logger.logss(headWordCounts.size + " head words: top 100 = " + GUtil.getTopNKeysSubCounter(headWordCounts, 100).toString);
    // FIRST WORDS
    val allFirstWordsInTrain = allNonPronominalMents.flatMap(ment => {
      val words = ment.words;
      if (words.size > 1) Seq[String](words(0).toLowerCase()) else Seq[String]();
    });
    val firstWordCounts = countAndPrune(allFirstWordsInTrain, cutoff);
    Logger.logss(firstWordCounts.size + " first words: top 100 = " + GUtil.getTopNKeysSubCounter(firstWordCounts, 100).toString);
    // LAST WORDS
    val allLastWordsInTrain = allNonPronominalMents.flatMap(ment => {
      val words = ment.words;
      if (words.size > 1 && ment.endIdx - 1 != ment.headIdx) Seq[String](words(words.size - 1).toLowerCase()) else Seq[String]();
    });
    val lastWordCounts = countAndPrune(allLastWordsInTrain, cutoff);
    Logger.logss(lastWordCounts.size + " last words: top 100 = " + GUtil.getTopNKeysSubCounter(lastWordCounts, 100).toString);
    // PENULTIMATE WORDS
    val allPenultimateWordsInTrain = allNonPronominalMents.flatMap(ment => {
      val words = ment.words;
      if (words.size > 2) Seq[String](words(words.size - 2).toLowerCase()) else Seq[String]();
    });
    val penultimateWordCounts = countAndPrune(allPenultimateWordsInTrain, cutoff);
    Logger.logss(penultimateWordCounts.size + " penultimate words: top 100 = " + GUtil.getTopNKeysSubCounter(penultimateWordCounts, 100).toString);
    // SECOND WORDS
    val allSecondWordsInTrain = allNonPronominalMents.flatMap(ment => {
      val words = ment.words;
      if (words.size > 3) Seq[String](words(1).toLowerCase()) else Seq[String]();
    });
    val secondWordCounts = countAndPrune(allSecondWordsInTrain, cutoff);
    Logger.logss(secondWordCounts.size + " second words: top 100 = " + GUtil.getTopNKeysSubCounter(secondWordCounts, 100).toString);
    // PRECEDING WORDS
    val allPrecedingWordsInTrain = allMents.map(_.contextWordOrPlaceholder(-1).toLowerCase);
    val precedingWordCounts = countAndPrune(allPrecedingWordsInTrain, cutoff);
    Logger.logss(precedingWordCounts.size + " preceding words: top 100 = " + GUtil.getTopNKeysSubCounter(precedingWordCounts, 100).toString);
    // FOLLOWING WORDS
    val allFollowingWordsInTrain = allMents.map(ment => ment.contextWordOrPlaceholder(ment.words.size).toLowerCase);
    val followingWordCounts = countAndPrune(allFollowingWordsInTrain, cutoff);
    Logger.logss(followingWordCounts.size + " following words: top 100 = " + GUtil.getTopNKeysSubCounter(followingWordCounts, 100).toString);
    // PRECEDING BY 2 WORDS
    val allPrecedingBy2WordsInTrain = allMents.map(_.contextWordOrPlaceholder(-2).toLowerCase);
    val precedingBy2WordCounts = countAndPrune(allPrecedingBy2WordsInTrain, cutoff);
    Logger.logss(precedingBy2WordCounts.size + " preceding by 2 words: top 100 = " + GUtil.getTopNKeysSubCounter(precedingBy2WordCounts, 100).toString);
    // FOLLOWING BY 2 WORDS
    val allFollowingBy2WordsInTrain = allMents.map(ment => ment.contextWordOrPlaceholder(ment.words.size + 1).toLowerCase);
    val followingBy2WordCounts = countAndPrune(allFollowingBy2WordsInTrain, cutoff);
    Logger.logss(followingBy2WordCounts.size + " following by 2 words: top 100 = " + GUtil.getTopNKeysSubCounter(followingBy2WordCounts, 100).toString);
    // GOVERNOR WORDS
    val allGovernorWordsInTrain = allMents.map(ment => ment.governor.toLowerCase);
    val governorWordCounts = countAndPrune(allGovernorWordsInTrain, cutoff);
    Logger.logss(governorWordCounts.size + " governors: top 100 = " + GUtil.getTopNKeysSubCounter(governorWordCounts, 100).toString);
    
    // PREFIXES AND SUFFIXES
    val allPrefixCounts = new Counter[String]();
    val allSuffixCounts = new Counter[String]();
    val allShapeCounts = new Counter[String]();
    val allClassCounts = new Counter[String]();
    for (trainDoc <- trainDocs; sentence <- trainDoc.rawDoc.words; word <- sentence) {
      allShapeCounts.incrementCount(NerFeaturizer.shapeFor(word), 1.0);
      allClassCounts.incrementCount(NerFeaturizer.classFor(word), 1.0);
      if (word.size >= 1) {
        allPrefixCounts.incrementCount(word.substring(0, 1), 1.0);
        allSuffixCounts.incrementCount(word.substring(word.size - 1), 1.0);
      }
      if (word.size >= 2) {
        allPrefixCounts.incrementCount(word.substring(0, 2), 1.0);
        allSuffixCounts.incrementCount(word.substring(word.size - 2), 1.0);
      }
      if (word.size >= 3) {
        allPrefixCounts.incrementCount(word.substring(0, 3), 1.0);
        allSuffixCounts.incrementCount(word.substring(word.size - 3), 1.0);
      }
    }
    allPrefixCounts.pruneKeysBelowThreshold(cutoff);
    allSuffixCounts.pruneKeysBelowThreshold(cutoff);
    allShapeCounts.pruneKeysBelowThreshold(cutoff);
    allClassCounts.pruneKeysBelowThreshold(cutoff);
    Logger.logss(allPrefixCounts.size + " prefixes: top 100 = " + GUtil.getTopNKeysSubCounter(allPrefixCounts, 100).toString);
    Logger.logss(allSuffixCounts.size + " suffixes: top 100 = " + GUtil.getTopNKeysSubCounter(allSuffixCounts, 100).toString);
    Logger.logss(allShapeCounts.size + " shapes: top 100 = " + GUtil.getTopNKeysSubCounter(allShapeCounts, 100).toString);
    Logger.logss(allClassCounts.size + " classes: top 100 = " + GUtil.getTopNKeysSubCounter(allClassCounts, 100).toString);
    new LexicalCountsBundle(headWordCounts, firstWordCounts, lastWordCounts, penultimateWordCounts, secondWordCounts, precedingWordCounts, followingWordCounts, precedingBy2WordCounts, followingBy2WordCounts, governorWordCounts, allPrefixCounts, allSuffixCounts, allShapeCounts, allClassCounts);
  }
}
