package edu.berkeley.nlp.entity.wiki

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.coref.Mention
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.util.Counter

/**
 * Simple data structure to store information about a query to the Wikipedia
 * title given surface database formed from a particular mention.
 * 
 * @author gdurrett
 */
case class Query(val words: Seq[String],
                 val originalMent: Mention,
                 val finalSpan: (Int, Int),
                 val queryType: String,
                 val removePuncFromQuery: Boolean = true,
                 val features: List[String] = List[String]()) {
  
  def getFinalQueryStr = {
    val wordsNoPunc = if (removePuncFromQuery) {
      words.map(str => str.filter(c => !Query.PuncList.contains(c))).filter(!_.isEmpty);
    } else {
      words;
    }
    if (wordsNoPunc.isEmpty) "" else wordsNoPunc.reduce(_ + " " + _);
  }
}

object Query {
  
  def makeNilQuery(ment: Mention) = {
    new Query(Seq[String]("XXNILXX"), ment, (ment.headIdx + 1, ment.headIdx + 1), "NIL");
  }
  
  // These parameter settings have been tuned to give best performance on query extraction
  // for ACE, so are probably good there but might need to be revisited in other settings.
  val CapitalizationQueryExpand = false;
  val PluralQueryExpand = true;
  val RemovePuncFromQuery = true;
  val UseFirstHead = true;
  val MaxQueryLen = 8;
  val BlackList = Set(
    "the", "a", "my", "your", "his", "her", "our",
    "their", "its", "this", "that", "these", "those",
    "of"
  )
  val PuncList = Set(
    ',', '.', '!', '?', ':', ';', '\'', '"', '(', ')',
    '[', ']', '{', '}', ' '
  )
  
  /**
   * Check if a token is "blacklisted", meaning that we shouldn't form a query that starts with
   * it (such queries tend to do weird and bad things
   */
  def isBlacklisted(word: String, mentStartIdx: Int) = {
    BlackList.contains(word) || (mentStartIdx == 0 && BlackList.contains(word.toLowerCase));
  }
  
  /**
   * Very crappy stemmer
   */
  def removePlural(word: String) = {
    if (word.endsWith("sses")) {
      word.dropRight(2);
    } else if (word.endsWith("ies")) {
      // Not quite right...
      word.substring(0, word.size - 3) + "y";
    } else if (word.endsWith("s")) {
      word.dropRight(1);
    } else {
      word;
    }
  }
  
  /**
   * Given a mention, extracts the set of possible queries that we'll consider. This is done by
   * considering different subsets of the words in the mention and munging capitalization and
   * stemming, since lowercasing and dropping a plural-marking "s" are useful for nominals.
   */
  def extractQueriesBest_old(ment: Mention, addNilQuery: Boolean = false): Seq[Query] = {
    val queries = new ArrayBuffer[Query];
    val mentWords = ment.words;
    // Try the whole query, then prefixes ending in the head
    val relHeadIdx = (if (UseFirstHead) ment.contextTree.getSpanHeadACECustom(ment.startIdx, ment.endIdx) else ment.headIdx) - ment.startIdx;
    val indicesToTry = (Seq((0, mentWords.size)) ++ (0 to relHeadIdx).map(i => (i, relHeadIdx + 1))).filter(indices => {
      indices._2 - indices._1 == 1 || !isBlacklisted(mentWords(indices._1), ment.startIdx);
    }).filter(indices => indices._2 - indices._1 > 0 && indices._2 - indices._1 <= MaxQueryLen).distinct;
    for (indices <- indicesToTry) {
      // Query the full thing as is
      val queriesThisSlice = new ArrayBuffer[Query];
      val query = new Query(mentWords.slice(indices._1, indices._2), ment, indices, "STD", RemovePuncFromQuery);
      val firstWord = mentWords(indices._1);
      val lastWord = mentWords(indices._2 - 1);
      queriesThisSlice += query;
      // Handle capitalization: if the first word does not have any uppercase characters
      if (!firstWord.map(Character.isUpperCase(_)).reduce(_ || _) && Character.isLowerCase(firstWord(0))) {
        queriesThisSlice += new Query(Seq(wikiCase(firstWord)) ++ mentWords.slice(indices._1 + 1, indices._2), ment, indices, "WIKICASED", RemovePuncFromQuery);
      }
      // Stemming (but only on head alone)
      if (PluralQueryExpand && (indices._2 - indices._1) == 1 && firstWord.last == 's') {
        queriesThisSlice ++= queriesThisSlice.map(query => new Query(Seq(removePlural(query.words(0))), ment, indices, query.queryType + "-STEM", RemovePuncFromQuery));
      }
      queries ++= queriesThisSlice;
    }
    // Finally, strip punctuation from queries; we don't do this earlier because it makes it hard
    // to find the head
//    val finalQueries = if (RemovePuncFromQuery) {
//      queries.map(_.map(str => str.filter(c => !PuncList.contains(c))).filter(!_.isEmpty)).filter(!_.isEmpty)
//    } else {
//      queries;
//    }
    queries.filter(!_.getFinalQueryStr.isEmpty) ++ (if (addNilQuery) Seq(Query.makeNilQuery(ment)) else Seq[Query]());
  }

  def extractQueriesBest(ment: Mention, addNilQuery: Boolean = false): Seq[Query] = {
    val queries = new ArrayBuffer[Query]()
    val mentWords = ment.words
    val relHeadIdx = ment.contextTree.getSpanHeadACECustom(ment.startIdx, ment.endIdx) - ment.startIdx
    def addQuery(start: Int, end: Int, featsi:List[String]): Unit = {
      var feats = featsi // gaaaaa
      val thisSlice = new ArrayBuffer[Query]()
      val wrds = mentWords.slice(start, end)
      thisSlice += new Query(wrds, ment, (start, end), "STD", RemovePuncFromQuery, feats)
      val firstWord = wrds(0)
      val lastWord = wrds(wrds.size - 1)
      if((end - start)== 1)
        feats ++= List("SingleItemQuery")
      if (!firstWord.map(Character.isUpperCase(_)).reduce(_ || _) && Character.isLowerCase(firstWord(0))) {
        thisSlice += new Query(Seq(wikiCase(firstWord)) ++ wrds.drop(1), ment, (start, end), "WIKICASED", RemovePuncFromQuery, feats);
      }
      // Stemming (but only on head alone)
      if (PluralQueryExpand && (end - start) == 1 && firstWord.last == 's') {
        thisSlice ++= thisSlice.map(qu =>
          new Query(Seq(removePlural(qu.words(0))), ment, (start, end), qu.queryType + "-STEM", RemovePuncFromQuery, feats));
      }
      queries ++= thisSlice
    }
    addQuery(0, ment.endIdx - ment.startIdx, List("SimpleQuery", "FullTextQuery"))
    // TODO: make this ignore items that simply add a blacklisted word
    for(i <- 0 to relHeadIdx) {
      addQuery(i, relHeadIdx + 1, List("SimpleQuery", "PreHeadQuery"))
    }
    for(i <- relHeadIdx+1 until mentWords.size) {
      addQuery(relHeadIdx, i, List("SimpleQuery", "PostHeadQuery"))
    }
    // try filtering words
    val filterWords = mentWords.filter(!isBlacklisted(_, 0))
    if(filterWords.size != mentWords.size) {
      // we lost something, make new query
      queries += new Query(filterWords, ment, (ment.startIdx, ment.endIdx), "FIT", RemovePuncFromQuery, List("FilteredQuery"))
    }
    queries.filter(!_.getFinalQueryStr.isEmpty) ++ (if (addNilQuery) Seq(Query.makeNilQuery(ment)) else Seq[Query]())
  }
  
  def extractDenotationSetWithNil(queries: Seq[Query], queryDisambigs: Seq[Counter[String]], maxDenotations: Int): Seq[String] = {
    val choicesEachQuery = queryDisambigs.map(_.getSortedKeys().asScala);
    val optionsAndPriorities = (0 until queryDisambigs.size).flatMap(i => {
      val sortedKeys = queryDisambigs(i).getSortedKeys().asScala
      (0 until sortedKeys.size).map(j => (sortedKeys(j), j * 1000 + i));
    });
//    choicesEachQuery.foreach(Logger.logss(_));
//    Logger.logss(optionsAndPriorities);
    val allFinalOptions = Seq(NilToken) ++ optionsAndPriorities.sortBy(_._2).map(_._1).distinct;
    val finalOptionsTruncated = allFinalOptions.slice(0, Math.min(allFinalOptions.size, maxDenotations));
//    Logger.logss(finalOptions);
    finalOptionsTruncated;
  }
}
