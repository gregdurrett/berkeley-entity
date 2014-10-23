package edu.berkeley.nlp.entity.wiki

import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.futile.util.CounterMap
import edu.berkeley.nlp.futile.util.Logger
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.entity.Chunk
import edu.berkeley.nlp.futile.util.Counter
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._
import scala.collection.mutable.HashSet
import edu.berkeley.nlp.entity.coref.Mention
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.entity.coref.MentionType

class FahrniWikifier(lexicon: CounterMap[String,String],
                     redirects: HashMap[String,HashSet[String]],
                     aceHeadsMap: HashMap[String,HashMap[Int,Seq[Chunk[(Int,Int)]]]]) extends Wikifier {
  
  var correct = 0;
  var oracleCorrect = 0;
  var total = 0;
  var correctNil = 0;
  var totalNil = 0;
  
  def getHeadSpanToUse(docName: String, ment: Mention, useBetterHead: Boolean): (Int, Int) = {
    val trueHeadIdxMatch = aceHeadsMap(docName)(ment.sentIdx).filter(chunk => chunk.start == ment.startIdx && chunk.end == ment.endIdx)
    if (useBetterHead && trueHeadIdxMatch.size >= 1 && trueHeadIdxMatch(0).label._1 >= ment.startIdx && trueHeadIdxMatch(0).label._2 <= ment.endIdx) {
      trueHeadIdxMatch(0).label;
    } else {
      (ment.headIdx, ment.headIdx + 1)
    }
  }
  
  def getQueries(docName: String, ment: Mention, useBetterHead: Boolean, useRedirects: Boolean): Seq[String] = {
    val trueHeadIdx = getHeadSpanToUse(docName, ment, useBetterHead);
    getQueries(ment, trueHeadIdx, useRedirects);
  }
  
  def getQueries(ment: Mention, headSpan: (Int, Int), useRedirects: Boolean): Seq[String] = {
    val headStrLc = ment.wordsLc.slice(headSpan._1 - ment.startIdx, headSpan._2 - ment.startIdx).reduce(_ + " " + _);
    val rawQueries = if (headStrLc.endsWith("s")) {
      Seq(headStrLc, headStrLc.dropRight(1))
    } else {
      Seq(headStrLc)
    }
    rawQueries.flatMap(query => {
      if (useRedirects && redirects.contains(query)) {
        Seq(query) ++ redirects(query);
      } else {
        Seq(query)
      }
    });
  }
  
  def wikify(docName: String, ment: Mention): String = {
    wikifyGetCounter(docName, ment).argMax;
  }
  
  def wikifyGetTitleSet(docName: String, ment: Mention): Seq[String] = {
    GUtil.getKeysInOrder(wikifyGetCounter(docName, ment))
  }
  
  def wikifyGetPriorForJointModel(docName: String, ment: Mention) = {
    wikifyGetCounter(docName, ment);
  }
  
  
  def traceExecutionGetWinningQueries(docName: String, ment: Mention, goldTitles: Seq[String]) = {
    val queries = getQueries(docName, ment, true, true);
    val goodQueries = new ArrayBuffer[String];
    for (query <- queries) {
      if (lexicon.containsKey(query)) {
        if (containsCorrect(goldTitles, GUtil.getKeysInOrder(lexicon.getCounter(query)))) {
          goodQueries += query;
        }
      }
    }
    goodQueries;
  }
  
  def traceExecution(docName: String, ment: Mention, goldTitles: Seq[String]) = {
    val modCounter = wikifyGetCounter(docName, ment, useRedirects = false, useBetterHead = true);
    if (!containsCorrect(goldTitles, GUtil.getKeysInOrder(modCounter))) {
      "REDIRECT";
    } else {
      "NO-REDIRECT";
    }
  }
  
  def wikifyGetCounter(docName: String, ment: Mention, useRedirects: Boolean = true, useBetterHead: Boolean = true): Counter[String] = {
    val resultsCounter = new Counter[String];
    if (ment.mentionType.isClosedClass) {
      resultsCounter.incrementCount(ExcludeToken, 2.0);
    } else {
      for (query <- getQueries(docName, ment, useRedirects, useBetterHead)) {
        if (lexicon.containsKey(query)) {
          resultsCounter.incrementAll(lexicon.getCounter(query));
        }
      }
    }
    if (resultsCounter.isEmpty) {
      resultsCounter.incrementCount(NilToken, 2.0);
    }
    resultsCounter;
  }
  
  def oracleWikify(docName: String, ment: Mention, goldTitles: Seq[String]) {
    val title = wikify(docName, ment);
    if (isCorrect(goldTitles, title)) {
      correct += 1;
    } else {
      Logger.logss("Incorrect: " + title + " / " + goldTitles);
    }
    val titleSeq = wikifyGetTitleSet(docName, ment);
    if (containsCorrect(goldTitles, titleSeq.toSet)) {
      oracleCorrect += 1;
    }
    total += 1;
  }
  
  
  def oracleWikifyNil(docName: String, ment: Mention) {
    totalNil += 1;
  }
  
  
  def printDiagnostics() {
    Logger.logss("TOTAL: " + total + ", " + totalNil + " nils");
    Logger.logss("  Correct: " + correct + ", correct NIL: " + correctNil);
    Logger.logss("  Oracle correct: " + oracleCorrect);
  }
}

object FahrniWikifier {
  
  def extractFahrniLexicon(lexiconFile: String) = {
    val lexicon = new CounterMap[String,String]
    val lexiconItr = IOUtils.lineIterator(IOUtils.openInHard(lexiconFile));
    Logger.logss("Reading Fahrni lexicon");
    var counter = 0;
    while (lexiconItr.hasNext) {
      if (counter % 100000 == 0) {
        Logger.logss("Read " + counter + " lines");
      }
      val line = lexiconItr.next;
      val splitLine = line.split("\t");
      for (i <- 1 until splitLine.size) {
        val lastColonIndex = splitLine(i).lastIndexOf(":");
        val penultimateColonIndex = splitLine(i).lastIndexOf(":", lastColonIndex - 1);
//        lexicon.incrementCount(splitLine(0), splitLine(i).substring(0, firstColonIndex).toInt, splitLine(i).substring(firstColonIndex + 1, secondColonIndex).toDouble);
        lexicon.incrementCount(splitLine(0), splitLine(i).substring(0, penultimateColonIndex), splitLine(i).substring(penultimateColonIndex + 1, lastColonIndex).toDouble);
      }
      counter += 1;
    }
    lexicon;
  }
  
  def extractRedirects(redirectsFile: String) = {
    val redirects = new HashMap[String,HashSet[String]]();
    val lineItr = IOUtils.lineIterator(IOUtils.openInHard(redirectsFile));
    var counter = 0;
    while (lineItr.hasNext) {
      if (counter % 100000 == 0) {
        Logger.logss("Read " + counter + " lines");
      }
      val nextLineEntries = lineItr.next().split("\t");
      if (!redirects.contains(nextLineEntries(0))) {
        redirects.put(nextLineEntries(0), new HashSet[String])
      }
      redirects(nextLineEntries(0)) += nextLineEntries(1);
      counter += 1;
    }
    redirects;
  }
  
  def extractRawFahrniLexicon(lexiconFile: String) = {
    val lexicon = extractFahrniLexicon(lexiconFile);
    val newCounterMap = new CounterMap[String,Int];
    for (key <- lexicon.keySet().asScala) {
      val counter = lexicon.getCounter(key);
      for (key2 <- counter.keySet.asScala) {
        newCounterMap.incrementCount(key, key2.toInt, counter.getCount(key2))
      }
    }
    newCounterMap;
  }
  
  def readFahrniWikifier(lexiconFile: String,
                         redirectsFile: String) = {
    Logger.logss("Loading ACE heads");
    val aceHeads = ACEMunger.mungeACEToGetHeads("data/ace05/ace05-all-copy");
    Logger.logss("Loading redirects");
    val redirects = extractRedirects(redirectsFile);
    Logger.logss("Loading lexicon");
    val lexicon = extractFahrniLexicon(lexiconFile);
    new FahrniWikifier(lexicon, redirects, aceHeads);
  }
}
