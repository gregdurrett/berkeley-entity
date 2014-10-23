package edu.berkeley.nlp.entity.wiki

import edu.berkeley.nlp.entity.coref.Mention
import edu.berkeley.nlp.futile.util.Counter
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.Chunk
import scala.collection.JavaConverters._
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.coref.CorefDoc
import edu.berkeley.nlp.futile.util.CounterMap
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.entity.coref.MentionType
import edu.berkeley.nlp.futile.fig.basic.Indexer

class BasicWikifier(val wikiDB: WikipediaInterface,
                    val queryChooser: Option[QueryChooser] = None,
                    val trainingDocs: Option[Seq[CorefDoc]] = None,
                    val trainingWikiAnnots: Option[CorpusWikiAnnots] = None,
                    val trainingMemorizationThreshold: Int = 1,
                    val aceHeads: Option[HashMap[String,HashMap[Int,Seq[Chunk[(Int,Int)]]]]] = None) extends Wikifier {
  
  val basicQueryChooser = QueryChooser.getBasicQueryChooser;
  
  val memorizedTrainCounts = new CounterMap[String,String];
  if (trainingDocs.isDefined && trainingWikiAnnots.isDefined) {
    for (doc <- trainingDocs.get) {
      val docWikiAnnots = trainingWikiAnnots.get(doc.rawDoc.docID);
      for (ment <- doc.predMentions) {
        val wikiAnnot = getGoldWikification(docWikiAnnots, ment);
        for (annot <- wikiAnnot) {
          memorizedTrainCounts.incrementCount(ment.spanToString, annot, 1.0);
        }
      }
    }
  }
  
  var correct = 0;
  var oracleCorrect = 0;
  var oracleOneBestCorrect = 0;
  var blacklistCorrect = 0;
  var unrecalledReasonable = 0;
  var unrecalled = 0;
  val countHistogramReasonable = new Counter[String];
  val countHistogramAll = new Counter[String];
  var total = 0;
  
  var correctNil = 0;
  var totalNil = 0;
  
  def getHeadIdxToUse(docName: String, ment: Mention) = {
    if (aceHeads.isDefined) {
      val aceHeadIdx = if (aceHeads.get.contains(docName) && aceHeads.get(docName).contains(ment.sentIdx)) {
        val possibleMatchingChunk = aceHeads.get(docName)(ment.sentIdx).filter(chunk => chunk.start == ment.startIdx && chunk.end == ment.endIdx);
        if (possibleMatchingChunk.size >= 1) {
          // Take the end of the chunk label
          possibleMatchingChunk.head.label._2 - 1;
        } else {
          -1;
        }
      } else {
        -1;
      }
      if (aceHeadIdx != -1 && aceHeadIdx != ment.headIdx) {
        Logger.logss("Using different head idx!");
        aceHeadIdx
      } else {
        ment.headIdx;
      }
    } else {
      ment.headIdx;
    }
  }

  def wikify(docName: String, ment: Mention): String = {
    if (ment.mentionType.isClosedClass) {
      ExcludeToken
    } else {
      val trainCounts = memorizedTrainCounts.getCounter(ment.spanToString);
      if (trainCounts.totalCount >= trainingMemorizationThreshold) {
        trainCounts.argMax;
      } else {
//        val result = wikiDB.disambiguateBest(ment, getHeadIdxToUse(docName, ment));
        val queries = Query.extractQueriesBest(ment, true)
        val query = (if (queryChooser.isDefined) queryChooser.get else basicQueryChooser).pickQuery(queries, wikiDB);
        wikiDB.disambiguateBestNoDisambig(query);
      }
    }
  }
  
  def wikifyGetTitleSet(docName: String, ment: Mention): Seq[String] = {
    GUtil.getKeysInOrder(wikifyGetCounter(docName, ment));
  }
  
  def wikifyGetPriorForJointModel(docName: String, ment: Mention) = {
    val counter = new Counter[String];
    if (ment.mentionType.isClosedClass) {
      counter.incrementCount(ExcludeToken, 2.0);
    } else {
      val trainCounter = wikifyGetCounterFromTrain(docName, ment);
      if (trainCounter.totalCount >= trainingMemorizationThreshold) {
        counter.incrementAll(trainCounter);
      } else {
        counter.incrementAll(wikifyGetCounter(docName, ment));
        if (counter.isEmpty) {
          counter.incrementCount(NilToken, 2.0);
        }
      }
    }
    counter;
  }
  
  def wikifyGetCounter(docName: String, ment: Mention) = {
//    wikiDB.disambiguateBestGetAllOptions(ment, getHeadIdxToUse(docName, ment))
    if (queryChooser.isDefined) {
      wikiDB.disambiguateBestGetAllOptions(queryChooser.get.pickQuery(Query.extractQueriesBest(ment, true), wikiDB));
    } else {
      wikiDB.disambiguateBestGetAllReasonableOptions(ment, getHeadIdxToUse(docName, ment))
    }
  }
  
  def wikifyGetCounterFromTrain(docName: String, ment: Mention) = {
    memorizedTrainCounts.getCounter(ment.spanToString);
  }
  
  def oracleWikifyNil(docName: String, ment: Mention) {
    totalNil += 1;
    if (wikify(docName, ment) == NilToken) {
      correctNil += 1;
    }
  }
  
  def oracleWikify(docName: String, ment: Mention, goldTitles: Seq[String]) {
    total += 1;
//    val reasonableOptions = wikiDB.disambiguateGetAllReasonableOptions(ment);
//    if (!containsCorrect(goldTitles, reasonableOptions.keySet.asScala)) {
//      unrecalledReasonable += 1;
//    } else {
//      countHistogramReasonable.incrementCount(bucketCount(getNumKeysHigherThanKey(goldTitles, reasonableOptions)), 1.0);
//    }
    val allOptions = wikifyGetCounter(docName, ment);
    if (!containsCorrect(goldTitles, allOptions.keySet.asScala)) {
      unrecalled += 1;
//      val queries = WikipediaTitleGivenSurfaceDB.extractQueriesBest(ment);
//      Logger.logss("Queries: " + queries);
//      for (query <- queries) {
//        val counter = wikiDB.titleGivenSurfaceDB.surfaceToTitle.getCounter(query);
//        if (!counter.isEmpty) {
//          Logger.logss("  Query: " + query);
//          Logger.logss("  Raw counter: " + counter);
//          Logger.logss("  Redirected: " + wikiDB.redirectsDB.followRedirectsCounter(counter));
//        }
//      }
//      Logger.logss("Completely unrecalled: [" + ment.spanToString + "], gold titles = " + goldTitles);
    } else {
      countHistogramAll.incrementCount(bucketCount(getNumKeysHigherThanKey(goldTitles, allOptions)), 1.0);
    }
    val allOptionsUnreasonable = wikiDB.disambiguateBestGetAllOptions(ment, getHeadIdxToUse(docName, ment))
    // Could only get it using an "unreasonable" query
    if (!containsCorrect(goldTitles, allOptions.keySet.asScala) && containsCorrect(goldTitles, allOptionsUnreasonable.keySet.asScala)) {
      val queries = Query.extractQueriesBest(ment).map(_.getFinalQueryStr);
      Logger.logss("Queries: " + queries);
      for (query <- queries) {
        val counter = wikiDB.titleGivenSurfaceDB.surfaceToTitle.getCounter(query);
        if (!counter.isEmpty) {
          Logger.logss("  Query: " + query);
          Logger.logss("  Raw counter: " + counter);
          Logger.logss("  Redirected: " + wikiDB.redirectsDB.followRedirectsCounter(counter));
        }
      }
      Logger.logss("Unrecalled by reasonable: [" + ment.spanToString + "], gold titles = " + goldTitles);
    }
    val allOptionsOneBest = wikiDB.disambiguateBestGetAllOneBestOptions(ment, getHeadIdxToUse(docName, ment))
    if (containsCorrect(goldTitles, allOptionsOneBest.keySet.asScala)) {
      oracleOneBestCorrect += 1;
    }
    
    val finalPredTitle = wikify(docName, ment);
    if (isCorrect(goldTitles, finalPredTitle)) {
      correct += 1;
    }
//    val surfaceToTitle = wikiDB.titleGivenSurfaceDB.surfaceToTitle;
//    val queries = WikipediaTitleGivenSurfaceDB.extractQueries(ment);
//    for (query <- queries) {
//      if (surfaceToTitle.contains(query)) {
//        
//      }
//    }
  }
  
  private def bucketCount(count: Int) = {
    if (count < 5) "" + count else if (count < 10) "5-9" else if (count < 20) "10-19" else if (count < 50) "20-49" else if (count < 100) "50-99" else "100+";
  }
  
  private def getNumKeysHigherThanKey(keys: Seq[String], counter: Counter[String]) = {
    var num = 0;
    var bestCount = keys.map(counter.getCount(_)).max;
    for (k <- counter.keySet.asScala) {
      if (!keys.contains(k) && counter.getCount(k) > bestCount) {
        num += 1;
      }
    }
    num;
  }
  
  def printDiagnostics() {
    Logger.logss("TOTAL: " + total + ", " + totalNil + " nils");
    Logger.logss("  Correct: " + correct + " (" + (total - correct) + ") " + GUtil.renderNumerDenom(correct, total) + ", correct NIL: " + correctNil);
    Logger.logss("  oracleOneBest: " + oracleOneBestCorrect + " (" + (total - oracleOneBestCorrect) + ") " + GUtil.renderNumerDenom(oracleOneBestCorrect, total));
//    Logger.logss("  unrecalledReasonable: " + unrecalledReasonable);
    Logger.logss("  unrecalled: " + unrecalled + " (" + (total - unrecalled) + ") " + GUtil.renderNumerDenom(total - unrecalled, total));
    Logger.logss("  countHistogramReasonable: " + countHistogramReasonable);
    Logger.logss("  countHistogram: " + countHistogramAll);
  }
}
