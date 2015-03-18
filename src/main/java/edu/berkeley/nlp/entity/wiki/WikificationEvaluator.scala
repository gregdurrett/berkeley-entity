package edu.berkeley.nlp.entity.wiki

import edu.berkeley.nlp.entity.{Document, Chunk, GUtil}
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.util.Counter
import scala.collection.JavaConverters._
import edu.berkeley.nlp.entity.joint.JointDocACE
import java.io.PrintWriter

import scala.collection.mutable.ArrayBuffer

object WikificationEvaluator {
  
  def removeExcludes(chunks: Seq[Chunk[String]]) = chunks.filter(chunk => chunk.label != ExcludeToken)
  def removeExcludesSeq(chunks: Seq[Chunk[Seq[String]]]) = chunks.filter(chunk => !chunk.label.sameElements(Seq(ExcludeToken)));
  
  def removeNils(chunks: Seq[Chunk[String]]) = chunks.filter(chunk => chunk.label != NilToken)
  def removeNilsSeq(chunks: Seq[Chunk[Seq[String]]]) = chunks.filter(chunk => !chunk.label.sameElements(Seq(NilToken)));
  
  def evaluateWikiChunksBySent(allGoldChunksBySent: Iterable[Seq[Chunk[Seq[String]]]], allPredChunksBySent: Iterable[Seq[Chunk[String]]]) {
//    NEEvaluator.evaluateChunksBySent(allGoldChunksBySent.map(removeNulls(_)),
//                                     allPredChunksBySent.map(removeNulls(_)),
//                                     printFineGrainedResults = false,
//                                     ignoreCase = true);
    var correct = 0;
    var totalPred = 0;
    var totalGold = 0;
    var correctNil = 0;
    var totalPredNil = 0;
    var totalGoldNil = 0;
    for ((goldChunksRaw, predChunksRaw) <- allGoldChunksBySent.zip(allPredChunksBySent)) {
      val goldChunks = goldChunksRaw.filter(_.label.size >= 1);
      val predChunks = removeExcludes(predChunksRaw);
      // Filter out NILs
      val goldChunksNoNils = goldChunks.filter(_.label.head != NilToken);
      val predChunksNoNils = removeNils(predChunks);
      totalPred += predChunksNoNils.size;
      totalGold += goldChunksNoNils.size;
      for (predChunk <- predChunksNoNils) {
        val matchingGoldChunks = goldChunksNoNils.filter(chunk => chunk.start == predChunk.start && chunk.end == predChunk.end);
        if (matchingGoldChunks.size > 0 && isCorrect(matchingGoldChunks.head.label, predChunk.label)) {
          correct += 1;
        }
      }
      // Filter out non-NILs
      val goldChunksNil = goldChunks.filter(_.label.head == NilToken);
      val predChunksNil = predChunks.filter(_.label == NilToken);
      totalPredNil += predChunksNil.size;
      totalGoldNil += goldChunksNil.size;
      for (predChunk <- predChunksNil) {
        if (goldChunksNil.filter(chunk => chunk.start == predChunk.start && chunk.end == predChunk.end).size > 0) {
          correctNil += 1;
        }
      }
    }
    Logger.logss("Results (non-NILs): " + GUtil.renderPRF1(correct, totalPred, totalGold));
    Logger.logss("Results (NILs): " + GUtil.renderPRF1(correctNil, totalPredNil, totalGoldNil));
  }
  
  /**
   * N.B. This isn't quite correct because the set of strings
   */
  def evaluateBOTF1(allGoldTitles: Seq[Set[Set[String]]], allPredTitles: Seq[Set[String]]) {
    var correct = 0;
    var precDenom = 0;
    var recDenom = 0;
    for (i <- 0 until allGoldTitles.size) {
      for (title <- allPredTitles(i)) {
        var markedCorrect = false;
        for (goldTitleSet <- allGoldTitles(i)) {
          markedCorrect = markedCorrect || isCorrect(goldTitleSet.toSeq, title);
        }
        if (markedCorrect) {
          correct += 1;
        }
      }
      precDenom += allPredTitles(i).size;
      recDenom += allGoldTitles(i).size;
    }
    Logger.logss("Results (BOT F1): " + GUtil.renderPRF1(correct, precDenom, recDenom));
  }


  // create sets of all the gold document references, and all the documents
  // that we generate, and then compute an F1
  def evaluateBOTF1_mfl(results : Map[Document, Seq[(Seq[String], Seq[String], Document)]]) = {
    // f1 = 2 * precision * recall / (percison + recall)
    var correct = 0
    var precDenom = 0
    var recDenom = 0
    for((doc, matches) <- results) {
      var seenBefore = Set[String]()
      val allGold = Set(matches.flatMap(_._1):_*)
      val allChoosen = Set(matches.map(_._2(0)):_*) //Set(matches.flatMap(_._2):_*)

      /*for((gold, selected, _) <- matches) {
        val goldS = Set(gold:_*)
        val selectedS = Set(selected(0)) //Set(selected:_*)
        val ints = goldS & selectedS
        //if(!ints.subsetOf(seenBefore)) {
          correct += ints.size
          seenBefore ++= ints
        //}
      }*/
      // TODO: something wrong with computing the set intersection

      val dprecDenom = allChoosen.size
      val drecDenom = allGold.size
      var dcorrect = 0
      allChoosen.foreach(c => {
        if(isCorrect(allGold.toSeq, c))
          dcorrect += 1
      })
      //val diff = (allGold ++ allChoosen) -- (allGold & allChoosen)
      //val dcorrect = (allGold & allChoosen).size
      //Logger.logss("Document f1: "+GUtil.renderPRF1(dcorrect, dprecDenom, drecDenom))
      precDenom += dprecDenom
      recDenom += drecDenom
      correct += dcorrect
    }
    Logger.logss("Results (BOT F1): " + GUtil.renderPRF1(correct, precDenom, recDenom))
  }

  
  def convertChunksToBagOfTitles(titles: Iterable[Seq[Chunk[String]]]): Set[String] = {
    val bagOfTitles = titles.flatMap(sentTitles => {
      removeNils(removeExcludes(sentTitles)).map(_.label).toSet;
    }).toSet;
//    Logger.logss(bagOfTitles);
    bagOfTitles;
  }
  
  def convertSeqChunksToBagOfTitles(titles: Iterable[Seq[Chunk[Seq[String]]]]): Set[Set[String]] = {
    titles.flatMap(sentTitles => {
      removeNilsSeq(removeExcludesSeq(sentTitles)).map(_.label.toSet).toSet;
    }).toSet;
  }
  
  def evaluateFahrniMetrics(goldWikiAnnots: Seq[Seq[Chunk[Seq[String]]]], predWikiAnnots: Seq[Seq[Chunk[String]]], wikiLabelsInTrain: Set[String]) {
    val eventCounter = new Counter[String];
    for (sentIdx <- 0 until predWikiAnnots.size) {
      for (chunk <- predWikiAnnots(sentIdx)) {
        val correspondingGold = extractChunkLabel(goldWikiAnnots(sentIdx), chunk.start, chunk.end);
        if (!correspondingGold.isDefined || correspondingGold.get.size == 0) {
          // Do nothing
//          Logger.logss("WARNING: Dropping gold");
        } else {
          val goldChunkLabel = correspondingGold.get;
          val goldIsNil = goldChunkLabel.size == 1 && goldChunkLabel(0) == NilToken;
          val unseenInTrain = !goldIsNil && (wikiLabelsInTrain & goldChunkLabel.toSet).size == 0;
          val correctnessLabel = if (!goldIsNil) {
            if (isCorrect(goldChunkLabel, chunk.label)) "cKB" else if (chunk.label == NilToken) "wKB_NIL" else "wKB_KB"
          } else {
            if (chunk.label == NilToken) "cNIL" else "wNIL_KB"
          }
          val correctnessLabelWithUnseen = correctnessLabel + (if (unseenInTrain) "_UNK" else "");
          eventCounter.incrementCount(correctnessLabelWithUnseen, 1.0);
        }
      }
    }
    displayResultsAllExs(eventCounter, true);
  }
  
  def displayResultsAllExs(counter: Counter[String], doUnks: Boolean) {
    Logger.logss("All examples");
    val correctKeys = counter.keySet.asScala.filter(key => key.startsWith("c")).toSeq
    val correctCounts = correctKeys.map(counter.getCount(_).toInt).foldLeft(0)(_ + _);
    val incorrectKeys = counter.keySet.asScala.filter(key => key.startsWith("w")).toSeq;
    val incorrectCounts = incorrectKeys.map(counter.getCount(_).toInt).foldLeft(0)(_ + _);
    Logger.logss("Accuracy: " + GUtil.renderStandardPercentage(correctCounts, correctCounts + incorrectCounts));
    val ckb = counter.getCount("cKB").toInt + counter.getCount("cKB_UNK").toInt;
    val cnil = counter.getCount("cNIL").toInt + counter.getCount("cNIL_UNK").toInt;
    val wkbkb = counter.getCount("wKB_KB").toInt + counter.getCount("wKB_KB_UNK").toInt;
    val wkbnil = counter.getCount("wKB_NIL").toInt + counter.getCount("wKB_NIL_UNK").toInt;
    val wnilkb = counter.getCount("wNIL_KB").toInt + counter.getCount("wNIL_KB_UNK").toInt;
    Logger.logss("KB: " + GUtil.renderPRF1(ckb, ckb + wkbkb + wnilkb, ckb + wkbkb + wkbnil));
    Logger.logss("NIL: " + GUtil.renderPRF1(cnil, cnil + wkbnil, cnil + wnilkb));
    if (doUnks) {
      val ckbUnk = counter.getCount("cKB_UNK").toInt;
      val wkbkbUnk = counter.getCount("wKB_KB_UNK").toInt;
      val wkbnilUnk = counter.getCount("wKB_NIL_UNK").toInt;
      val wnilkbUnk = counter.getCount("wNIL_KB_UNK").toInt;
      Logger.logss("Restricted to UNKs: " + GUtil.renderPRF1(ckbUnk, ckbUnk + wkbkbUnk + wnilkbUnk, ckbUnk + wkbkbUnk + wkbnilUnk));
      val ckbSeen = counter.getCount("cKB").toInt;
      val wkbkbSeen = counter.getCount("wKB_KB").toInt;
      val wkbnilSeen = counter.getCount("wKB_NIL").toInt;
      val wnilkbSeen = counter.getCount("wNIL_KB").toInt;
      Logger.logss("Restricted to non-UNKs: " + GUtil.renderPRF1(ckbSeen, ckbSeen + wkbkbSeen + wnilkbSeen, ckbSeen + wkbkbSeen + wkbnilSeen));
    }
  }
  
  
  def writeWikificationRightAndWrong(writer: PrintWriter,
                                     auxWriter: PrintWriter,
                                     doc: JointDocACE,
                                     goldWikiAnnots: Seq[Seq[Chunk[Seq[String]]]],
                                     rawChunkNames: CorpusAnnots[(String,String,Int)],
                                     docName: String,
                                     predChunks: Seq[Seq[Chunk[String]]],
                                     wikiLabelsInTrain: Set[String]) {
    for (sentIdx <- 0 until predChunks.size) {
      for (chunk <- predChunks(sentIdx)) {
        val correspondingGold = extractChunkLabel(goldWikiAnnots(sentIdx), chunk.start, chunk.end);
        if (!correspondingGold.isDefined || correspondingGold.get.size == 0) {
          // Do nothing
//          Logger.logss("WARNING: Dropping gold");
        } else {
          val goldChunkLabel = correspondingGold.get;
          val goldIsNil = goldChunkLabel.size == 1 && goldChunkLabel(0) == NilToken;
          val unseenInTrain = !goldIsNil && (wikiLabelsInTrain & goldChunkLabel.toSet).size == 0;
          val correctnessLabel = if (!goldIsNil) {
            if (isCorrect(goldChunkLabel, chunk.label)) "cKB" else if (chunk.label == NilToken) "wKB_NIL" else "wKB_KB"
          } else {
            if (chunk.label == NilToken) "cNIL" else "wNIL_KB"
          }
          val correctnessLabelWithUnseen = correctnessLabel + (if (unseenInTrain) "_UNK" else "");
          val chunkName = extractAnnotation(rawChunkNames, docName, sentIdx, chunk.start, chunk.end);
          if (!chunkName.isDefined) {
            Logger.logss("WARNING: Couldn't recover name for chunk: " + docName + " " + sentIdx + " " + chunk.start + " " + chunk.end);
          } else {
            writer.println(chunkName.get._1 + "\t" + chunkName.get._2 + "\t" + chunkName.get._3 + "\t" + correctnessLabelWithUnseen);
            auxWriter.println(chunkName.get._1 + "\t" + chunkName.get._2 + "\t" + chunkName.get._3 + "\t" + doc.rawDoc.words(sentIdx).slice(chunk.start, chunk.end).foldLeft("")(_ + " " + _).trim + "\t" + goldChunkLabel + "\t" + chunk.label);
          }
        }
      }
    }
  }
  
//  def computeOracle(jointDocGraphs: Seq[JointDocFactorGraphACE]) {
//    var possible = 0;
//    var nonNull = 0;
//    for (jointDocGraph <- jointDocGraphs) {
//      for (i <- 0 until jointDocGraph.docGraph.getMentions.size) {
//        val goldWiki = jointDocGraph.doc.getGoldWikLabels(jointDocGraph.doc.docGraph.getMention(i));
//        if (goldWiki.size >= 1) {
//          nonNull += 1;
//          if ((jointDocGraph.wikiNodes(i).domain.entries.map(_.toLowerCase).toSet & goldWiki.map(_.toLowerCase).toSet).size > 0) {
//            possible += 1;
//          }
//        }
//      }
//    }
//    Logger.logss(possible + " possible in current graph out of " + nonNull + " non-null");
//  }
}
