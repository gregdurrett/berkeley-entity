package edu.berkeley.nlp.entity.joint

import edu.berkeley.nlp.entity.coref.DocumentGraph
import edu.berkeley.nlp.entity.Chunk
import com.sun.xml.internal.bind.v2.model.core.MaybeElement
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.ner.NerSystemLabeled
import edu.berkeley.nlp.entity.bp.Domain
import edu.berkeley.nlp.futile.fig.basic.Indexer
import scala.collection.JavaConverters._
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.entity.coref.UID
import edu.berkeley.nlp.entity.Driver
import edu.berkeley.nlp.entity.ner.NerFeaturizer
import scala.collection.mutable.HashSet
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.Document
import edu.berkeley.nlp.entity.ner.NerPruner

class JointDoc(val rawDoc: Document,
               val docGraph: DocumentGraph,
               val goldNERChunks: Seq[Seq[Chunk[String]]],
               val goldWikiChunks: Seq[Seq[Chunk[String]]]) {
  var savedNerPruner: Option[NerPruner] = None;
  val cachedStartNERDomain = new Domain(NerSystemLabeled.StdLabelIndexer.getObjects.asScala.filter(NerSystemLabeled.getStructuralType(_) != "I").toArray);
  val cachedWholeNERDomain = new Domain(NerSystemLabeled.StdLabelIndexer.getObjects.asScala.toArray);
  
  def cacheNerPruner(maybeNerPruner: Option[NerPruner]) {
    savedNerPruner = maybeNerPruner;
  }
  
  def getPredNerDomains(sentIdx: Int): Array[Domain[String]] = {
    val sentLen = rawDoc.words(sentIdx).size
    // If we're pruning
    if (savedNerPruner.isDefined) {
      val allOptions = savedNerPruner.get.pruneSentence(rawDoc, sentIdx);
      Array.tabulate(sentLen)(wordIdx => {
        new Domain(allOptions(wordIdx));
      });
    } else {
      Array.tabulate(sentLen)(wordIdx => if (wordIdx == 0) cachedStartNERDomain else cachedWholeNERDomain);
    }
  }
  
  def getGoldNerDomains(sentIdx: Int): Array[Domain[String]] = {
    val sentLen = rawDoc.words(sentIdx).size
    // If we're pruning
    if (savedNerPruner.isDefined) {
      val goldNESequence = NerSystemLabeled.convertToBIO(goldNERChunks(sentIdx), sentLen);
      // Need to get the possible options to see if it's possible
      val allOptions = savedNerPruner.get.pruneSentence(rawDoc, sentIdx);
      val somethingImpossible = (0 until sentLen).map(i => !allOptions(i).contains(goldNESequence(i))).reduce(_ || _);
      val impossibleGoldNERChunks = goldNERChunks(sentIdx).filter(chunk => {
        (chunk.start until chunk.end).map(wordIdx => !allOptions(wordIdx).contains(NerSystemLabeled.getGoldNETag(Array(chunk), wordIdx))).foldLeft(false)(_ || _);
      });
      Array.tabulate(sentLen)(wordIdx => {
        val isPossible = allOptions(wordIdx).contains(goldNESequence(wordIdx)) &&
                impossibleGoldNERChunks.filter(chunk => chunk.start <= wordIdx && wordIdx < chunk.end).size == 0;
        if (isPossible) {
          new Domain(Array(goldNESequence(wordIdx)));
        } else {
          new Domain(allOptions(wordIdx));
        }
      });
    } else {
      Array.tabulate(sentLen)(wordIdx => new Domain(Array(NerSystemLabeled.getGoldNETag(goldNERChunks(sentIdx), wordIdx))));
    }
  }
}

object JointDoc {
  
  def apply(rawDoc: Document,
            docGraph: DocumentGraph,
            maybeGoldNERChunks: Option[Seq[Seq[Chunk[String]]]],
            maybeGoldWikiChunks: Option[Seq[Seq[Chunk[String]]]]) = {
    val goldNERChunks = if (maybeGoldNERChunks.isDefined) {
      maybeGoldNERChunks.get
    } else {
      (0 until rawDoc.numSents).map(i => Seq[Chunk[String]]());
    }
    val goldWikiChunks = if (maybeGoldWikiChunks.isDefined) {
      maybeGoldWikiChunks.get
    } else {
      (0 until rawDoc.numSents).map(i => Seq[Chunk[String]]());
    }
    new JointDoc(rawDoc, docGraph, goldNERChunks, goldWikiChunks);
  }
  
  def assembleJointDocs(docGraphs: Seq[DocumentGraph],
                        goldConllDocsForNER: Seq[Document],
                        goldWikification: HashMap[String,HashMap[Int,ArrayBuffer[Chunk[String]]]]) = {
    docGraphs.map(docGraph => {
      val rawDoc = docGraph.corefDoc.rawDoc;
      val maybeGoldConllDoc = goldConllDocsForNER.filter(goldDoc => goldDoc.uid == rawDoc.uid);
      val maybeGoldNERChunks = if (maybeGoldConllDoc.size >= 1) {
        Some(maybeGoldConllDoc.head.nerChunks);
      } else {
        None;
      }
//      val goldWiki = if (goldWikification) {
//        Some(maybeGoldConllDoc.head);
//      } else
//        None;
//      }
      JointDoc(rawDoc, docGraph, maybeGoldNERChunks, None);
    });
  }
  
  def assessNERPruning(docs: Seq[JointDoc]) {
    var numPossibleLabelsRemaining = 0;
    var totalPossibleLabels = 0;
    var numRecalled = 0;
    var totalChunks = 0;
    var numGoldChunks = 0;
    var totalPossibleChunks = 0;
    for (doc <- docs) {
      for (sentIdx <- 0 until doc.rawDoc.numSents) {
        val goldChunks = doc.goldNERChunks(sentIdx);
        val domains = doc.getPredNerDomains(sentIdx);
        val numLabels = NerSystemLabeled.StdLabelIndexer.size
        for (wordIdx <- 0 until doc.rawDoc.words(sentIdx).size) {
          numPossibleLabelsRemaining += domains(wordIdx).size;
          totalPossibleLabels += numLabels;
        }
        for (chunk <- goldChunks) {
          var recalled = true;
          for (i <- chunk.start until chunk.end - 1) {
            val label = (if (i == chunk.start) "B-" else "I-") + chunk.label
            if (!domains(i).entries.contains(label)) {
              recalled = false
            }
          }
          if (recalled) numRecalled += 1;
          totalChunks += 1;
        }
        totalPossibleChunks += getPossibleChunks(domains).size;
      }
      numGoldChunks += doc.goldNERChunks.map(_.size).foldLeft(0)(_ + _);
    }
    Logger.logss("Number of possible chunks: " + totalPossibleChunks + ", number of gold chunks: " + numGoldChunks);
    Logger.logss("Amount pruned: " + GUtil.renderNumerDenom(numPossibleLabelsRemaining, totalPossibleLabels))
    Logger.logss("Oracle recall: " + GUtil.renderNumerDenom(numRecalled, totalChunks))
  }
  
//  def assessProperMentionRecallWithNER(docs: Seq[JointDoc]) {
//    var totalNumNERMents = 0;
//    // Check how many NER chunks disagree with mention spans
//    var numChunksAgreeing = 0;
//    var numChunksBiggerThanMent = 0;
//    var numChunksSmallerThanMent = 0;
//    // Check how many mention-relevant NER chunks we mess up on
//    var numMentionsMissedInIdentiFinder = 0;
//    var numMentionsMissedInOneBest = 0;
//    var numMentionsStructurallyMissedInIdentiFinder = 0;
//    var numMentionsStructurallyMissedInOneBest = 0;
//    var numMentionsStructurallyMissedInAll = 0;
//    for (doc <- docs) {
//      for (sentIdx <- 0 until doc.rawDoc.numSents) {
//        val goldChunks = doc.goldNERChunks(sentIdx);
//        val completeDomain = new Domain(NerSystemLabeled.StdLabelIndexer.getObjects.asScala.toArray);
//        val bestPredChunks = JointDocFactorGraphOnto.decodeNERProduceChunks((0 until doc.rawDoc.words(sentIdx).size).map(i => completeDomain), doc.nerMarginals.get(sentIdx).map(_.map(_.toDouble)));
//        val existingPredChunks = doc.rawDoc.nerChunks(sentIdx);
////        val prunedDomainThreshold = -1;
//        val prunedDomainThreshold = -Driver.nerPruningNegThreshold
//        val prunedDomains = doc.getNerDomains(sentIdx, NerSystemLabeled.StdLabelIndexer, false, prunedDomainThreshold);
//        val possibleChunks = getPossibleChunks(prunedDomains);
//        
//        val gmsToUse = doc.docGraph.corefDoc.goldMentions.filter(_.sentIdx == sentIdx);
//        val pmSpans = doc.docGraph.corefDoc.predMentions.filter(_.sentIdx == sentIdx).map(pm => pm.startIdx -> pm.endIdx);
//        val missedGms = gmsToUse.filter(gm => !pmSpans.contains(gm.startIdx -> gm.endIdx));
////        for (gm <- gmsToUse) {
//        for (gm <- missedGms) {
//          val maybeGoldNERChunk = ConllDoc.getCorrespondingNERChunk(doc.goldNERChunks(sentIdx), gm.headIdx);
//          if (maybeGoldNERChunk.isDefined) {
//            totalNumNERMents += 1;
//            val nerChunk = maybeGoldNERChunk.get
//            if (nerChunk.start == gm.startIdx && nerChunk.end == gm.endIdx) {
//              numChunksAgreeing += 1;
//              if (!existingPredChunks.contains(nerChunk)) {
//                numMentionsMissedInIdentiFinder += 1;
//              }
//              if (!existingPredChunks.map(chunk => chunk.start -> chunk.end).contains(nerChunk.start -> nerChunk.end)) {
//                numMentionsStructurallyMissedInIdentiFinder+= 1;
//              }
//              if (!bestPredChunks.contains(nerChunk)) {
//                numMentionsMissedInOneBest += 1;
//              }
//              if (!bestPredChunks.map(chunk => chunk.start -> chunk.end).contains(nerChunk.start -> nerChunk.end)) {
//                numMentionsStructurallyMissedInOneBest += 1;
//              }
//              if (!possibleChunks.contains(nerChunk.start -> nerChunk.end)) {
//                numMentionsStructurallyMissedInAll += 1;
//              }
//            } else if (nerChunk.start <= gm.startIdx && nerChunk.end >= gm.endIdx) {
////              Logger.logss("NER Chunk (bigger): " + doc.rawDoc.words(sentIdx).slice(nerChunk.start, nerChunk.end).reduce(_ + " " + _));
////              Logger.logss("Ment : " + doc.rawDoc.words(sentIdx).slice(gm.startIdx, gm.endIdx).reduce(_ + " " + _));
//              numChunksBiggerThanMent += 1;
//            } else if (nerChunk.start >= gm.startIdx && nerChunk.end <= gm.endIdx) {
////              Logger.logss("NER Chunk (smaller): " + doc.rawDoc.words(sentIdx).slice(nerChunk.start, nerChunk.end).reduce(_ + " " + _));
////              Logger.logss("Ment : " + doc.rawDoc.words(sentIdx).slice(gm.startIdx, gm.endIdx).reduce(_ + " " + _));
//              numChunksSmallerThanMent += 1;
//            }
//          }
//        }
//      }
//    }
//    Logger.logss(totalNumNERMents + " total ments with relevant NER chunks");
//    Logger.logss(numChunksAgreeing + " where NER and coref agree, " + numChunksBiggerThanMent +
//                 " where chunk is bigger than ment, " + numChunksSmallerThanMent + " where chunk is smaller than ment, ");
//    Logger.logss(numMentionsMissedInIdentiFinder + " / " + numMentionsStructurallyMissedInIdentiFinder + " missed in IdentiFinder (labeled / unlabeled)");
//    Logger.logss(numMentionsMissedInOneBest + " / " + numMentionsStructurallyMissedInOneBest + " missed in one best (labeled / unlabeled)");
//    Logger.logss("NA / " + numMentionsStructurallyMissedInAll + " missed in one best (labeled / unlabeled)");
//  }
  
  def getPossibleChunks(domains: Array[Domain[String]]): Seq[(Int,Int)] = {
    val isBLocation = (0 until domains.size).map(i => !domains(i).entries.filter(_.startsWith("B")).isEmpty);
    val isILocation = (0 until domains.size).map(i => !domains(i).entries.filter(_.startsWith("I")).isEmpty);
    val isOLocation = (0 until domains.size).map(i => !domains(i).entries.filter(_.startsWith("O")).isEmpty);
    val onlyOLocations = (0 until domains.size).filter(i => domains(i).entries.size == 1 && domains(i).entries(0) == "O");
    val lastValidEndMap = (0 until domains.size).map(i => {
      val futureOLocs = onlyOLocations.filter(_ > i);
      if (futureOLocs.isEmpty) domains.size else futureOLocs.head;
    });
    val possibleChunks = new ArrayBuffer[(Int,Int)]
    for (start <- 0 until domains.size; end <- start + 1 to lastValidEndMap(start)) {
      var okay = true;
      for (i <- start until end) {
        if ((i == start && !isBLocation(i)) || (i > start && !isILocation(i))) {
          okay = false;
        }
      }
      if (end < domains.size && !isBLocation(end) && !isOLocation(end)) {
        okay = false;
      }  
      if (okay) possibleChunks += start -> end;
    }
    possibleChunks
  }
}
