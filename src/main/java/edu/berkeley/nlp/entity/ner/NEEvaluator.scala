package edu.berkeley.nlp.entity.ner

import edu.berkeley.nlp.entity.ConllDoc
import edu.berkeley.nlp.entity.coref.Mention
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.coref.CorefSystem
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.entity.DepConstTree
import edu.berkeley.nlp.entity.Chunk
import edu.berkeley.nlp.futile.util.Counter
import scala.collection.JavaConverters._
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.futile.LightRunner
import scala.collection.mutable.HashSet
import edu.berkeley.nlp.entity.coref.UID
import edu.berkeley.nlp.entity.joint.JointDoc
import edu.berkeley.nlp.entity.ConllDocReader

object NEEvaluator {
  
  def neFilter(label: String) = label != "QUANTITY" && label != "CARDINAL" && label != "PERCENT"
    
  def shareHead(tree: DepConstTree, neChunk: Chunk[String], corefChunks: Seq[Chunk[Int]]) = {
    val neChunkHead = tree.getSpanHead(neChunk.start, neChunk.end);
    corefChunks.map(chunk => tree.getSpanHead(chunk.start, chunk.end)).contains(neChunkHead);
  }
  
//  def reprogramTags(docs: Seq[ConllDoc], mentionsAndRevisedPredTags: Seq[Seq[(Mention,String)]]): Seq[ConllDoc] = {
//    docs.zip(mentionsAndRevisedPredTags).map(docAndRevision => reprogramTags(docAndRevision._1, docAndRevision._2));
//  }
//  
//  def reprogramTags(doc: ConllDoc, mentionsAndRevisedPredTags: Seq[(Mention,String)]): ConllDoc = {
//    val newNerChunks = (0 until doc.numSents).map(sentIdx => doc.nerChunks(sentIdx).map(nerChunk => {
//      val relevantMentAndTag = mentionsAndRevisedPredTags.filter(mentAndTag => mentAndTag._1.sentIdx == sentIdx && nerChunk.start <= mentAndTag._1.headIdx && mentAndTag._1.headIdx < nerChunk.end);
//      if (relevantMentAndTag.size == 1) {
//        new Chunk[String](nerChunk.start, nerChunk.end, relevantMentAndTag(0)._2);
//      } else {
//        nerChunk
//      }
//    }));
//    new ConllDoc(doc.docID, doc.docPartNo, doc.words, doc.pos, doc.trees, newNerChunks, doc.corefChunks, doc.speakers, doc.rawText);
//  }
  
  def reprogramChunks(chunks: Seq[Seq[Chunk[String]]], mentionsAndRevisedPredTags: Seq[(Mention,String)]): Seq[Seq[Chunk[String]]]= {
    (0 until chunks.size).map(sentIdx => chunks(sentIdx).map(nerChunk => {
      val relevantMentAndTag = mentionsAndRevisedPredTags.filter(mentAndTag => mentAndTag._1.sentIdx == sentIdx && nerChunk.start <= mentAndTag._1.headIdx && mentAndTag._1.headIdx < nerChunk.end);
      if (relevantMentAndTag.size == 1) {
        new Chunk[String](nerChunk.start, nerChunk.end, relevantMentAndTag(0)._2);
      } else {
        nerChunk
      }
    }));
  }
  
  def evaluate(goldDocs: Seq[ConllDoc], predDocs: Seq[ConllDoc]) {
    evaluateChunks(goldDocs, predDocs.map(_.nerChunks));
  }

  def evaluateChunks(goldDocs: Seq[ConllDoc], allPredChunks: Seq[Seq[Seq[Chunk[String]]]]) {
    var correct = 0;
    val correctByLabel = new Counter[String];
    var correctSameHead = 0;
    var oracleCorrect = 0;
    var oracleCorrectSameHead = 0;
    var totalPred = 0;
    val totalPredByLabel = new Counter[String];
    var totalGold = 0;
    val totalGoldByLabel = new Counter[String];
    for (docIdx <- 0 until goldDocs.size) {
      val goldDoc = goldDocs(docIdx);
      for (sentIdx <- 0 until goldDoc.words.size) {
        // All NE chunks
        val predChunks = allPredChunks(docIdx)(sentIdx);
        val goldChunks = goldDoc.nerChunks(sentIdx);
        // Restrict to chunks of certain types
//        val predChunks = predDoc.nerChunks(sentIdx).filter(chunk => neFilter(chunk.label));
//        val goldChunks = goldDoc.nerChunks(sentIdx).filter(chunk => neFilter(chunk.label));
        // Restrict to chunks that overlap GMs
//        val predChunks = predDoc.nerChunks(sentIdx).filter(chunk => neFilter(chunk.label)).filter(chunk => shareHead(goldDoc.trees(sentIdx), chunk, goldDoc.corefChunks(sentIdx)));
//        val goldChunks = goldDoc.nerChunks(sentIdx).filter(chunk => neFilter(chunk.label)).filter(chunk => shareHead(goldDoc.trees(sentIdx), chunk, goldDoc.corefChunks(sentIdx)));
        totalPred += predChunks.size;
        predChunks.foreach(chunk => totalPredByLabel.incrementCount(chunk.label, 1.0));
        totalGold += goldChunks.size;
        goldChunks.foreach(chunk => totalGoldByLabel.incrementCount(chunk.label, 1.0));
        for (predChunk <- predChunks) {
          if (goldChunks.contains(predChunk)) {
            correct += 1;
            correctByLabel.incrementCount(predChunk.label, 1.0)
          }
          for (goldChunk <- goldChunks) {
            var correctSameHeadUsed = false;
            var oracleCorrectUsed = false;
            var oracleCorrectSameHeadUsed = false;
            if (!correctSameHeadUsed && goldDoc.trees(sentIdx).getSpanHead(goldChunk.start, goldChunk.end) == goldDoc.trees(sentIdx).getSpanHead(predChunk.start, predChunk.end) &&
                goldChunk.label == predChunk.label) {
              correctSameHeadUsed = true;
              correctSameHead += 1;
            }
            if (!oracleCorrectUsed && goldChunk.start == predChunk.start && goldChunk.end == predChunk.end) {
              oracleCorrectUsed = true;
              oracleCorrect += 1;
            }
            if (!oracleCorrectSameHeadUsed && goldDoc.trees(sentIdx).getSpanHead(goldChunk.start, goldChunk.end) == goldDoc.trees(sentIdx).getSpanHead(predChunk.start, predChunk.end)) {
              oracleCorrectSameHeadUsed = true;
              oracleCorrectSameHead += 1;
            }
          }
        }
      }
    }
    Logger.logss("Results: " + GUtil.renderPRF1(correct, totalPred, totalGold));
    for (tag <- totalGoldByLabel.keySet.asScala.toSeq.sorted) {
      Logger.logss("  Results for " + GUtil.padToK(tag, 11) + ": " + GUtil.renderPRF1(correctByLabel.getCount(tag).toInt, totalPredByLabel.getCount(tag).toInt, totalGoldByLabel.getCount(tag).toInt));
    }
    Logger.logss("Results, same head: " + GUtil.renderPRF1(correctSameHead, totalPred, totalGold));
    Logger.logss("Relabeling oracle: " + GUtil.renderPRF1(oracleCorrect, totalPred, totalGold));
    Logger.logss("Relabeling oracle, same head: " + GUtil.renderPRF1(oracleCorrectSameHead, totalPred, totalGold));
  }
  
  def evaluateChunksBySent(allGoldChunksBySent: Iterable[Seq[Chunk[String]]], allPredChunksBySent: Iterable[Seq[Chunk[String]]], printFineGrainedResults: Boolean = true, ignoreCase: Boolean = false) {
    var correct = 0;
    val correctByLabel = new Counter[String];
    var totalPred = 0;
    val totalPredByLabel = new Counter[String];
    var totalGold = 0;
    val totalGoldByLabel = new Counter[String];
    for ((goldChunksRaw, predChunksRaw) <- allGoldChunksBySent.zip(allPredChunksBySent)) {
      val goldChunks = if (ignoreCase) goldChunksRaw.map(chunk => new Chunk(chunk.start, chunk.end, chunk.label.toLowerCase)); else goldChunksRaw;
      val predChunks = if (ignoreCase) predChunksRaw.map(chunk => new Chunk(chunk.start, chunk.end, chunk.label.toLowerCase)); else predChunksRaw;
      totalPred += predChunks.size;
      predChunks.foreach(chunk => totalPredByLabel.incrementCount(chunk.label, 1.0));
      totalGold += goldChunks.size;
      goldChunks.foreach(chunk => totalGoldByLabel.incrementCount(chunk.label, 1.0));
      for (predChunk <- predChunks) {
        if (goldChunks.contains(predChunk)) {
          correct += 1;
          correctByLabel.incrementCount(predChunk.label, 1.0)
        }
//        for (goldChunk <- goldChunks) {
//          if (goldChunk.start == predChunk.start && goldChunk.end == predChunk.end && goldChunk.label.toString == predChunk.label.toString) {
//            correct += 1;
//            correctByLabel.incrementCount(predChunk.label, 1.0)
//          }
//        }
      }
    }
    Logger.logss("Results: " + GUtil.renderPRF1(correct, totalPred, totalGold));
    if (printFineGrainedResults) {
      for (tag <- totalGoldByLabel.keySet.asScala.toSeq.sorted) {
        Logger.logss("  Results for " + GUtil.padToK(tag, 11) + ": " + GUtil.renderPRF1(correctByLabel.getCount(tag).toInt, totalPredByLabel.getCount(tag).toInt, totalGoldByLabel.getCount(tag).toInt));
      }
    }
  }
  
  def basicEvaluateChunksBySentGetSuffStats(allGoldChunksBySent: Iterable[Seq[Chunk[String]]], allPredChunksBySent: Iterable[Seq[Chunk[String]]]): (Int, Int, Int) = {
    var correct = 0;
    var totalPred = 0;
    var totalGold = 0;
    for ((goldChunks, predChunks) <- allGoldChunksBySent.zip(allPredChunksBySent)) {
      totalPred += predChunks.size;
      totalGold += goldChunks.size;
      for (predChunk <- predChunks) {
        if (goldChunks.contains(predChunk)) {
          correct += 1;
        }
      }
    }
    (correct, totalPred, totalGold);
  }
  
  def readEvaluateIllinoisNEROutput(file: String, sentences: Seq[Seq[String]], allGoldChunksBySent: Iterable[Seq[Chunk[String]]]) {
    val chunks = readIllinoisNEROutput(file, sentences);
    evaluateChunksBySent(allGoldChunksBySent, chunks);
  }
  
  def readIllinoisNEROutput(file: String, sentences: Seq[Seq[String]]) = {
    val text = IOUtils.readLinesHard(file).asScala;
    val allText = text.reduce(_ + "\n" + _);
    val tokenizedText = allText.split("\\s+");
    Logger.logss("Num sentences: " + sentences.size);
    Logger.logss("Gold num tokens: " + sentences.map(_.size).reduce(_ + _));
    Logger.logss("Pred num tokens (including NER markers): " + tokenizedText.size);
    
    var currIdx = 0;
    val allChunks = new ArrayBuffer[Seq[Chunk[String]]];
    for (i <- 0 until sentences.size) {
//      if (i % 100 == 0) {
//        Logger.logs("Sentence " + i);
//      }
      val chunksCurrSent = new ArrayBuffer[Chunk[String]]
      var currNELabel = "";
      var currNEStart = -1;
      var numRealTokensConsumed = 0;
      while (numRealTokensConsumed < sentences(i).size) {
        if (tokenizedText(currIdx).startsWith("[") && tokenizedText(currIdx) != "[") {
          currNELabel = tokenizedText(currIdx).substring(1);
          currNEStart = numRealTokensConsumed;
        } else if (tokenizedText(currIdx).endsWith("]")) {
          chunksCurrSent += new Chunk[String](currNEStart, numRealTokensConsumed + 1, currNELabel);
          numRealTokensConsumed += 1;
        } else {
          val currPredWord = tokenizedText(currIdx);
          val currGoldWord = sentences(i)(numRealTokensConsumed);
          if (currPredWord != "(" && currPredWord != ")" && currPredWord != currGoldWord) {
            Logger.logss("Mismatch on sentence " + i + ": " + currPredWord + " " + currGoldWord + " " + sentences(i));
            System.exit(0);
          }
          numRealTokensConsumed += 1;
        }
        currIdx += 1;
      }
      allChunks += chunksCurrSent;
    }
    Logger.logss(tokenizedText.size + " " + currIdx);
    allChunks;
  }
  
  def readIllinoisNEROutputSoft(file: String) = {
    val output = IOUtils.readLinesHard(file).asScala;
    val tokenizedText = output.map(_.split("\\s+"));
    
    val allChunks = new ArrayBuffer[Seq[Chunk[String]]];
    for (i <- 0 until tokenizedText.size) {
//      if (i % 100 == 0) {
//        Logger.logs("Sentence " + i);
//      }
      val chunksCurrSent = new ArrayBuffer[Chunk[String]]
      var currNELabel = "";
      var currNEStart = -1;
      var numRealTokensConsumed = 0;
      for (tokIdx <- 0 until tokenizedText(i).size) {
        if (tokenizedText(i)(tokIdx).startsWith("[") && tokenizedText(i)(tokIdx) != "[") {
          currNELabel = tokenizedText(i)(tokIdx).substring(1);
          currNEStart = numRealTokensConsumed;
        } else if (tokenizedText(i)(tokIdx).endsWith("]")) {
          chunksCurrSent += new Chunk[String](currNEStart, numRealTokensConsumed + 1, currNELabel);
          numRealTokensConsumed += 1;
        } else {
          numRealTokensConsumed += 1;
        }
      }
      allChunks += chunksCurrSent;
    }
    allChunks;
  }
  
  def writeIllinoisNEROutput(file: String, allSentences: Seq[Seq[String]], allChunks: Seq[Seq[Chunk[String]]]) {
    val writer = IOUtils.openOutHard(file);
    for ((sentence, chunks) <- allSentences.zip(allChunks)) {
      val chunkStartsMap = chunks.map(chunk => chunk.start -> chunk).toMap;
      val chunkEndsMap = chunks.map(chunk => (chunk.end - 1) -> chunk).toMap;
      var line = new StringBuilder();
      for (idx <- 0 until sentence.size) {
        if (chunkStartsMap.contains(idx)) {
          line = line.append("[" + chunkStartsMap(idx).label + " ");
        }
        line = line.append(sentence(idx))
        if (chunkEndsMap.contains(idx)) {
          line = line.append("]")
        } 
        line = line.append(" ");
      }
      writer.println(line.toString);
    }
    writer.close();
  }
  
  def evaluateOnConll2011(docs: Seq[JointDoc], predChunks: Seq[Seq[Seq[Chunk[String]]]], conll2011DocNames: Set[String], outputPath: String) {
    Logger.logss("Restricting to CoNLL 2011 documents (either dev or test): " + conll2011DocNames);
    val allGoldChunks = new ArrayBuffer[Seq[Chunk[String]]];
    val allPredChunks = new ArrayBuffer[Seq[Chunk[String]]];
    val allSents = new ArrayBuffer[Seq[String]];
    val uidsKept = new HashSet[UID];
    var numKept = 0;
    for ((doc, chunks) <- docs.zip(predChunks)) {
      if (conll2011DocNames.contains(doc.rawDoc.docID.substring(doc.rawDoc.docID.lastIndexOf("/") + 1))) {
        allGoldChunks ++= doc.goldNERChunks;
        allPredChunks ++= chunks;
        allSents ++= doc.rawDoc.words;
        uidsKept += doc.rawDoc.uid;
        numKept += 1;
      } else {
        Logger.logss("Discarding " + doc.rawDoc.docID);
      }
    }
    Logger.logss("Kept " + numKept + " docs out of " + docs.size + ": " + uidsKept);
    evaluateChunksBySent(allGoldChunks, allPredChunks);
    if (outputPath != "") {
      NEEvaluator.writeIllinoisNEROutput(outputPath, allSents, allPredChunks);
    }
  }
  
  def main(args: Array[String]) {
    LightRunner.initializeOutput(NEEvaluator.getClass);
    // Illinois evaluation
//    val goldConllDocsDev2011 = CorefSystem.loadRawConllDocs("data/ontonotes-conll/dev", -1, true);
//    readEvaluateIllinoisNEROutput("data/ner-output/conll-2011-dev.tagged", goldConllDocsDev2011.flatMap(_.words), goldConllDocsDev2011.flatMap(_.nerChunks));
//    val goldConllDocsTest2011 = CorefSystem.loadRawConllDocs("data/ontonotes-conll/test", -1, true);
//    readEvaluateIllinoisNEROutput("data/ner-output/conll-2011-test.tagged", goldConllDocsTest2011.flatMap(_.words), goldConllDocsTest2011.flatMap(_.nerChunks));
      
    
    Logger.logss("DEV 2011");
    val goldConllDocsDev2011 = ConllDocReader.loadRawConllDocsWithSuffix("data/ontonotes-conll/dev", -1, "gold_conll");
    readEvaluateIllinoisNEROutput("data/ner-output/conll-2011-dev-downloadedmodel.tagged", goldConllDocsDev2011.flatMap(_.words), goldConllDocsDev2011.flatMap(_.nerChunks));
    readEvaluateIllinoisNEROutput("data/ner-output/conll-2011-dev-retrained.tagged", goldConllDocsDev2011.flatMap(_.words), goldConllDocsDev2011.flatMap(_.nerChunks));
    Logger.logss("\n\nTEST 2011");
    val goldConllDocsTest2011 = ConllDocReader.loadRawConllDocsWithSuffix("data/ontonotes-conll/test", -1, "gold_conll");
    readEvaluateIllinoisNEROutput("data/ner-output/conll-2011-test-downloadedmodel.tagged", goldConllDocsTest2011.flatMap(_.words), goldConllDocsTest2011.flatMap(_.nerChunks));
    readEvaluateIllinoisNEROutput("data/ner-output/conll-2011-test-retrained.tagged", goldConllDocsTest2011.flatMap(_.words), goldConllDocsTest2011.flatMap(_.nerChunks));
    
    Logger.logss("\n\nDEV 2012");
    val goldConllDocsDev2012 = ConllDocReader.loadRawConllDocsWithSuffix("data/conll-2012-en/dev", -1, "gold_conll");
    readEvaluateIllinoisNEROutput("data/ner-output/conll-2012-dev-downloadedmodel.tagged", goldConllDocsDev2012.flatMap(_.words), goldConllDocsDev2012.flatMap(_.nerChunks));
    readEvaluateIllinoisNEROutput("data/ner-output/conll-2012-dev-retrained.tagged", goldConllDocsDev2012.flatMap(_.words), goldConllDocsDev2012.flatMap(_.nerChunks));
    Logger.logss("\n\nTEST 2012");
    val goldConllDocsTest2012 = ConllDocReader.loadRawConllDocsWithSuffix("data/ontonotes-conll/test", -1, "gold_conll");
    readEvaluateIllinoisNEROutput("data/ner-output/conll-2012-test-downloadedmodel.tagged", goldConllDocsTest2012.flatMap(_.words), goldConllDocsTest2012.flatMap(_.nerChunks));
    readEvaluateIllinoisNEROutput("data/ner-output/conll-2012-test-retrained.tagged", goldConllDocsTest2012.flatMap(_.words), goldConllDocsTest2012.flatMap(_.nerChunks));
    
//    val docs = CorefSystem.loadCorefDocs("data/conll-2012-en/dev", -1, None, true);
//    writeIllinoisNEROutput("data/ner-output/test.tagged", docs.flatMap(_.rawDoc.words), docs.flatMap(_.rawDoc.nerChunks));
    
//    val docs = CorefSystem.loadCorefDocs("data/dev", -1, None, false);
//    val goldConllDocs = CorefSystem.loadRawConllDocs("data/dev", -1, true);
//    val docs = CorefSystem.loadCorefDocs("data/conll-2012-en/dev", -1, None, false);
//    val predConllDocs = CorefSystem.loadRawConllDocs("data/dev", -1, false);
//    val goldConllDocs = CorefSystem.loadRawConllDocs("data/dev", -1, true);
//    val predConllDocs = CorefSystem.loadRawConllDocs("data/conll-2012-en/dev", -1, false);
//    val goldConllDocs = CorefSystem.loadRawConllDocs("data/conll-2012-en/dev", -1, true);
//    val predConllDocs = CorefSystem.loadRawConllDocs("data/conll-2012-en/test", -1, false);
//    val goldConllDocs = CorefSystem.loadRawConllDocs("data/conll-2012-en/test", -1, true);
//    NEEvaluator.evaluate(goldConllDocs, predConllDocs)
    LightRunner.finalizeOutput();
  }
}
