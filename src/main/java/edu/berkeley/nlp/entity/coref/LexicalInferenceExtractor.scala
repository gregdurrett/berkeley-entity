package edu.berkeley.nlp.entity.coref

import edu.berkeley.nlp.entity.Driver
import edu.berkeley.nlp.entity.sem.QueryCountsBundle
import edu.berkeley.nlp.entity.sem.SemClasser
import edu.berkeley.nlp.entity.sem.BasicWordNetSemClasser
import edu.berkeley.nlp.entity.sem.MentionFilter
import edu.berkeley.nlp.futile.util.Logger
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.futile.fig.basic.IOUtils

case class LexicalInferenceExample(val firstType: String,
                                   val firstText: String,
                                   val firstShort: String,
                                   val secondType: String,
                                   val secondText: String,
                                   val secondShort: String,
                                   val isCoref: Boolean) {
  override def toString() = firstType + "\t" + firstText + "\t" + firstShort + "\t" + secondType + "\t" + secondText + "\t" + secondShort + "\t" + isCoref
}

object LexicalInferenceExtractor {
  
  def extractShortLabel(ment: Mention) = {
    if (ment.mentionType == MentionType.PROPER) {
      val matchingChunks = ment.rawDoc.nerChunks(ment.sentIdx).filter(chunk => chunk.start <= ment.headIdx && ment.headIdx < chunk.end);
      if (matchingChunks.size >= 1) {
        val chunk = matchingChunks.sortBy(chunk => chunk.end - chunk.start).head
        ment.rawDoc.words(ment.sentIdx).slice(chunk.start, chunk.end).reduce(_ + " " + _)
      } else {
        ment.headStringLc
      }
    } else {
      ment.headStringLc
    }
  }
  
  def makeExample(docGraph: DocumentGraph, currIdx: Int, antIdx: Int, isCoref: Boolean) = {
    val curr = docGraph.getMention(currIdx)
    val ant = docGraph.getMention(antIdx)
    require(curr.mentionType == MentionType.PROPER || curr.mentionType == MentionType.NOMINAL)
    require(ant.mentionType == MentionType.PROPER || ant.mentionType == MentionType.NOMINAL)
    new LexicalInferenceExample(ant.mentionType.toString, ant.spanToString, extractShortLabel(ant), curr.mentionType.toString, curr.spanToString, extractShortLabel(curr), isCoref)
  }

  def writeExamples(docs: Seq[DocumentGraph], corefPruner: CorefPruner, outFile: String) {
    val posExs = new HashMap[(String,String),LexicalInferenceExample]
    val negExs = new HashMap[(String,String),LexicalInferenceExample]
    var posCount = 0
    var negCount = 0
    var docCount = 0
    for (doc <- docs) {
      if (docCount % 100 == 0) {
        Logger.logss("On doc " + docCount + " / " + docs.size)
      }
      corefPruner.prune(doc)
      for (i <- 0 until doc.size) {
        val curr = doc.getMention(i)
        // Extract positive examples
        if (MentionFilter.unaryIsCanonicalHardestReferringCaseNoPruning(doc, i)) {
          for (j <- doc.getGoldAntecedentsNoPruning(i)) {
            if (MentionFilter.binaryIsHardestReferringCase(doc, i, j)) {
//              Logger.logss("POSITIVE: " + doc.getMention(j).headString + " " + curr.headString)
              val ex = makeExample(doc, i, j, true)
              posExs += (ex.firstShort -> ex.secondShort) -> ex
              posCount += 1
            }
          }
        }
        // Extract negative examples
        if (!curr.mentionType.isClosedClass()) {
          for (j <- 0 until i) {
            val prev = doc.getMention(j)
            if (!doc.isGoldCurrentPruning(i, j) && !doc.isPruned(i, j) && !doc.getMention(j).mentionType.isClosedClass() && curr.headStringLc != prev.headStringLc) {
//              Logger.logss("NEGATIVE: " + prev.headString + " == " + curr.headString + "; " + prev.spanToString + " == " + curr.spanToString)
              val ex = makeExample(doc, i, j, false)
              negExs += (ex.firstShort -> ex.secondShort) -> ex
              negCount += 1
            }
          }
        }
      }
      docCount += 1
    }
    Logger.logss(posCount + " total pos examples, cut down to " + posExs.size + " with uniques; " + negCount + " total neg examples, cut down to " + negExs.size + " with uniques")
    val writer = IOUtils.openOutHard(outFile)
    for (ex <- posExs.values) {
      writer.println(ex.toString())
    }
    for (ex <- negExs.values) {
      writer.println(ex.toString())
    }
    writer.close
  }
  
  def main(args: Array[String]) {
    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(Driver.numberGenderDataPath);
    val corefPruner = CorefPruner.buildPruner("models:models-exper/corefpruner-onto.ser.gz:-2")
    
//    val trainDocs = CorefSystem.loadCorefDocs("../zcc11-new/data/conll-2012-en/train", -1, Driver.corefDocSuffix, Some(numberGenderComputer));
//    val trainDocGraphs = trainDocs.map(new DocumentGraph(_, true));
//    writeExamples(trainDocGraphs, corefPruner, "data/lexinf-train.txt")
    
    val devDocs = CorefSystem.loadCorefDocs("../zcc11-new/data/conll-2012-en/dev", -1, Driver.corefDocSuffix, Some(numberGenderComputer));
    val devDocGraphs = devDocs.map(new DocumentGraph(_, true));
    writeExamples(devDocGraphs, corefPruner, "data/extraction-testing.txt")
//    
//    val testDocs = CorefSystem.loadCorefDocs("../zcc11-new/data/conll-2012-en/test", -1, Driver.corefDocSuffix, Some(numberGenderComputer));
//    val testDocGraphs = testDocs.map(new DocumentGraph(_, true));
//    writeExamples(testDocGraphs, corefPruner, "data/lexinf-test.txt")
  }
}