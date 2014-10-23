package edu.berkeley.nlp.entity.sig

import edu.berkeley.nlp.entity.ner.NEEvaluator
import edu.berkeley.nlp.entity.coref.CorefSystem
import edu.berkeley.nlp.entity.ConllDocReader
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.entity.Chunk

object BootstrapDriverNER {

  def main(args: Array[String]) {
    val goldPath = args(0);
    val worseFilePath = args(1);
    val betterFilePath = args(2);
    val goldDocs = ConllDocReader.loadRawConllDocsWithSuffix(goldPath, -1, "gold_conll", Language.ENGLISH);
//    val sentences = goldDocs.flatMap(_.words)
//    val worseChunks = NEEvaluator.readIllinoisNEROutput(worseFilePath, sentences)
//    val betterChunks = NEEvaluator.readIllinoisNEROutput(betterFilePath, sentences)
    val worseChunks = NEEvaluator.readIllinoisNEROutputSoft(worseFilePath)
    val betterChunks = NEEvaluator.readIllinoisNEROutputSoft(betterFilePath)
    val goldChunks = goldDocs.flatMap(_.nerChunks);
    
    val worseSuffStats = convertToSuffStats(goldChunks, worseChunks);
    val betterSuffStats = convertToSuffStats(goldChunks, betterChunks);
    
    BootstrapDriver.printSimpleBootstrapPValue(worseSuffStats, betterSuffStats, new F1Computer(0, 1, 0, 2))
  }
  
  def convertToSuffStats(goldChunks: Seq[Seq[Chunk[String]]], predChunks: Seq[Seq[Chunk[String]]]): Seq[Seq[Double]] = {
    for (i <- 0 until goldChunks.size) yield {
      Seq(predChunks(i).filter(chunk => goldChunks(i).contains(chunk)).size.toDouble, predChunks(i).size.toDouble, goldChunks(i).size.toDouble);
    }
  }
}
