package edu.berkeley.nlp.entity.wiki

import edu.berkeley.nlp.futile.fig.basic.IOUtils
import scala.collection.JavaConverters._
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.GUtil
import java.io.File
import edu.berkeley.nlp.futile.util.Counter
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.futile.LightRunner
import edu.berkeley.nlp.entity.sig.BootstrapDriver
import edu.berkeley.nlp.entity.sig.AccuracyComputer

object FahrniOutputAnalyzer {

  def main(args: Array[String]) {
    LightRunner.initializeOutput(FahrniOutputAnalyzer.getClass);
//    processLines(filterFahrniForDevSet(IOUtils.readLinesHard("data/ace05/fahrni-output/baseline.output").asScala), false);
//    processLines(filterFahrniForDevSet(IOUtils.readLinesHard("data/ace05/fahrni-output/best.output").asScala), false);
//    Logger.logss("DETACHED");
//    processLines(IOUtils.readLinesHard("data/ace05/my-output/dev-detached-wikiout.txt").asScala, true);
//    Logger.logss("JOINT");
//    processLines(IOUtils.readLinesHard("data/ace05/my-output/dev-joint-links-wikiout.txt").asScala, true);
//    Logger.logss("DETACHED-FAHRNI");
//    processLines(IOUtils.readLinesHard("data/ace05/my-output/dev-detached-fahrni-wikiout.txt").asScala, true);
//    Logger.logss("JOINT-FAHRNI");
//    processLines(IOUtils.readLinesHard("data/ace05/my-output/dev-joint-fahrni-wikiout.txt").asScala, true);
//    Logger.logss("DETACHED-MEMORIZING");
//    processLines(IOUtils.readLinesHard("data/ace05/my-output/dev-detached-mem-wikiout.txt").asScala, true);
//    Logger.logss("JOINT-MEMORIZING");
//    processLines(IOUtils.readLinesHard("data/ace05/my-output/dev-joint-links-mem-wikiout.txt").asScala, true);
//    whereDoTheyDiffer(formMapMe("data/ace05/dev-detached-wikiout.txt"),
//                      formMapFahrni("data/ace05/fahrni-output/baseline.output"));
    
//    diffOutput("data/ace05/my-output/dev-joint-aux-wikiout.txt", "data/ace05/fahrni-output/best.output", false)
//    diffOutput("data/ace05/my-output/test-detached-choosing.txt", "data/ace05/fahrni-output/best.output", true)
    
    val bestDetachedTestRun = "data/ace05-old/my-output/test-detached-lqw7.txt";
    val bestJointTestRun = "data/ace05-old/my-output/test-joint-lqw7.txt";
    
//    diffOutput(bestJointTestRun, "data/ace05-old/fahrni-output/baseline.output", true, false)
    diffOutput(bestJointTestRun, "data/ace05-old/fahrni-output/best.output", true, false)
    Logger.logss("Sig test (base vs. joint)");
    val testDetachedMap = formMapMe(bestDetachedTestRun, true);
    val testJointMap = formMapMe(bestJointTestRun, true);
    sigTest(testDetachedMap, testJointMap);
//    processLines(IOUtils.readLinesHard(bestDetachedTestRun).asScala, true)
//    processLines(IOUtils.readLinesHard(outFile).asScala, true)
    LightRunner.finalizeOutput;
  }
  
  def diffOutput(myOutputPath: String, fahrniOutputPath: String, isTest: Boolean, doPrint: Boolean) {
//    val fahrniLines = filterFahrniForDevSet(IOUtils.readLinesHard("data/ace05/fahrni-output/baseline.output").asScala);
//    val lines = IOUtils.readLinesHard("data/ace05/my-output/dev-joint-links-mem-wikiout.txt").asScala
    val myMap = formMapMe(myOutputPath, isTest);
    val fahrniMap = formMapFahrni(fahrniOutputPath, isTest);
//    val linesMe = IOUtils.readLinesHard("data/ace05/my-output/dev-joint-links-mem-wikiout.txt").asScala.map(_.split("\\s+")).filter(_.size == 4).map(_(3));
    val counts = new Counter[String];
    val restrictedFahrniMap = new HashMap[String,String];
    for (key <- myMap.keySet.toSeq.sorted) {
      val myResult = myMap(key).replace("_UNK", "");
      if (!fahrniMap.contains(key)) {
        Logger.logss("Fahrni does not contain " + key);
      } else {
        val fahrniResult = fahrniMap(key);
        restrictedFahrniMap += key -> fahrniResult
        if (doPrint) Logger.logss(key + "\t" + myResult + "\t" + fahrniResult);
        counts.incrementCount(myResult + "-" + fahrniResult, 1.0);
      }
    }
    Logger.logss("Intersection contains " + restrictedFahrniMap.size)
    val restrictedFahrniCounts = new Counter[String]
    restrictedFahrniMap.foreach(entry => restrictedFahrniCounts.incrementCount(entry._2, 1.0));
    Logger.logss("Fahrni restricted to my subset");
    WikificationEvaluator.displayResultsAllExs(restrictedFahrniCounts, false);
    Logger.logss("Sig test (Fahrni better)");
    sigTest(myMap, restrictedFahrniMap.toMap);
    Logger.logss("Sig test (me better)");
    sigTest(restrictedFahrniMap.toMap, myMap);
    Logger.logss(counts);
  }
  
  def formMap(lines: Seq[String], isFahrni: Boolean) = {
    val linesSplit = lines.map(_.split("\\s+"));
    linesSplit.map(splitLine => {
      val fileName = if (isFahrni) splitLine(0).dropRight(4) else splitLine(0);
      fileName + ":" + splitLine.slice(1, 3).map(_.toLowerCase).reduce(_ + ":" + _) -> splitLine(3);
    }).toMap;
  }
  
  def formMapMe(fileName: String, isTest: Boolean) = {
    formMap(filterMeForSet(IOUtils.readLinesHard(fileName).asScala, isTest), false);
  }
  
  def formMapFahrni(fileName: String, isTest: Boolean) = {
    formMap(filterFahrniForSet(IOUtils.readLinesHard(fileName).asScala, isTest), true);
  }
  
  def whereDoTheyDiffer(myLines: Map[String,String], fahrniLines: Map[String,String]) {
    Logger.logss(myLines.keySet.size + " " + fahrniLines.keySet.size);
    if (myLines.keySet.size > fahrniLines.keySet.size) {
      (myLines.keySet -- fahrniLines.keySet).foreach(line => Logger.logss(line));
    }
  }
  
  def filterMeForSet(lines: Seq[String], isTest: Boolean) = {
    val devSetNames = new File(if (isTest) "data/ace05/test" else "data/ace05/dev").listFiles().map(_.getName().toLowerCase).toSet;
    // Drop right to get rid of ".sgm"
    lines.filter(line => devSetNames.contains(line.split("\\s+")(0).toLowerCase));
  }
  
  def filterFahrniForSet(lines: Seq[String], isTest: Boolean) = {
    val devSetNames = new File(if (isTest) "data/ace05/test" else "data/ace05/dev").listFiles().map(_.getName().toLowerCase).toSet;
    // Drop right to get rid of ".sgm"
    lines.filter(line => devSetNames.contains(line.split("\\s+")(0).toLowerCase.dropRight(4)));
  }
  
  def processLines(lines: Seq[String], doUnks: Boolean) {
    val results = lines.map(_.split("\\s+")).filter(_.size == 4).map(_(3));
    Logger.logss(lines.size + " dev set lines, " + results.size + " lines kept after filtering");
    val counter = new Counter[String];
    results.foreach(counter.incrementCount(_, 1.0));
    WikificationEvaluator.displayResultsAllExs(counter, doUnks);
  }
  
  def sigTest(linesWorse: Map[String,String], linesBetter: Map[String,String]) {
    // Second position is always 1 just to increment the denominator
    if (!linesWorse.keySet.sameElements(linesBetter.keySet)) {
      throw new RuntimeException("Unequal key sets: " + linesWorse.size + " " + linesBetter.size);
    }
    // Token-level sig test: basically says everything ever is significant
    val worseSuffStats = linesWorse.keySet.toSeq.sorted.map(key => Seq(if (linesWorse(key).startsWith("c")) 1.0 else 0.0, 1.0));
    val betterSuffStats = linesBetter.keySet.toSeq.sorted.map(key => Seq(if (linesBetter(key).startsWith("c")) 1.0 else 0.0, 1.0));
//    val worseSuffStats = getDocLevelSuffStatsForSigTest(linesWorse);
//    val betterSuffStats = getDocLevelSuffStatsForSigTest(linesBetter);
    BootstrapDriver.printSimpleBootstrapPValue(worseSuffStats, betterSuffStats, new AccuracyComputer);
  }
  
  def getDocLevelSuffStatsForSigTest(lines: Map[String,String]): Seq[Seq[Double]] = {
    val docNames = lines.keySet.map(str => str.substring(0, str.indexOf(":"))).toSeq.sorted;
    val suffStats = docNames.map(docName => {
      val thisDocKeys = lines.keySet.filter(_.startsWith(docName)).toSeq;
      val numCorrect = thisDocKeys.map(key => {
        if (lines(key).startsWith("c")) 1.0 else 0.0
      }).foldLeft(0.0)(_ + _);
      Seq(numCorrect, thisDocKeys.size);
    });
    Logger.logss(suffStats);
    suffStats
  }
}
