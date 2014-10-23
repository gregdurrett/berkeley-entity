package edu.berkeley.nlp.entity.coref
import java.io.File
import java.io.PrintWriter
import java.util.regex.Pattern

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.sys.process.stringSeqToProcess
import scala.sys.process.Process
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.Driver
import edu.berkeley.nlp.entity.ConllDoc
import edu.berkeley.nlp.entity.ConllDocWriter

class CorefConllScorer(val conllEvalScriptPath: String) {
  
  def renderFinalScore(conllDocs: Seq[ConllDoc], rawPredClusterings: Seq[OrderedClusteringBound], goldClusterings: Seq[OrderedClusteringBound]) = {
    val summary = score(conllDocs, rawPredClusterings, goldClusterings, true);
    CorefConllScorer.processConllString(summary, false);
  }
  
  def renderSuffStats(conllDoc: ConllDoc, rawPredClustering: OrderedClusteringBound, goldClustering: OrderedClusteringBound) = {
    val summary = score(Seq(conllDoc), Seq(rawPredClustering), Seq(goldClustering), false);
    CorefConllScorer.processConllString(summary, true);
  }
  
  def score(conllDocs: Seq[ConllDoc], rawPredClusterings: Seq[OrderedClusteringBound], goldClusterings: Seq[OrderedClusteringBound], saveTempFiles: Boolean) = {
    val predClusterings = if (Driver.doConllPostprocessing) rawPredClusterings.map(_.postprocessForConll()) else rawPredClusterings;
//    var predFile = File.createTempFile("temp", ".conll");
    val (predFile, goldFile) = if (Driver.conllOutputDir != "" && saveTempFiles) {
      val pFile = File.createTempFile("temp", ".conll", new File(Driver.conllOutputDir));
      val gFile = new File(pFile.getAbsolutePath() + "-gold");      
      Logger.logss("PRED FILE: " + pFile.getAbsolutePath());
      Logger.logss("GOLD FILE: " + gFile.getAbsolutePath());
      Logger.logss("To score, run:");
      Logger.logss("perl scorer.pl all " + gFile.getAbsolutePath() + " " + pFile.getAbsolutePath() + " none");
      (pFile, gFile);
    } else {
      val pFile = File.createTempFile("temp", ".conll");
      val gFile = new File(pFile.getAbsolutePath() + "-gold");
      pFile.deleteOnExit();
      gFile.deleteOnExit();
      (pFile, gFile);
    }
    val predWriter = new PrintWriter(predFile);
    val goldWriter = new PrintWriter(goldFile);
    for (i <- 0 until conllDocs.size) {
      ConllDocWriter.writeDoc(predWriter, conllDocs(i), predClusterings(i));
      ConllDocWriter.writeDoc(goldWriter, conllDocs(i), goldClusterings(i));
    }
    // Flush and close the buffers
    predWriter.close();
    goldWriter.close();
    Logger.logss("Running scoring program...");
    import edu.berkeley.nlp.entity.Driver;
// Build and run the process for the CoNLL eval script script
    import scala.sys.process._
    val output = Process(Seq(conllEvalScriptPath, "all", goldFile.getAbsolutePath(), predFile.getAbsolutePath(), "none")).lines;
    Logger.logss("Scoring program complete!");
    output.reduce(_ + "\n" + _);
  }
}

object CorefConllScorer {
  
  def processConllString(summary: String, renderSuffStats: Boolean) = {
    val pr = Pattern.compile("Coreference:.*\\(([0-9.]+) / ([0-9.]+)\\).*\\(([0-9.]+) / ([0-9.]+)\\)");
    val prMatcher = pr.matcher(summary);
    var prCount = 0;
    var (mucPNum, mucPDenom, mucRNum, mucRDenom) = (0.0, 0.0, 0.0, 0.0);
    var (bcubPNum, bcubPDenom, bcubRNum, bcubRDenom) = (0.0, 0.0, 0.0, 0.0);
    var (ceafePNum, ceafePDenom, ceafeRNum, ceafeRDenom) = (0.0, 0.0, 0.0, 0.0);
    // Four matches: MUC, B-cubed, CEAFM, CEAFE (BLANC doesn't match because of different formatting)
    while (prMatcher.find()) {
      if (prCount == 0) {
        mucRNum = prMatcher.group(1).toDouble;
        mucRDenom = prMatcher.group(2).toDouble;
        mucPNum = prMatcher.group(3).toDouble;
        mucPDenom = prMatcher.group(4).toDouble;
      }
      if (prCount == 1) {
        bcubRNum = prMatcher.group(1).toDouble;
        bcubRDenom = prMatcher.group(2).toDouble;
        bcubPNum = prMatcher.group(3).toDouble;
        bcubPDenom = prMatcher.group(4).toDouble;
      }
      if (prCount == 3) {
        ceafeRNum = prMatcher.group(1).toDouble;
        ceafeRDenom = prMatcher.group(2).toDouble;
        ceafePNum = prMatcher.group(3).toDouble;
        ceafePDenom = prMatcher.group(4).toDouble;
      }
      prCount += 1;
    }
    val mucP = mucPNum/mucPDenom * 100.0;
    val mucR = mucRNum/mucRDenom * 100.0;
    val mucF = 2 * mucP * mucR/(mucP + mucR);
    val bcubP = bcubPNum/bcubPDenom * 100.0;
    val bcubR = bcubRNum/bcubRDenom * 100.0;
    val bcubF = 2 * bcubP * bcubR/(bcubP + bcubR);
    val ceafeP = ceafePNum/ceafePDenom * 100.0;
    val ceafeR = ceafeRNum/ceafeRDenom * 100.0;
    val ceafeF = 2 * ceafeP * ceafeR/(ceafeP + ceafeR);
    val avg = (mucF + bcubF + ceafeF)/3.0;
    if (renderSuffStats) {
      "MUC/BCUB/CEAFE P/R N/D:\t" + mucPNum + "\t" + mucPDenom + "\t" + mucRNum + "\t" + mucRDenom + "\t" + bcubPNum + "\t" + bcubPDenom + "\t" + bcubRNum + "\t" + bcubRDenom + "\t" + ceafePNum + "\t" + ceafePDenom + "\t" + ceafeRNum + "\t" +ceafeRDenom;
    } else {
      "MUC P-R-F1, BCUB P-R-F1, CEAFE P-R-F1, Average:\t" + fmt(mucP) + "\t" + fmt(mucR) + "\t" + fmt(mucF) + "\t" + fmt(bcubP) + "\t" + fmt(bcubR) + "\t" + fmt(bcubF) + "\t" + fmt(ceafeP) + "\t" + fmt(ceafeR) + "\t" + fmt(ceafeF) + "\t" + fmt(avg) + "\n" +
             "MUC = " + fmt(mucF) + ", BCUB = " + fmt(bcubF) + ", CEAFE = " + fmt(ceafeF) + ", AVG = " + fmt(avg);
    }
  }
  
  private def fmt(d: Double): String = {
    val str = "" + (d + 0.005);
    str.substring(0, Math.min(str.length(), str.indexOf(".") + 3));
  }
  
  def main(args: Array[String]) {
import scala.sys.process._
    val cmd = Seq("ls", "clean-data/");
    println(cmd.lines.toIndexedSeq);
  }
  
}
