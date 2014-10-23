package edu.berkeley.nlp.entity.sig
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.futile.util.Logger

object BootstrapDriver {

  def main(args: Array[String]) {
    // Each argument is a log file consisting of tab-separated sufficient statistics 
    require(args.size % 3 == 0);
    val allSuffStats = new ArrayBuffer[Seq[String]]();
    for (i <- 0 until args.size by 3) {
      allSuffStats += extractSuffStats(args(i), args(i+1).toInt, args(i+2).toInt);
    }
    val processedSuffStats: ArrayBuffer[Seq[Seq[Double]]] = allSuffStats.map(_.map(_.split("\t").toSeq.drop(1).map(_.toDouble)));
    val worseSuffStats = processedSuffStats.slice(0, processedSuffStats.size/2);
    val betterSuffStats = processedSuffStats.slice(processedSuffStats.size/2, processedSuffStats.size);
    val flattenedWorseSuffStats = worseSuffStats.flatMap(lst => lst);
    val flattenedBetterSuffStats = betterSuffStats.flatMap(lst => lst);
    val flattenedAllSuffStats = processedSuffStats.flatMap(lst => lst);
    
    
    val mucComputer = new F1Computer(0, 1, 2, 3);
    val bcubComputer = new F1Computer(4, 5, 6, 7);
    val ceafeComputer = new F1Computer(8, 9, 10, 11);
    
    val mucWorse = mucComputer.computeMetricFull(flattenedWorseSuffStats);
    val mucBetter = mucComputer.computeMetricFull(flattenedBetterSuffStats);
    val bcubWorse = bcubComputer.computeMetricFull(flattenedWorseSuffStats);
    val bcubBetter = bcubComputer.computeMetricFull(flattenedBetterSuffStats);
    val ceafeWorse = ceafeComputer.computeMetricFull(flattenedWorseSuffStats);
    val ceafeBetter = ceafeComputer.computeMetricFull(flattenedBetterSuffStats);
    
    val mucTotal = mucComputer.computeMetricFull(flattenedAllSuffStats);
    val bcubTotal = bcubComputer.computeMetricFull(flattenedAllSuffStats);
    val ceafeTotal = ceafeComputer.computeMetricFull(flattenedAllSuffStats);
    
    println("MUC, BCUB, CEAFE, AVG");
    println("Total: " + fmtMetricValues(mucTotal) + "F1" + fmtMetricValues(bcubTotal) + "F1" + fmtMetricValues(ceafeTotal) + "F1 & " + (mucTotal(2) + bcubTotal(2) + ceafeTotal(2))/3.0);
    println("Worse (if sigtesting): " + fmtMetricValues(mucWorse) + "F1" + fmtMetricValues(bcubWorse) + "F1" + fmtMetricValues(ceafeWorse) + "F1 & " + (mucWorse(2) + bcubWorse(2) + ceafeWorse(2))/3.0);
    println("Better (if sigtesting): " + fmtMetricValues(mucBetter) + "F1" + fmtMetricValues(bcubBetter) + "F1" + fmtMetricValues(ceafeBetter) + "F1 & " + (mucBetter(2) + bcubBetter(2) + ceafeBetter(2))/3.0);
    
    println("MUC RESULTS");
//    val worseMetrics = worseSuffStats(0).toIndexedSeq[Seq[Double]].map(docStats => mucComputer.computeMetricFull(Seq(docStats)).toString);
//    val betterMetrics = betterSuffStats(0).toIndexedSeq[Seq[Double]].map(docStats => mucComputer.computeMetricFull(Seq(docStats)).toString);
//    (0 until 50).foreach(i => println(worseMetrics(i) + "\t\t" + betterMetrics(i)));
    
//    printPValueEachExper(worseSuffStats, betterSuffStats, mucComputer);
    val (baseMuc, improvedMuc) = printAggregatedPValue(worseSuffStats, betterSuffStats, mucComputer, false);
    printAggregatedPValue(worseSuffStats, betterSuffStats, mucComputer, true);
    println("Individual p-values");
    for (i <- 0 until worseSuffStats.size) {
      printAggregatedPValue(Seq(worseSuffStats(i)), Seq(betterSuffStats(i)), mucComputer, false);
    }
    println("BCUB RESULTS");
//    printPValueEachExper(worseSuffStats, betterSuffStats, bcubComputer);
    val (baseBcub, improvedBcub) = printAggregatedPValue(worseSuffStats, betterSuffStats, bcubComputer, false);
    printAggregatedPValue(worseSuffStats, betterSuffStats, bcubComputer, true);
    println("Individual p-values");
    for (i <- 0 until worseSuffStats.size) {
      printAggregatedPValue(Seq(worseSuffStats(i)), Seq(betterSuffStats(i)), bcubComputer, false);
    }
    println("CEAFE RESULTS");
//    printPValueEachExper(worseSuffStats, betterSuffStats, ceafeComputer);
    val (baseCeafe, improvedCeafe) = printAggregatedPValue(worseSuffStats, betterSuffStats, ceafeComputer, false);
    printAggregatedPValue(worseSuffStats, betterSuffStats, ceafeComputer, true);
    println("Individual p-values");
    for (i <- 0 until worseSuffStats.size) {
      printAggregatedPValue(Seq(worseSuffStats(i)), Seq(betterSuffStats(i)), ceafeComputer, false);
    }
  }
  
  def extractSuffStats(fileName: String, numExpers: Int, experIndexToExtract: Int): Seq[String] = {
    val lines = Source.fromFile(fileName).getLines().toSeq.filter(line => line.trim().matches("^DEV: [0-9]+: MUC/BCUB/CEAFE P/R N/D:.*"));
    require(lines.size % numExpers == 0);
    val linesPerExper = lines.size / numExpers;
    lines.slice(linesPerExper * experIndexToExtract, linesPerExper * experIndexToExtract + linesPerExper);
  }
  
  def printPValueEachExper(worseSuffStats: Seq[Seq[Seq[Double]]], betterSuffStats: Seq[Seq[Seq[Double]]], metricComputer: MetricComputer) {
    val rng = new Random(0);
    for (i <- 0 until worseSuffStats.size) {
      val worseSuffStatsExper = worseSuffStats(i);
      val betterSuffStatsExper = betterSuffStats(i);
      var numSigDifferences = 0;
      val NumTrials = 10000;
      for (j <- 0 until NumTrials) {
//        if (j % 1000 == 0) {
//          println(i + " " + j);
//        }
        val resampledIndices = resample(worseSuffStatsExper.size, rng);
        if (metricComputer.isSigDifference(worseSuffStatsExper, betterSuffStatsExper, resampledIndices)) {
          numSigDifferences += 1;
        }
      }
      println("Experiment " + i + " (size " + worseSuffStats(i).size + "): baseline = " + metricComputer.computeMetricFull(worseSuffStatsExper) +
              ", improved = " + metricComputer.computeMetricFull(betterSuffStatsExper) +
              ", fraction sig = " + (numSigDifferences.toDouble/NumTrials.toDouble));
    }
  }
  
  def printAggregatedPValue(worseSuffStats: Seq[Seq[Seq[Double]]], betterSuffStats: Seq[Seq[Seq[Double]]], metricComputer: MetricComputer, stratified: Boolean): (Double, Double) = {
    val rng = new Random(0);
    val experOffsets = new Array[Int](worseSuffStats.size);
    for (i <- 0 until worseSuffStats.size) {
      experOffsets(i) = (if (i == 0) 0 else experOffsets(i-1) + worseSuffStats(i-1).size);
    }
    val allWorseSuffStats = worseSuffStats.flatMap(lst => lst);
    val allBetterSuffStats = betterSuffStats.flatMap(lst => lst);
//    var numBetter = 0;
//    var numEq = 0;
//    for (i <- 0 until allWorseSuffStats.size) {
//      if (metricComputer.computeMetric(Seq(allBetterSuffStats(i))) > metricComputer.computeMetric(Seq(allWorseSuffStats(i)))) {
//        numBetter += 1;
//      }
//      if (metricComputer.computeMetric(Seq(allBetterSuffStats(i))) == metricComputer.computeMetric(Seq(allWorseSuffStats(i)))) {
//        numEq += 1;
//      }
//    }
//    println("Num better: " + numBetter + ", num eq: " + numEq + " (/ " + allWorseSuffStats.size + ")");
    var numSigDifferences = 0;
    val NumTrials = 1000;
    for (j <- 0 until NumTrials) {
//      if (j % 1000 == 0) {
//        println(j);
//      }
      val allResampledIndices = if (stratified) {
        val resampled = new ArrayBuffer[Int];
        for (i <- 0 until worseSuffStats.size) {
          resampled ++= resample(worseSuffStats(i).size, rng).map(idx => idx + experOffsets(i));
        }
        resampled;
      } else {
        resample(allWorseSuffStats.size, rng);
      }
      if (metricComputer.isSigDifference(allWorseSuffStats, allBetterSuffStats, allResampledIndices)) {
        numSigDifferences += 1;
      }
    }
    val baselineFull = metricComputer.computeMetricFull(allWorseSuffStats);
    val improvedFull = metricComputer.computeMetricFull(allBetterSuffStats);
    val fracSig = (numSigDifferences.toDouble/NumTrials.toDouble);
    println("Overall (stratified = " + stratified + "): fraction sig = " + fracSig);
    val dag = if (fracSig > 0.95) "\\ddag" else if (fracSig > 0.9) "\\dag" else "";
    val baselineStrList = baselineFull.map(entry => GUtil.fmtTwoDigitNumber(entry, 2));
    val improvedStrList = improvedFull.map(entry => GUtil.fmtTwoDigitNumber(entry, 2));
    val improvedStrListMutable = new ArrayBuffer[String]();
    improvedStrListMutable ++= improvedStrList;
    improvedStrListMutable(improvedStrListMutable.size-1) = dag + improvedStrListMutable(improvedStrListMutable.size-1);
//    println(baselineStrList.foldLeft("")((str, entry) => str + " & " + entry));
//    println(improvedStrListMutable.foldLeft("")((str, entry) => str + " & " + entry));
    (baselineFull(baselineFull.size - 1), improvedFull(improvedFull.size - 1))
  }
  
  def printSimpleBootstrapPValue(worseSuffStats: Seq[Seq[Double]], betterSuffStats: Seq[Seq[Double]], metricComputer: MetricComputer) {
    val rng = new Random(0);
    var numSigDifferences = 0;
    val NumTrials = 1000;
    for (j <- 0 until NumTrials) {
//      if (j % 1000 == 0) {
//        println(j);
//      }
      val resampledIndices = resample(worseSuffStats.size, rng);
      if (metricComputer.isSigDifference(worseSuffStats, betterSuffStats, resampledIndices)) {
        numSigDifferences += 1;
      }
    }
    val baselineFull = metricComputer.computeMetricFull(worseSuffStats);
    val improvedFull = metricComputer.computeMetricFull(betterSuffStats);
    val fracSig = (numSigDifferences.toDouble/NumTrials.toDouble);
    Logger.logss("1-p = " + fracSig);
    // Print the last metric value since it's usually F1 or whatever
    Logger.logss("Results: " + baselineFull(baselineFull.size - 1) + " " + improvedFull(improvedFull.size - 1));
  }
  
  def fmtMetricValues(suffStats: Seq[Double]): String = {
    MetricComputer.fmtMetricValues(suffStats);
  }
  
  def resample(size: Int, rng: Random): Seq[Int] = {
    (0 until size).map(i => rng.nextInt(size));
  }
}
