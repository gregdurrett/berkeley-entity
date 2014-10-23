package edu.berkeley.nlp.entity.sig

import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.futile.util.Logger

trait MetricComputer {
  
  def isSigDifference(origWorse: Seq[Seq[Double]], origBetter: Seq[Seq[Double]], resampledIndices: Seq[Int]): Boolean = {
    val origBetterScore = computeMetric(origBetter);
    val origWorseScore = computeMetric(origWorse);
    val origDiff = origBetterScore - origWorseScore;
    if (origDiff < 0) {
      false;
    } else {
      val newBetterScore = computeMetric(origBetter, resampledIndices);
      val newWorseScore = computeMetric(origWorse, resampledIndices);
      val newDiff = newBetterScore - newWorseScore;
      var sig = newDiff < 2 * origDiff;
//      println(GUtil.fmtTwoDigitNumber(origWorseScore, 2) + " " + GUtil.fmtTwoDigitNumber(origBetterScore, 2) + " " +
//              GUtil.fmtTwoDigitNumber(newWorseScore, 2) + " " + GUtil.fmtTwoDigitNumber(newBetterScore, 2))
//      newDiff > 0
      sig;
    }
  }
  
  def computeMetric(results: Seq[Seq[Double]]): Double = computeMetric(results, 0 until results.size);
  
  def computeMetric(results: Seq[Seq[Double]], idxList: Seq[Int]): Double;
  
  def computeMetricFull(results: Seq[Seq[Double]]): Seq[Double] = computeMetricFull(results, 0 until results.size);
  
  // Supports returning multiple metric values so you can retrieve precision, recall,
  // and F1 for metrics that have that structure
  def computeMetricFull(results: Seq[Seq[Double]], idxList: Seq[Int]): Seq[Double];
}

object MetricComputer {
    
  def fmtMetricValues(suffStats: Seq[Double]): String = {
    val strList = suffStats.map(entry => GUtil.fmtTwoDigitNumber(entry, 2));
    strList.foldLeft("")((str, entry) => str + " & " + entry)
  }
}

class F1Computer(val precNumIdx: Int, val precDenomIdx: Int, val recNumIdx: Int, val recDenomIdx: Int) extends MetricComputer {
  
  def computeMetric(results: Seq[Seq[Double]], idxList: Seq[Int]): Double = {
    computeMetricFull(results, idxList)(2);
  }
  
  def computeMetricFull(results: Seq[Seq[Double]], idxList: Seq[Int]): Seq[Double] = {
    require(results.size >= 1);
    require(results(0).size > precNumIdx && results(0).size > precDenomIdx && results(0).size > recNumIdx && results(0).size > recDenomIdx)
    var totalPrecNum = idxList.foldLeft(0.0)((total, idx) => total + results(idx)(precNumIdx));
    var totalPrecDenom = idxList.foldLeft(0.0)((total, idx) => total + results(idx)(precDenomIdx));
    var totalRecNum = idxList.foldLeft(0.0)((total, idx) => total + results(idx)(recNumIdx));
    var totalRecDenom = idxList.foldLeft(0.0)((total, idx) => total + results(idx)(recDenomIdx));
    val prec = totalPrecNum/totalPrecDenom;
    val rec = totalRecNum/totalRecDenom;
    Seq(prec * 100.0, rec * 100.0, 2 * prec * rec/(prec + rec) * 100.0);
  }
  
}

object F1Computer {
  def apply() = new F1Computer(0, 1, 0, 2); // correct, total pred, total gold 
}

class AccuracyComputer extends MetricComputer {
  
  def computeMetric(results: Seq[Seq[Double]], idxList: Seq[Int]): Double = {
    computeMetricFull(results, idxList)(0);
  }
  
  def computeMetricFull(results: Seq[Seq[Double]], idxList: Seq[Int]): Seq[Double] = {
    Seq(idxList.map(results(_)(0)).foldLeft(0.0)(_ + _) / idxList.map(results(_)(1)).foldLeft(0.0)(_ + _));
  }
  
}
