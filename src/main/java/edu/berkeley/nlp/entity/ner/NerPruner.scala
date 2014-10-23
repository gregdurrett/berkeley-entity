package edu.berkeley.nlp.entity.ner

import scala.collection.mutable.HashMap
import edu.berkeley.nlp.entity.coref.UID
import edu.berkeley.nlp.entity.ConllDoc
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.entity.Driver
import edu.berkeley.nlp.futile.util.Logger

trait NerPruner {

  def pruneSentence(doc: ConllDoc, sentIdx: Int): Array[Array[String]];
}

@SerialVersionUID(1L)
class NerPrunerFromModel(val nerModel: NerSystemLabeled,
                         val pruningThreshold: Double) extends NerPruner with Serializable {
  
  def pruneSentence(doc: ConllDoc, sentIdx: Int): Array[Array[String]] = {
    val sentMarginals = nerModel.computeLogMarginals(doc.words(sentIdx).toArray, doc.pos(sentIdx).toArray);
    NerPruner.pruneFromMarginals(sentMarginals, nerModel.labelIndexer, pruningThreshold);
  }
}

@SerialVersionUID(1L)
class NerPrunerFromMarginals(val nerMarginals: HashMap[UID,Seq[Array[Array[Float]]]],
                             val neLabelIndexer: Indexer[String],
                             val pruningThreshold: Double) extends NerPruner with Serializable  {
  
  def pruneSentence(doc: ConllDoc, sentIdx: Int): Array[Array[String]] = {
    require(nerMarginals.contains(doc.uid));
    NerPruner.pruneFromMarginals(nerMarginals(doc.uid)(sentIdx), neLabelIndexer, pruningThreshold);
  }
}

@SerialVersionUID(1L)
class NerPrunerFromMarginalsAndModel(val nerMarginals: HashMap[UID,Seq[Array[Array[Float]]]],
                                     val neLabelIndexer: Indexer[String],
                                     val nerModel: NerSystemLabeled,
                                     val pruningThreshold: Double) extends NerPruner with Serializable {
  
  def pruneSentence(doc: ConllDoc, sentIdx: Int): Array[Array[String]] = {
    val sentMarginals = if (nerMarginals.contains(doc.uid)) {
      nerMarginals(doc.uid)(sentIdx)
    } else {
      nerModel.computeLogMarginals(doc.words(sentIdx).toArray, doc.pos(sentIdx).toArray);
    }
    NerPruner.pruneFromMarginals(sentMarginals, neLabelIndexer, pruningThreshold);
  }
}

object NerPruner {
  
  def buildPruner(strategy: String): NerPruner = {
    val splitStrategy = strategy.split(":");
    if (splitStrategy(0) == "model") {
      val nerModel = GUtil.load(splitStrategy(1)).asInstanceOf[NerSystemLabeled]
      val threshold = splitStrategy(2).toDouble;
      new NerPrunerFromModel(nerModel, threshold);
    } else if (splitStrategy(0) == "marginals") {
      val nerMarginals = GUtil.load(splitStrategy(1)).asInstanceOf[HashMap[UID,Seq[Array[Array[Float]]]]];
      val threshold = splitStrategy(2).toDouble;
      new NerPrunerFromMarginals(nerMarginals, NerSystemLabeled.StdLabelIndexer, threshold);
    } else if (splitStrategy(0) == "marginalsmodel") {
      val nerMarginals = GUtil.load(splitStrategy(1)).asInstanceOf[HashMap[UID,Seq[Array[Array[Float]]]]];
      val nerModel = GUtil.load(splitStrategy(2)).asInstanceOf[NerSystemLabeled]
      val threshold = splitStrategy(3).toDouble
      new NerPrunerFromMarginalsAndModel(nerMarginals, NerSystemLabeled.StdLabelIndexer, nerModel, threshold);
    } else if (splitStrategy(0) == "build") {
      Logger.logss("----------------------------")
      Logger.logss("BUILDING COARSE NER MODELS");
      val marginalsOutPath = splitStrategy(1)
      val modelOutPath = if (marginalsOutPath.endsWith(".ser.gz")) marginalsOutPath.dropRight(7) + "-model.ser.gz" else marginalsOutPath + "-model.ser.gz";
      val threshold = splitStrategy(2).toFloat;
      val numFolds = splitStrategy(3).toInt;
      val (nerMarginals, nerModel) = NerSystemLabeled.trainPredictTokenMarginalsKFold(Driver.trainPath, Driver.trainSize, Driver.brownPath, Array(Driver.testPath), Driver.testSize, numFolds, marginalsOutPath, modelOutPath);
      new NerPrunerFromMarginalsAndModel(nerMarginals, NerSystemLabeled.StdLabelIndexer, nerModel, threshold);
    } else {
      throw new RuntimeException("Unknown NER pruning method")
    }
  }
  
  def pruneFromMarginals(sentMarginals: Array[Array[Float]], neLabelIndexer: Indexer[String], pruningThreshold: Double) = {
    val sentLen = sentMarginals.size;
    val allOptions = Array.tabulate(sentLen)(wordIdx => {
      val bestOption = GUtil.argMaxIdxFloat(sentMarginals(wordIdx));
      val bestScore = sentMarginals(wordIdx)(bestOption);
      val remainingOptions = (0 until neLabelIndexer.size).toArray.filter(labelIdx => {
        !sentMarginals(wordIdx)(labelIdx).isInfinite && sentMarginals(wordIdx)(labelIdx) >= bestScore + pruningThreshold
      }).map(i => neLabelIndexer.getObject(i))
      remainingOptions;
    });
    allOptions;
  } 
}
