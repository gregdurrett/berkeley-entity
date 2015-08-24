package edu.berkeley.nlp.entity.coref

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.sem.QueryCountsBundle
import edu.berkeley.nlp.entity.sem.BasicWordNetSemClasser
import scala.util.Random
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.entity.Driver

trait CorefPruner {

  def prune(doc: DocumentGraph);
  
  def pruneAll(docs: Seq[DocumentGraph]) {
    var pruningStats = new PruningStats(0, 0, 0, 0, 0, 0, 0, 0);
    for (doc <- docs) {
      prune(doc);
      pruningStats = pruningStats.add(doc.computePruningStats());
    }
    Logger.logss("Pruning result: " + pruningStats);
  }
}

@SerialVersionUID(1L)
class CorefPrunerDistance(val maxBackptrMentDistance: Int,
                          val maxPronounSentDistance: Int) extends CorefPruner with Serializable {
  
  def prune(doc: DocumentGraph) {
    doc.pruneEdgesMentDistanceSentDistance(maxBackptrMentDistance, maxPronounSentDistance)
  }
}

@SerialVersionUID(1L)
class CorefPrunerStandard(val pairwiseScorer: PairwiseScorer,
                          val logPruningThreshold: Double) extends CorefPruner with Serializable {
  
  def prune(doc: DocumentGraph) {
    pruneWithGivenThreshold(doc, logPruningThreshold);
  }
  
  def pruneWithGivenThreshold(doc: DocumentGraph, newThreshold: Double) {
    doc.pruneEdgesModel(pairwiseScorer, newThreshold)
  }
}

@SerialVersionUID(1L)
class CorefPrunerFolds(val docsToFolds: HashMap[UID,Int],
                       val foldModels: ArrayBuffer[PairwiseScorer],
                       val logPruningThreshold: Double) extends CorefPruner with Serializable {
  
  def prune(doc: DocumentGraph) {
    pruneWithGivenThreshold(doc, logPruningThreshold);
  }
  
  def pruneWithGivenThreshold(doc: DocumentGraph, threshold: Double) {
    if (docsToFolds.contains(doc.corefDoc.rawDoc.uid)) {
       doc.pruneEdgesModel(foldModels(docsToFolds(doc.corefDoc.rawDoc.uid)), threshold);
    } else {
      doc.pruneEdgesModel(foldModels.head, threshold);
    }
  }
}

object CorefPrunerJavaHack {
  def trainAndSaveKFoldModels(trainPath: String, trainSize: Int, numFolds: Int, modelsOutPath: String) = {
    CorefPruner.trainAndSaveKFoldModels(trainPath, trainSize, numFolds, modelsOutPath)
  }
}

object CorefPruner {
  
  def buildPruner(strategy: String): CorefPruner = {
    val splitStrategy = strategy.split(":");
    if (splitStrategy(0) == "distance") {
      val splitStrategy = strategy.split(":");
      val maxBackptrMentDistance = splitStrategy(1).toInt
      val maxPronounSentDistance = splitStrategy(2).toInt;
      new CorefPrunerDistance(maxBackptrMentDistance, maxPronounSentDistance)
    } else if (splitStrategy(0) == "models") {
      val modelPath = splitStrategy(1)
      val logPruningThreshold = splitStrategy(2).toFloat;
      val (foldMapping, models) = GUtil.load(modelPath).asInstanceOf[(HashMap[UID,Int], ArrayBuffer[PairwiseScorer])];
      new CorefPrunerFolds(foldMapping, models, logPruningThreshold);
    } else if (splitStrategy(0) == "build") {
      Logger.logss("----------------------------")
      Logger.logss("BUILDING COARSE COREF MODELS");
      val modelPath = splitStrategy(1)
      val threshold = splitStrategy(2).toFloat;
      val numFolds = splitStrategy(3).toInt;
      val docIDsAndModels = trainAndSaveKFoldModels(Driver.trainPath, Driver.trainSize, numFolds, modelPath);
      new CorefPrunerFolds(new HashMap[UID,Int] ++ docIDsAndModels._1, docIDsAndModels._2, threshold); 
    } else {
      throw new RuntimeException("Unrecognized strategy: " + strategy);
    }
  }
  
  def trainAndSaveKFoldModels(trainPath: String, trainSize: Int, numFolds: Int, modelsOutPath: String) = {
    CorefSystem.checkFileReachableForWrite(modelsOutPath, "modelsOutPath");
    val docIDsAndModels = trainKFoldModels(trainPath, trainSize, numFolds);
    GUtil.save(docIDsAndModels, modelsOutPath);
    docIDsAndModels
  }
  
  def trainKFoldModels(trainPath: String, trainSize: Int, numFolds: Int) = {
    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(Driver.numberGenderDataPath);
    val queryCounts: Option[QueryCountsBundle] = None;
    // Sort and then shuffle so we get the same folds every time
    val allTrainDocs = CorefSystem.loadCorefDocs(trainPath, trainSize, "auto_conll", Some(numberGenderComputer)).sortBy(_.rawDoc.uid);
    val allTrainDocsReordered = new scala.util.Random(0).shuffle(allTrainDocs);
    
    val rand = new Random(0);
    // Shuffle the doc IDs and assign them in equal measure to each fold
    val docIDsShuffled = rand.shuffle(allTrainDocsReordered.map(_.rawDoc.uid))
    val foldSize = docIDsShuffled.size/numFolds;
    val docIDsToFolds = new HashMap[UID,Int] ++ (0 until docIDsShuffled.size).map(i => docIDsShuffled(i) -> Math.min(i/foldSize, numFolds - 1));
    val docIDsAndModels = (docIDsToFolds, new ArrayBuffer[PairwiseScorer]);
    
    for (fold <- (0 until numFolds)) {
      // Train on data outside the fold, evaluate on data inside the fold
      val foldTrainingDocs = allTrainDocsReordered.filter(doc => docIDsToFolds(doc.rawDoc.uid) != fold);
      val foldTestDocs = allTrainDocsReordered.filter(doc => docIDsToFolds(doc.rawDoc.uid) == fold);
      Logger.logss("STARTING FOLD " + fold + ": training on " + foldTrainingDocs.size + " documents, running on " + foldTestDocs.size + " documents");
      
      val foldTrainDocGraphs = foldTrainingDocs.map(new DocumentGraph(_, true));
      CorefSystem.preprocessDocsCacheResources(foldTrainDocGraphs);
      
      val lexicalCounts = LexicalCountsBundle.countLexicalItems(foldTrainingDocs, Driver.lexicalFeatCutoff);
      val featureIndexer = new Indexer[String]();
      featureIndexer.getIndex(PairwiseIndexingFeaturizerJoint.UnkFeatName);
      val featureSetSpec = FeatureSetSpecification(Driver.pairwiseFeats, Driver.conjScheme, Driver.conjFeats, Driver.conjMentionTypes, Driver.conjTemplates);
      val basicFeaturizer = new PairwiseIndexingFeaturizerJoint(featureIndexer, featureSetSpec, lexicalCounts, queryCounts, Some(new BasicWordNetSemClasser), Seq[AuxiliaryFeaturizer]());
      val featurizerTrainer = new CorefFeaturizerTrainer();
      featurizerTrainer.featurizeBasic(foldTrainDocGraphs, basicFeaturizer);
      
      val basicInferencer = new DocumentInferencerBasic()
      val lossFcnObjFirstPass = PairwiseLossFunctions(Driver.lossFcn);
      val firstPassWeights = featurizerTrainer.train(foldTrainDocGraphs,
                                                     basicFeaturizer,
                                                     Driver.eta.toFloat,
                                                     Driver.reg.toFloat,
                                                     Driver.batchSize,
                                                     lossFcnObjFirstPass,
                                                     Driver.numItrs,
                                                     basicInferencer);
      val pairwiseScorer = new PairwiseScorer(basicFeaturizer, firstPassWeights).pack;
      docIDsAndModels._2 += pairwiseScorer;
    }
    Logger.logs(docIDsAndModels._2.size + " models created");
    docIDsAndModels
  }
}
