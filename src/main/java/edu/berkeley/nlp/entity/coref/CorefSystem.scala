package edu.berkeley.nlp.entity.coref

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import scala.Array.canBuildFrom
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.entity.Chunk
import edu.berkeley.nlp.entity.Driver.WikifierType
import edu.berkeley.nlp.entity.joint.FactorGraphFactoryACE
import edu.berkeley.nlp.entity.joint.FactorGraphFactoryOnto
import edu.berkeley.nlp.entity.joint.GeneralTrainer
import edu.berkeley.nlp.entity.joint.JointComputerShared
import edu.berkeley.nlp.entity.joint.JointDoc
import edu.berkeley.nlp.entity.joint.JointDocACE
import edu.berkeley.nlp.entity.joint.JointFeaturizerShared
import edu.berkeley.nlp.entity.joint.JointLossFcns
import edu.berkeley.nlp.entity.wiki.WikificationEvaluator
import edu.berkeley.nlp.entity.wiki.WikipediaInterface
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.entity.ner.MCNerFeaturizer
import edu.berkeley.nlp.entity.ner.NEEvaluator
import edu.berkeley.nlp.entity.ner.NerFeaturizer
import edu.berkeley.nlp.entity.ner.NerSystemLabeled
import edu.berkeley.nlp.entity.sem.BasicWordNetSemClasser
import edu.berkeley.nlp.entity.sem.QueryCountsBundle
import edu.berkeley.nlp.entity.sem.SemClasser
import edu.berkeley.nlp.entity.wiki._
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.xdistrib.CorefComputerDistrib
import edu.berkeley.nlp.entity.xdistrib.ComponentFeaturizer
import edu.berkeley.nlp.entity.xdistrib.DocumentGraphComponents
import edu.berkeley.nlp.futile.fig.exec.Execution
import edu.berkeley.nlp.entity.Driver
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.entity.ConllDoc
import edu.berkeley.nlp.entity.WordNetInterfacer
import edu.berkeley.nlp.entity.ConllDocWriter
import edu.berkeley.nlp.entity.ConllDocReader
import edu.berkeley.nlp.entity.sem.BrownClusterInterface
import edu.berkeley.nlp.entity.ner.NerPrunerFromMarginals
import edu.berkeley.nlp.entity.ner.NerPruner
import edu.berkeley.nlp.entity.joint.JointPredictor

object CorefSystem {
  
  def checkFileReachableForRead(file: String, msg: String) {
    if (file.isEmpty) {
      throw new RuntimeException("Undefined " + msg + "; must be defined for the mode you're running in");
    }
    if (!new File(file).exists()) {
      throw new RuntimeException(msg + " file/directory doesn't exist for read: " + file);
    }
  }
  def checkFileReachableForWrite(file: String, msg: String) {
    if (file.isEmpty) {
      throw new RuntimeException("Undefined " + msg + "; must be defined for the mode you're running in");
    }
    val fileObj = new File(file);
    // Null if it's in the current directory, which is typically okay
    if (fileObj.getParentFile() != null && !fileObj.getParentFile().exists()) {
      throw new RuntimeException(msg + " file/directory couldn't be opened for write: " + file);
    }
  }
  
  def loadCorefDocs(path: String, size: Int, suffix: String, maybeNumberGenderComputer: Option[NumberGenderComputer]): Seq[CorefDoc] = {
    val docs = ConllDocReader.loadRawConllDocsWithSuffix(path, size, suffix);
    val assembler = CorefDocAssembler(Driver.lang, Driver.useGoldMentions);
    val mentionPropertyComputer = new MentionPropertyComputer(maybeNumberGenderComputer);
    val corefDocs = if (Driver.useCoordination) {
      docs.map(doc => assembler.createCorefDocWithCoordination(doc, mentionPropertyComputer));
    } else {
      docs.map(doc => assembler.createCorefDoc(doc, mentionPropertyComputer));
    }
    CorefDocAssembler.checkGoldMentionRecall(corefDocs);
    corefDocs;
  }
  
  def preprocessDocsCacheResources(allDocGraphs: Seq[DocumentGraph]) {
    if (Driver.wordNetPath != "") {
      val wni = new WordNetInterfacer(Driver.wordNetPath);
      allDocGraphs.foreach(_.cacheWordNetInterfacer(wni));
    }
  }
  
  def runTrainEvaluate(trainPath: String, trainSize: Int, devPath: String, devSize: Int, modelPath: String) {
    checkFileReachableForRead(Driver.conllEvalScriptPath, "conllEvalScriptPath");
    checkFileReachableForRead(devPath, "testPath");
    val scorer = runTrain(trainPath, trainSize);
    if (!modelPath.isEmpty) {
      GUtil.save(scorer, modelPath);
    }
    if (!devPath.isEmpty) {
      runEvaluate(devPath, devSize, scorer);
    }
  }
  
  def runTrainPredict(trainPath: String, trainSize: Int, devPath: String, devSize: Int, modelPath: String, outPath: String, doConllPostprocessing: Boolean) {
    checkFileReachableForRead(devPath, "testPath");
    checkFileReachableForWrite(outPath, "outputPath");
    val scorer = runTrain(trainPath, trainSize);
    if (!modelPath.isEmpty) {
      GUtil.save(scorer, modelPath);
    }
    if (!devPath.isEmpty) {
      runPredictWriteOutput(devPath, devSize, scorer, outPath, doConllPostprocessing);
    }
  }
  
  def runTrain(trainPath: String, trainSize: Int, modelPath: String) {
    checkFileReachableForWrite(modelPath, "modelPath");
    val scorer = runTrain(trainPath, trainSize);
    GUtil.save(scorer, modelPath);
  }
  
  def runTrain(trainPath: String, trainSize: Int): PairwiseScorer = {
    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(Driver.numberGenderDataPath);
    val queryCounts: Option[QueryCountsBundle] = None;
    val trainDocs = loadCorefDocs(trainPath, trainSize, Driver.corefDocSuffix, Some(numberGenderComputer));
    // Randomize
    val trainDocsReordered = new scala.util.Random(0).shuffle(trainDocs);
    val lexicalCounts = LexicalCountsBundle.countLexicalItems(trainDocs, Driver.lexicalFeatCutoff);
    val semClasser: Option[SemClasser] = Driver.semClasserType match {
      case "basic" => Some(new BasicWordNetSemClasser);
      case e => throw new RuntimeException("Other semclassers not implemented");
    }
    val trainDocGraphs = trainDocsReordered.map(new DocumentGraph(_, true));
    preprocessDocsCacheResources(trainDocGraphs);
    CorefPruner.buildPruner(Driver.pruningStrategy).pruneAll(trainDocGraphs);
    
    val featureIndexer = new Indexer[String]();
    featureIndexer.getIndex(PairwiseIndexingFeaturizerJoint.UnkFeatName);
    val featureSetSpec = FeatureSetSpecification(Driver.pairwiseFeats, Driver.conjScheme, Driver.conjFeats, Driver.conjMentionTypes, Driver.conjTemplates);
    val auxFeaturizers = new ArrayBuffer[AuxiliaryFeaturizer]
    if (featureSetSpec.featsToUse.contains("lexinf")) {
      auxFeaturizers += LexicalInferenceFeaturizer.loadLexInfFeaturizer(Driver.lexInfPath)
    }
    val basicFeaturizer = new PairwiseIndexingFeaturizerJoint(featureIndexer, featureSetSpec, lexicalCounts, queryCounts, semClasser, auxFeaturizers);
    val featurizerTrainer = new CorefFeaturizerTrainer();
    featurizerTrainer.featurizeBasic(trainDocGraphs, basicFeaturizer);
    PairwiseIndexingFeaturizer.printFeatureTemplateCounts(featureIndexer)

    val weights = if (Driver.alternateTraining) {
      val basicInferencer = new DocumentInferencerBasic()
      val lossFcnObjFirstPass = PairwiseLossFunctions(Driver.lossFcn);
      featurizerTrainer.train(trainDocGraphs, basicFeaturizer, Driver.eta.toFloat, Driver.reg.toFloat, Driver.batchSize, lossFcnObjFirstPass, Driver.numItrs, basicInferencer);
    } else {
      
      Array[Float]()
    }
    new PairwiseScorer(basicFeaturizer, weights).pack;
  }
  
  def prepareTestDocuments(devPath: String, devSize: Int): Seq[DocumentGraph] = {
    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(Driver.numberGenderDataPath);
    val devDocs = loadCorefDocs(devPath, devSize, Driver.corefDocSuffix, Some(numberGenderComputer));
    val devDocGraphs = devDocs.map(new DocumentGraph(_, false));
    preprocessDocsCacheResources(devDocGraphs);
    CorefPruner.buildPruner(Driver.pruningStrategy).pruneAll(devDocGraphs);
    devDocGraphs;
  }
  
  def runPredict(devDocGraphs: Seq[DocumentGraph], scorer: PairwiseScorer): Seq[(Array[Int],OrderedClustering)] = {
    runPredict(devDocGraphs, scorer, false);
  }
  
  def runPredictParallel(devDocGraphs: Seq[DocumentGraph], scorer: PairwiseScorer): Seq[(Array[Int],OrderedClustering)] = {
    runPredict(devDocGraphs, scorer, true);
  }
  
  def runPredict(devDocGraphs: Seq[DocumentGraph], scorer: PairwiseScorer, isParallel: Boolean): Seq[(Array[Int],OrderedClustering)] = {
    val basicInferencer = new DocumentInferencerBasic();
    val indices = (0 until devDocGraphs.size);
    Logger.startTrack("Decoding dev");
    val results = (if (isParallel) indices.par else indices).map(i => {
      Logger.logss("Decoding " + i);
      val devDocGraph = devDocGraphs(i);
      devDocGraph.featurizeIndexNonPrunedUseCache(scorer.featurizer);
      val (backptrs, clustering) = basicInferencer.viterbiDecodeFormClustering(devDocGraph, scorer);
      devDocGraph.clearFeatureCache();
      (backptrs, clustering);
    }).toIndexedSeq;
    Logger.endTrack();
    results;
  }
  
  def runEvaluate(devPath: String, devSize: Int, modelPath: String) {
    runEvaluateParallel(devPath, devSize, GUtil.load(modelPath).asInstanceOf[PairwiseScorer]);
  }
  
  def runEvaluate(devPath: String, devSize: Int, scorer: PairwiseScorer) {
    val devDocGraphs = prepareTestDocuments(devPath, devSize);
    new CorefFeaturizerTrainer().featurizeBasic(devDocGraphs, scorer.featurizer);  // dev docs already know they are dev docs so they don't add features
    Logger.startTrack("Decoding dev");
    val basicInferencer = new DocumentInferencerBasic();
    val (allPredBackptrs, allPredClusterings) = basicInferencer.viterbiDecodeAllFormClusterings(devDocGraphs, scorer);
    Logger.logss(CorefEvaluator.evaluateAndRender(devDocGraphs, allPredBackptrs, allPredClusterings, Driver.conllEvalScriptPath, "DEV: ", Driver.analysesToPrint));
    Logger.endTrack();
  }
  
  def runEvaluateParallel(devPath: String, devSize: Int, scorer: PairwiseScorer) {
    val devDocGraphs = prepareTestDocuments(devPath, devSize);
    val allPredBackptrsAndClusterings = runPredictParallel(devDocGraphs, scorer);
    Logger.logss(CorefEvaluator.evaluateAndRender(devDocGraphs, allPredBackptrsAndClusterings.map(_._1), allPredBackptrsAndClusterings.map(_._2), Driver.conllEvalScriptPath, "DEV: ", Driver.analysesToPrint));
  }
  
  def runPredictWriteOutput(devPath: String, devSize: Int, modelPath: String, outPath: String, doConllPostprocessing: Boolean) {
    runPredictWriteOutput(devPath, devSize, GUtil.load(modelPath).asInstanceOf[PairwiseScorer], outPath, doConllPostprocessing);
  }
  
  def runPredictWriteOutput(devPath: String, devSize: Int, scorer: PairwiseScorer, outPath: String, doConllPostprocessing: Boolean) {
    // Read because it should be a directory
    checkFileReachableForRead(outPath, "outputPath");
    val devDocGraphs = prepareTestDocuments(devPath, devSize);
    val allPredBackptrsAndPredClusterings = runPredict(devDocGraphs, scorer, false);
    for (i <- 0 until devDocGraphs.size) {
      val writer = IOUtils.openOutHard(outPath + "/" + devDocGraphs(i).corefDoc.rawDoc.fileName + "-" + devDocGraphs(i).corefDoc.rawDoc.docPartNo + ".pred_conll");
      ConllDocWriter.writeDoc(writer, devDocGraphs(i).corefDoc.rawDoc, allPredBackptrsAndPredClusterings(i)._2.bind(devDocGraphs(i).getMentions, doConllPostprocessing));
      writer.close();
    }
  }
}
