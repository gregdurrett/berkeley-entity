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
    val trainDocs = loadCorefDocs(trainPath, trainSize, Driver.docSuffix, Some(numberGenderComputer));
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
    val basicFeaturizer = new PairwiseIndexingFeaturizerJoint(featureIndexer, featureSetSpec, lexicalCounts, queryCounts, semClasser);
    val featurizerTrainer = new CorefFeaturizerTrainer();
    featurizerTrainer.featurizeBasic(trainDocGraphs, basicFeaturizer);
    PairwiseIndexingFeaturizer.printFeatureTemplateCounts(featureIndexer)

    val basicInferencer = new DocumentInferencerBasic()
    val lossFcnObjFirstPass = PairwiseLossFunctions(Driver.lossFcn);
    val firstPassWeights = featurizerTrainer.train(trainDocGraphs,
                                                   basicFeaturizer,
                                                   Driver.eta.toFloat,
                                                   Driver.reg.toFloat,
                                                   Driver.batchSize,
                                                   lossFcnObjFirstPass,
                                                   Driver.numItrs,
                                                   basicInferencer);
    new PairwiseScorer(basicFeaturizer, firstPassWeights).pack;
  }
  
  def prepareTestDocuments(devPath: String, devSize: Int): Seq[DocumentGraph] = {
    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(Driver.numberGenderDataPath);
    val devDocs = loadCorefDocs(devPath, devSize, Driver.docSuffix, Some(numberGenderComputer));
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
//    new CorefFeaturizerTrainer().featurizeBasic(devDocGraphs, scorer.featurizer);  // dev docs already know they are dev docs so they don't add features
//    val basicInferencer = new DocumentInferencerBasic();
    val allPredBackptrsAndPredClusterings = runPredict(devDocGraphs, scorer, false);
    for (i <- 0 until devDocGraphs.size) {
      val writer = IOUtils.openOutHard(outPath + "/" + devDocGraphs(i).corefDoc.rawDoc.fileName + "-" + devDocGraphs(i).corefDoc.rawDoc.docPartNo + ".pred_conll");
      ConllDocWriter.writeDoc(writer, devDocGraphs(i).corefDoc.rawDoc, allPredBackptrsAndPredClusterings(i)._2.bind(devDocGraphs(i).getMentions, doConllPostprocessing));
      writer.close();
    }
  }
  
  
  //////////////////////////////
  //////////////////////////////
  //////////////////////////////
  
//  
//  def preprocessDocsForEverything(path: String,
//                                  size: Int,
//                                  mentionPropertyComputer: MentionPropertyComputer,
//                                  nerPruner: NerPruner,
//                                  corefPruner: CorefPruner,
//                                  train: Boolean) = {
//    // Read in raw data
//    val rawDocs = ConllDocReader.loadRawConllDocs(path, size, false);
//    val goldConllDocs = ConllDocReader.loadRawConllDocs(path, size, true);
//    val goldWikification = new HashMap[String,HashMap[Int,ArrayBuffer[Chunk[String]]]];
//    val assembler = CorefDocAssembler(Driver.lang, Driver.useGoldMentions);
//    val corefDocs = rawDocs.map(doc => assembler.createCorefDoc(doc, mentionPropertyComputer));
//    CorefDocAssembler.checkGoldMentionRecall(corefDocs);
//    val docGraphs = corefDocs.map(new DocumentGraph(_, train));
//    preprocessDocsCacheResources(docGraphs);
//    // Prune coref now that we have mentions
//    corefPruner.pruneAll(docGraphs);
//    
//    val jointDocsOrigOrder = JointDoc.assembleJointDocs(docGraphs, goldConllDocs, goldWikification);
//    // Store NER marginals
//    
////    JointDoc.applyNerPruning(jointDocsOrigOrder, nerMarginals);
//    
//    jointDocsOrigOrder.foreach(_.cacheNerPruner(Some(nerPruner)));
//    if (train) {
//      // Randomize
//      new scala.util.Random(0).shuffle(jointDocsOrigOrder)
//    } else {
//      jointDocsOrigOrder;
//    }
//  }
//  
//  def runTrainEvaluateEverything(trainPath: String, trainSize: Int, testPath: String, testSize: Int) = {
//    // Resources needed for document assembly: number/gender computer, NER marginals, coref models and mapping of documents to folds
//    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(Driver.numberGenderDataPath);
//    val mentionPropertyComputer = new MentionPropertyComputer(Some(numberGenderComputer));
//    
//    // N.B. DIFFERENT DUE TO NER BEING PRESENT
//    // Load NER pruning masks and coref models
////    val nerMarginals = GUtil.load(Driver.nerMarginalsPath).asInstanceOf[HashMap[UID,Seq[Array[Array[Float]]]]];
////    val nerPruner = new NerPrunerFromMarginals(nerMarginals, NerSystemLabeled.StdLabelIndexer, -Driver.nerPruningNegThreshold);
//    val nerPruner = NerPruner.buildPruner(Driver.nerPruningStrategy);
//    val corefPruner = CorefPruner.buildPruner(Driver.pruningStrategy)
//    val jointDocs = preprocessDocsForEverything(trainPath, trainSize, mentionPropertyComputer, nerPruner, corefPruner, true);
//    
//    // N.B. Only difference here is the NER
//    ///////////////////////
//    // Build the featurizer, which involves building specific featurizers for each task
//    val featureIndexer = new Indexer[String];
//    val maybeBrownClusters = if (Driver.brownPath != "") Some(BrownClusterInterface.loadBrownClusters(Driver.brownPath, 0)) else None
//    val nerFeaturizer = NerFeaturizer(Driver.nerFeatureSet.split("\\+").toSet, featureIndexer, NerSystemLabeled.StdLabelIndexer, jointDocs.flatMap(_.rawDoc.words), None, maybeBrownClusters);
//    val jointFeaturizer = buildFeaturizerShared(jointDocs.map(_.docGraph.corefDoc), featureIndexer, nerFeaturizer, maybeBrownClusters);
//    val maybeWikipediaInterface: Option[WikipediaInterface] = if (Driver.wikipediaPath != "") Some(GUtil.load(Driver.wikipediaPath).asInstanceOf[WikipediaInterface]) else None;
//    
//    
//    ///////////////////////
//    val fgfOnto = new FactorGraphFactoryOnto(jointFeaturizer, maybeWikipediaInterface);
//    val computer = new JointComputerShared(fgfOnto);
//    jointDocs.foreach(jointDoc => {
//      fgfOnto.getDocFactorGraph(jointDoc, true, true, true, PairwiseLossFunctions(Driver.lossFcn), JointLossFcns.nerLossFcn, JointLossFcns.wikiLossFcn);
//      fgfOnto.getDocFactorGraph(jointDoc, false, true, true, PairwiseLossFunctions(Driver.lossFcn), JointLossFcns.nerLossFcn, JointLossFcns.wikiLossFcn);
//    });
//    PairwiseIndexingFeaturizer.printFeatureTemplateCounts(featureIndexer)
//    Logger.logss(featureIndexer.size + " total features");
//    
//    val finalWeights = new GeneralTrainer[JointDoc].trainAdagrad(jointDocs, computer, featureIndexer.size, Driver.eta.toFloat, Driver.reg.toFloat, Driver.batchSize, Driver.numItrs);
//    if (Driver.modelPath != "") GUtil.save(new JointPredictor(jointFeaturizer, finalWeights, corefPruner, nerPruner), Driver.modelPath);
//    
//    ///////////////////////
//    // Evaluation of each part of the model
//    // Build dev docs
//    val jointDevDocs = preprocessDocsForEverything(testPath, testSize, mentionPropertyComputer, nerPruner, corefPruner, false);
//    
//    val allPredBackptrsAndClusterings = new ArrayBuffer[(Array[Int],OrderedClustering)];
//    val predNEChunks = new ArrayBuffer[Seq[Seq[Chunk[String]]]];
//    val outWriter = IOUtils.openOutHard(Execution.getFile("output.conll"))
//    val outWikiWriter = IOUtils.openOutHard(Execution.getFile("output-wiki.conll"))
//    Logger.startTrack("Decoding");
//    for (i <- (0 until jointDevDocs.size)) {
//      Logger.logss("Decoding " + i);
//      val jointDevDoc = jointDevDocs(i);
//      val (backptrs, clustering, nerChunks, wikiChunks) = computer.viterbiDecodeProduceAnnotations(jointDevDoc, finalWeights);
//      allPredBackptrsAndClusterings += (backptrs -> clustering);
//      predNEChunks += nerChunks;
//      ConllDocWriter.writeDocWithPredAnnotationsWikiStandoff(outWriter, outWikiWriter, jointDevDoc.rawDoc, nerChunks, clustering.bind(jointDevDoc.docGraph.getMentions, Driver.doConllPostprocessing), wikiChunks);
//    }
//    outWriter.close();
//    outWikiWriter.close();
//    Logger.endTrack();
//    Logger.logss(CorefEvaluator.evaluateAndRender(jointDevDocs.map(_.docGraph), allPredBackptrsAndClusterings.map(_._1), allPredBackptrsAndClusterings.map(_._2),
//                                                  Driver.conllEvalScriptPath, "DEV: ", Driver.analysesToPrint));
//    NEEvaluator.evaluateChunksBySent(jointDevDocs.flatMap(_.goldNERChunks), predNEChunks.flatten);
//    NEEvaluator.evaluateOnConll2011(jointDevDocs, predNEChunks, Driver.conll2011Path.split(",").flatMap(path => ConllDocReader.readDocNames(path)).toSet, if (Driver.writeNerOutput) Execution.getFile("ner.txt") else "");
//  }
//  
//  
//  def runOntoPredict(path: String, size: Int, modelPath: String) {
//    val jointPredictor = GUtil.load(modelPath).asInstanceOf[JointPredictor];
//    
//    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(Driver.numberGenderDataPath);
//    val mentionPropertyComputer = new MentionPropertyComputer(Some(numberGenderComputer));
//    val maybeWikipediaInterface: Option[WikipediaInterface] = if (Driver.wikipediaPath != "") Some(GUtil.load(Driver.wikipediaPath).asInstanceOf[WikipediaInterface]) else None;
//    jointPredictor.decode(path, size, mentionPropertyComputer, maybeWikipediaInterface, Driver.doConllPostprocessing);
//  }
//  
//  
//  //////////////////////////////
//  //////////// ACE /////////////
//  //////////////////////////////
//  def preprocessACEDocsForEverything(path: String,
//                                     size: Int,
//                                     mentionPropertyComputer: MentionPropertyComputer,
//                                     corefPruner: CorefPruner,
//                                     wikiPath: String,
//                                     train: Boolean) = {
//    // Read in raw data
//    val rawDocs = ConllDocReader.loadRawConllDocsWithSuffix(path, size, "", Language.ENGLISH);
//    val goldWikification: CorpusWikiAnnots = if (wikiPath != "") {
//      val corpusAnnots = new CorpusWikiAnnots;
//      for (entry <- WikiAnnotReaderWriter.readAllStandoffAnnots(wikiPath)) {
//        val fileName = entry._1._1;
//        val docAnnots = new DocWikiAnnots;
//        for (i <- 0 until entry._2.size) {
////          if (!entry._2(i).isEmpty) {
//          docAnnots += i -> (new ArrayBuffer[Chunk[Seq[String]]]() ++ entry._2(i))
////          }
//        }
//        corpusAnnots += fileName -> docAnnots
//      }
//      corpusAnnots;
//    } else {
//      Logger.logss("Wikification not loaded");
//      new CorpusWikiAnnots;
//    }
//    val corefDocs = if (Driver.useGoldMentions) {
//      val assembler = CorefDocAssembler(Driver.lang, true);
//      rawDocs.map(doc => assembler.createCorefDoc(doc, mentionPropertyComputer));
//    } else {
//      val assembler = new CorefDocAssemblerACE(Driver.allAcePath);
//      rawDocs.map(doc => assembler.createCorefDoc(doc, mentionPropertyComputer));
//    }
//    CorefDocAssembler.checkGoldMentionRecall(corefDocs);
//    val docGraphs = corefDocs.map(new DocumentGraph(_, train));
//    preprocessDocsCacheResources(docGraphs);
//    // Prune coref now that we have mentions
//    corefPruner.pruneAll(docGraphs);
//    
//    
//    val jointDocsOrigOrder = JointDocACE.assembleJointDocs(docGraphs, goldWikification);
//    // TODO: Apply NER pruning
////    JointDoc.applyNerPruning(jointDocsOrigOrder, nerMarginals);
//    if (train) {
//      // Randomize
//      new scala.util.Random(0).shuffle(jointDocsOrigOrder)
//    } else {
//      jointDocsOrigOrder;
//    }
//  }
//  
//  def runTrainEvaluateEverythingACE(trainPath: String, trainSize: Int, testPath: String, testSize: Int) = {
//    // Resources needed for document assembly: number/gender computer, NER marginals, coref models and mapping of documents to folds
//    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(Driver.numberGenderDataPath);
//    val mentionPropertyComputer = new MentionPropertyComputer(Some(numberGenderComputer));
//    
//    // Load coref models
//    val corefPruner = CorefPruner.buildPruner(Driver.pruningStrategy)
//    val jointDocs = preprocessACEDocsForEverything(trainPath, trainSize, mentionPropertyComputer, corefPruner, Driver.wikiGoldPath, true);
//    // TODO: Are NER models necessary?
//    
//    ///////////////////////
//    // Build the featurizer, which involves building specific featurizers for each task
//    val featureIndexer = new Indexer[String]();
//    val maybeBrownClusters = if (Driver.brownPath != "") Some(BrownClusterInterface.loadBrownClusters(Driver.brownPath, 0)) else None
//    val nerFeaturizer = MCNerFeaturizer(Driver.nerFeatureSet.split("\\+").toSet, featureIndexer, MCNerFeaturizer.StdLabelIndexer, jointDocs.flatMap(_.rawDoc.words), None, maybeBrownClusters)
//    val jointFeaturizer = buildFeaturizerShared(jointDocs.map(_.docGraph.corefDoc), featureIndexer, nerFeaturizer, maybeBrownClusters);
//    val maybeWikipediaInterface: Option[WikipediaInterface] = if (Driver.wikipediaPath != "") Some(GUtil.load(Driver.wikipediaPath).asInstanceOf[WikipediaInterface]) else None;
//    
//    ///////////////////////
//    // Cache features
//    val fgfAce = new FactorGraphFactoryACE(jointFeaturizer, maybeWikipediaInterface);
//    val computer = new JointComputerShared(fgfAce);
//    jointDocs.foreach(jointDoc => {
//      fgfAce.getDocFactorGraph(jointDoc, true, true, true, PairwiseLossFunctions(Driver.lossFcn), JointLossFcns.nerLossFcn, JointLossFcns.wikiLossFcn);
//      fgfAce.getDocFactorGraph(jointDoc, false, true, true, PairwiseLossFunctions(Driver.lossFcn), JointLossFcns.nerLossFcn, JointLossFcns.wikiLossFcn);
//    });
//    PairwiseIndexingFeaturizer.printFeatureTemplateCounts(featureIndexer)
//    Logger.logss(featureIndexer.size + " total features");
//    
//    val finalWeights = new GeneralTrainer[JointDocACE].trainAdagrad(jointDocs, computer, featureIndexer.size, Driver.eta.toFloat, Driver.reg.toFloat, Driver.batchSize, Driver.numItrs);
//    
//    ///////////////////////
//    // Evaluation of each part of the model
//    // Build dev docs
//    val jointDevDocs = preprocessACEDocsForEverything(testPath, testSize, mentionPropertyComputer, corefPruner, Driver.wikiGoldPath, false);
//    
//    val allPredBackptrsAndClusterings = new ArrayBuffer[(Array[Int],OrderedClustering)];
//    val allPredNEChunks = new ArrayBuffer[Seq[Seq[Chunk[String]]]];
//    val allPredWikiChunks = new ArrayBuffer[Seq[Chunk[String]]];
//    val allPredWikiTitles = new ArrayBuffer[Set[String]];
//    val predWriter = if (Driver.outputPath != "") Some(IOUtils.openOutHard(Driver.outputPath)) else None;
//    val goldWriter = if (Driver.writeGold) Some(IOUtils.openOutHard(Execution.getFile("gold.conll"))) else None;
//    val predWikiWriter = if (Driver.writeWikiOutput) Some(IOUtils.openOutHard(Execution.getFile("wiki.txt"))) else None;
//    val predWikiWriterAux = if (Driver.writeWikiOutput) Some(IOUtils.openOutHard(Execution.getFile("wiki-aux.txt"))) else None;
//    val maybeRawChunkNames = if (Driver.writeWikiOutput) Some(ACEMunger.mungeACEToGetChunkLabels(Driver.allAcePath, Driver.rawWikiGoldPath)) else None
//    val wikiLabelsInTrain: Set[String] = jointDocs.flatMap(_.goldWikiChunks.flatMap(_.flatMap(_.label)).toSet).toSet;
//    val outWriter = IOUtils.openOutHard(Execution.getFile("output.conll"))
//    val outWikiWriter = IOUtils.openOutHard(Execution.getFile("output-wiki.conll"))
//    Logger.startTrack("Decoding");
//    for (i <- (0 until jointDevDocs.size)) {
//      Logger.logss("Decoding " + i);
//      val jointDevDoc = jointDevDocs(i);
//      val (backptrs, clustering, nerChunks, wikiChunks) = computer.viterbiDecodeProduceAnnotations(jointDevDoc, finalWeights);
//      val goldNerChunks = jointDevDoc.goldChunks;
//      val predClusteringBound = new OrderedClusteringBound(jointDevDoc.docGraph.getMentions, clustering)
//      val goldClusteringBound = new OrderedClusteringBound(jointDevDoc.docGraph.corefDoc.goldMentions, jointDevDoc.docGraph.corefDoc.goldClustering)
//      if (predWriter.isDefined) ConllDocWriter.writeDocWithPredAnnotations(predWriter.get, jointDevDoc.rawDoc, nerChunks, predClusteringBound, Some(wikiChunks));
//      if (goldWriter.isDefined) {
//        val goldWikiAnnotsToWrite = Some(jointDevDoc.goldWikiChunks.map(_.map(chunk => {
//          new Chunk[String](chunk.start, chunk.end, if (chunk.label.size == 0) NilToken else chunk.label(0).replace("_", " ").toLowerCase)
//        })));
//        ConllDocWriter.writeDocWithPredAnnotations(goldWriter.get, jointDevDoc.rawDoc, jointDevDoc.goldChunks, goldClusteringBound, goldWikiAnnotsToWrite);
//      }
//      allPredBackptrsAndClusterings += (backptrs -> clustering);
//      allPredNEChunks += nerChunks;
//      allPredWikiChunks ++= wikiChunks;
//      allPredWikiTitles += WikificationEvaluator.convertChunksToBagOfTitles(wikiChunks);
//      if (predWikiWriter.isDefined) {
//        WikificationEvaluator.writeWikificationRightAndWrong(predWikiWriter.get, predWikiWriterAux.get, jointDevDoc, jointDevDoc.goldWikiChunks,
//                                                                  maybeRawChunkNames.get, jointDevDoc.rawDoc.docID, wikiChunks, wikiLabelsInTrain);
//      }
//      ConllDocWriter.writeDocWithPredAnnotationsWikiStandoff(outWriter, outWikiWriter, jointDevDoc.rawDoc, nerChunks, clustering.bind(jointDevDoc.docGraph.getMentions, Driver.doConllPostprocessing), wikiChunks);
//    }
//    outWriter.close();
//    outWikiWriter.close();
//    if (Driver.writeNerOutput) {
//      NEEvaluator.writeIllinoisNEROutput(Execution.getFile("ner.txt"), jointDevDocs.flatMap(_.rawDoc.words), allPredNEChunks.flatten);
//    }
//    
//    Logger.endTrack();
//    Logger.logss("MENTION DETECTION")
//    CorefDoc.displayMentionPRF1(jointDevDocs.map(_.docGraph.corefDoc));
//    Logger.logss("COREF");
//    Logger.logss(CorefEvaluator.evaluateAndRender(jointDevDocs.map(_.docGraph), allPredBackptrsAndClusterings.map(_._1), allPredBackptrsAndClusterings.map(_._2),
//                                                  Driver.conllEvalScriptPath, "DEV: ", Driver.analysesToPrint));
//    Logger.logss("NER");
//    NEEvaluator.evaluateChunksBySent(jointDevDocs.flatMap(_.goldChunks), allPredNEChunks.flatten);
//    Logger.logss("WIKIFICATION");
//    WikificationEvaluator.evaluateWikiChunksBySent(jointDevDocs.flatMap(_.goldWikiChunks), allPredWikiChunks);
//    WikificationEvaluator.evaluateBOTF1(jointDevDocs.map(doc => WikificationEvaluator.convertSeqChunksToBagOfTitles(doc.goldWikiChunks)), allPredWikiTitles);
//    WikificationEvaluator.evaluateFahrniMetrics(jointDevDocs.flatMap(_.goldWikiChunks), allPredWikiChunks, wikiLabelsInTrain)
//    if (predWriter.isDefined) predWriter.get.close
//    if (goldWriter.isDefined) goldWriter.get.close
//    if (predWikiWriter.isDefined) {
//      predWikiWriter.get.close
//      predWikiWriterAux.get.close
//    }
//  }
//  
////  def predictACE(testPath: String, testSize: Int, jointPredictor: JointPredictor[MCNerFeaturizer], maybeWikipediaInterface: Option[WikipediaInterface]) {
////    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(Driver.numberGenderDataPath);
////    val mentionPropertyComputer = new MentionPropertyComputer(Some(numberGenderComputer));
////    
////    val fgfAce = new FactorGraphFactoryACE(jointPredictor.Featurizer, maybeWikipediaInterface);
////    val computer = new JointComputerShared(fgfAce);
////    ///////////////////////
////    // Evaluation of each part of the model
////    // Build dev docs
////    val jointDevDocs = preprocessACEDocsForEverything(testPath, testSize, mentionPropertyComputer, foldMapping, models, Driver.wikiGoldPath, false);
////    
////    val allPredBackptrsAndClusterings = new ArrayBuffer[(Array[Int],OrderedClustering)];
////    val allPredNEChunks = new ArrayBuffer[Seq[Seq[Chunk[String]]]];
////    val allPredWikiChunks = new ArrayBuffer[Seq[Chunk[String]]];
////    val allPredWikiTitles = new ArrayBuffer[Set[String]];
////    val predWriter = if (Driver.outputPath != "") Some(IOUtils.openOutHard(Driver.outputPath)) else None;
////    val goldWriter = if (Driver.writeGold) Some(IOUtils.openOutHard(Execution.getFile("gold.conll"))) else None;
////    val predWikiWriter = if (Driver.writeWikiOutput) Some(IOUtils.openOutHard(Execution.getFile("wiki.txt"))) else None;
////    val predWikiWriterAux = if (Driver.writeWikiOutput) Some(IOUtils.openOutHard(Execution.getFile("wiki-aux.txt"))) else None;
////    val maybeRawChunkNames = if (Driver.writeWikiOutput) Some(ACEMunger.mungeACEToGetChunkLabels(Driver.allAcePath, Driver.rawWikiGoldPath)) else None
////    val wikiLabelsInTrain: Set[String] = jointDocs.flatMap(_.goldWikiChunks.flatMap(_.flatMap(_.label)).toSet).toSet;
////    val outWriter = IOUtils.openOutHard(Execution.getFile("output.conll"))
////    val outWikiWriter = IOUtils.openOutHard(Execution.getFile("output-wiki.conll"))
////    Logger.startTrack("Decoding");
////    for (i <- (0 until jointDevDocs.size)) {
////      Logger.logss("Decoding " + i);
////      val jointDevDoc = jointDevDocs(i);
////      val (backptrs, clustering, nerChunks, wikiChunks) = computer.viterbiDecodeProduceAnnotations(jointDevDoc, finalWeights);
////      val goldNerChunks = jointDevDoc.goldChunks;
////      val predClusteringBound = new OrderedClusteringBound(jointDevDoc.docGraph.getMentions, clustering)
////      val goldClusteringBound = new OrderedClusteringBound(jointDevDoc.docGraph.corefDoc.goldMentions, jointDevDoc.docGraph.corefDoc.goldClustering)
////      if (predWriter.isDefined) ConllDocWriter.writeDocWithPredAnnotations(predWriter.get, jointDevDoc.rawDoc, nerChunks, predClusteringBound, Some(wikiChunks));
////      if (goldWriter.isDefined) {
////        val goldWikiAnnotsToWrite = Some(jointDevDoc.goldWikiChunks.map(_.map(chunk => {
////          new Chunk[String](chunk.start, chunk.end, if (chunk.label.size == 0) NilToken else chunk.label(0).replace("_", " ").toLowerCase)
////        })));
////        ConllDocWriter.writeDocWithPredAnnotations(goldWriter.get, jointDevDoc.rawDoc, jointDevDoc.goldChunks, goldClusteringBound, goldWikiAnnotsToWrite);
////      }
////      allPredBackptrsAndClusterings += (backptrs -> clustering);
////      allPredNEChunks += nerChunks;
////      allPredWikiChunks ++= wikiChunks;
////      allPredWikiTitles += WikificationEvaluator.convertChunksToBagOfTitles(wikiChunks);
////      if (predWikiWriter.isDefined) {
////        WikificationEvaluator.writeWikificationRightAndWrong(predWikiWriter.get, predWikiWriterAux.get, jointDevDoc, jointDevDoc.goldWikiChunks,
////                                                                  maybeRawChunkNames.get, jointDevDoc.rawDoc.docID, wikiChunks, wikiLabelsInTrain);
////      }
////      ConllDocWriter.writeDocWithPredAnnotationsWikiStandoff(outWriter, outWikiWriter, jointDevDoc.rawDoc, nerChunks, clustering.bind(jointDevDoc.docGraph.getMentions, Driver.doConllPostprocessing), wikiChunks);
////    }
////    outWriter.close();
////    outWikiWriter.close();
////    if (Driver.writeNerOutput) {
////      NEEvaluator.writeIllinoisNEROutput(Execution.getFile("ner.txt"), jointDevDocs.flatMap(_.rawDoc.words), allPredNEChunks.flatten);
////    }
////    
////    Logger.endTrack();
////    Logger.logss("MENTION DETECTION")
////    CorefDoc.displayMentionPRF1(jointDevDocs.map(_.docGraph.corefDoc));
////    Logger.logss("COREF");
////    Logger.logss(CorefEvaluator.evaluateAndRender(jointDevDocs.map(_.docGraph), allPredBackptrsAndClusterings.map(_._1), allPredBackptrsAndClusterings.map(_._2),
////                                                  Driver.conllEvalScriptPath, "DEV: ", Driver.analysesToPrint));
////    Logger.logss("NER");
////    NEEvaluator.evaluateChunksBySent(jointDevDocs.flatMap(_.goldChunks), allPredNEChunks.flatten);
////    Logger.logss("WIKIFICATION");
////    WikificationEvaluator.evaluateWikiChunksBySent(jointDevDocs.flatMap(_.goldWikiChunks), allPredWikiChunks);
////    WikificationEvaluator.evaluateBOTF1(jointDevDocs.map(doc => WikificationEvaluator.convertSeqChunksToBagOfTitles(doc.goldWikiChunks)), allPredWikiTitles);
//////    WikificationEvaluator.computeOracle(jointDevDocs.map(devDoc => computer.instantiateGraph(devDoc, false, false)))
////    WikificationEvaluator.evaluateFahrniMetrics(jointDevDocs.flatMap(_.goldWikiChunks), allPredWikiChunks, wikiLabelsInTrain)
////    if (predWriter.isDefined) predWriter.get.close
////    if (goldWriter.isDefined) goldWriter.get.close
////    if (predWikiWriter.isDefined) {
////      predWikiWriter.get.close
////      predWikiWriterAux.get.close
////      FahrniOutputAnalyzer.processLines(IOUtils.readLines(Execution.getFile("wiki.txt")).asScala, true);
////    }
////  }
//  
//  
//  
//  
//  
//  
//  def buildFeaturizerShared[T](trainDocs: Seq[CorefDoc], featureIndexer: Indexer[String], nerFeaturizer: T, maybeBrownClusters: Option[Map[String,String]]) = {
//    featureIndexer.getIndex(PairwiseIndexingFeaturizerJoint.UnkFeatName);
//    val queryCounts: Option[QueryCountsBundle] = None;
//    val lexicalCounts = LexicalCountsBundle.countLexicalItems(trainDocs, Driver.lexicalFeatCutoff);
//    val semClasser: Option[SemClasser] = Some(new BasicWordNetSemClasser);
//    val corefFeatureSetSpec = FeatureSetSpecification(Driver.pairwiseFeats, Driver.conjScheme, Driver.conjFeats, Driver.conjMentionTypes, Driver.conjTemplates);
//    val corefFeaturizer = new PairwiseIndexingFeaturizerJoint(featureIndexer, corefFeatureSetSpec, lexicalCounts, queryCounts, semClasser);
//    new JointFeaturizerShared[T](corefFeaturizer, nerFeaturizer, maybeBrownClusters, Driver.corefNerFeatures, Driver.corefWikiFeatures, Driver.wikiNerFeatures, featureIndexer)
//  }
//  
//  
//  
//  
//  /////////////
//  // DISTRIB //
//  /////////////
//  
////  def runTrainEvaluateDistrib(trainPath: String, trainSize: Int, testPath: String, testSize: Int) {
////    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(Driver.numberGenderDataPath);
////    val wikipediaInterface: Option[WikipediaInterface] = if (Driver.wikipediaPath != "") {
////      Some(GUtil.load(Driver.wikipediaPath).asInstanceOf[WikipediaInterface]); 
////    } else {
////      Logger.logss("Not loading Wikipedia file");
////      None;
////    }
////    val trainDocs = loadCorefDocs(trainPath, trainSize, Some(numberGenderComputer), false);
////    val trainDocsReordered = if (Driver.randomizeTrainOrder) new scala.util.Random(0).shuffle(trainDocs) else trainDocs;
////    val lexicalCounts = LexicalCountsBundle.countLexicalItems(trainDocs, Driver.lexicalFeatCutoff);
////    val trainDocGraphs = trainDocsReordered.map(new DocumentGraph(_, true));
////    preprocessDocsCacheResources(trainDocGraphs);
////    DocumentGraph.pruneEdgesAll(trainDocGraphs, new PruningStrategy(Driver.pruningStrategy), null);
////    
////    val featureIndexer = new Indexer[String]();
////    featureIndexer.getIndex(PairwiseIndexingFeaturizerJoint.UnkFeatName);
////    val featureSetSpec = FeatureSetSpecification(Driver.pairwiseFeats, Driver.conjScheme, Driver.conjFeats, Driver.conjMentionTypes, Driver.conjTemplates);
////    val basicFeaturizer = new PairwiseIndexingFeaturizerJoint(featureIndexer, featureSetSpec, lexicalCounts, None, wikipediaInterface, Some(new BasicWordNetSemClasser));
////    val featurizerTrainer = new CorefFeaturizerTrainer();
////    featurizerTrainer.featurizeBasic(trainDocGraphs, basicFeaturizer);
////    PairwiseIndexingFeaturizer.printFeatureTemplateCounts(featureIndexer)
////    
////    // Index component features
////    val componentIndexer = new Indexer[String];
////    val componentFeaturizer = new ComponentFeaturizer(componentIndexer, featureSetSpec, lexicalCounts, None, wikipediaInterface, Some(new BasicWordNetSemClasser));
////    val trainDocGraphsComponents = trainDocGraphs.map(doc => DocumentGraphComponents.cacheComponents(doc, componentFeaturizer, true));
////
////    val corefLossFcn = PairwiseLossFunctions(Driver.lossFcn);
////    val computer = new CorefComputerDistrib(corefLossFcn, featureIndexer, componentIndexer, Driver.distribSize);
////    val totalFeatures = featureIndexer.size + componentIndexer.size * Driver.distribSize
////    Logger.logss(featureIndexer.size + " sparse features, " + componentIndexer.size + " component features, " + totalFeatures + " total features");
////    
////    val finalWeights = new GeneralTrainer[DocumentGraphComponents].trainAdagrad(trainDocGraphsComponents, computer, totalFeatures, Driver.eta.toFloat, Driver.reg.toFloat, Driver.batchSize, Driver.numItrs);
////    var totalCompFeatureWeight = 0.0F;
////    var compFeaturesNonzero = 0;
////    val numCompFeatures = finalWeights.size - featureIndexer.size;
////    for (i <- featureIndexer.size until finalWeights.size) {
////      if (finalWeights(i) != 0) {
////        compFeaturesNonzero += 1;
////      }
////      totalCompFeatureWeight += Math.abs(finalWeights(i));
////    }
////    Logger.logss(compFeaturesNonzero + " / " + numCompFeatures + " component features nonzero, average magnitude is " + (totalCompFeatureWeight/numCompFeatures));
////    
////    val devDocs = loadCorefDocs(testPath, testSize, Some(numberGenderComputer), false);
////    val devDocGraphs = devDocs.map(new DocumentGraph(_, false));
////    preprocessDocsCacheResources(devDocGraphs);
////    featurizerTrainer.featurizeBasic(devDocGraphs, basicFeaturizer);
////    val devDocGraphsComponents = devDocGraphs.map(doc => DocumentGraphComponents.cacheComponents(doc, componentFeaturizer, false));
////    val (allPredBackptrs, allPredClusterings) = computer.viterbiDecodeAllFormClusterings(devDocGraphsComponents, finalWeights);
////    Logger.logss(CorefEvaluator.evaluateAndRender(devDocGraphs, allPredBackptrs, allPredClusterings, Driver.conllEvalScriptPath, "DEV: ", Driver.analysesToPrint));
////  }
}
