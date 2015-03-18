package edu.berkeley.nlp.entity

import edu.berkeley.nlp.entity.sem.BrownClusterInterface
import edu.berkeley.nlp.entity.wiki.WikificationEvaluator
import edu.berkeley.nlp.entity.coref.MentionPropertyComputer
import edu.berkeley.nlp.entity.sem.QueryCountsBundle
import edu.berkeley.nlp.entity.joint.JointDoc
import edu.berkeley.nlp.entity.joint.GeneralTrainer
import edu.berkeley.nlp.entity.joint.JointDocACE
import edu.berkeley.nlp.entity.joint.JointComputerShared
import edu.berkeley.nlp.entity.coref.NumberGenderComputer
import edu.berkeley.nlp.entity.sem.BasicWordNetSemClasser
import edu.berkeley.nlp.entity.coref.FeatureSetSpecification
import edu.berkeley.nlp.futile.fig.exec.Execution
import edu.berkeley.nlp.entity.coref.CorefEvaluator
import edu.berkeley.nlp.entity.ner.NerPrunerFromMarginals
import edu.berkeley.nlp.entity.coref.DocumentGraph
import edu.berkeley.nlp.entity.joint.FactorGraphFactoryACE
import edu.berkeley.nlp.entity.wiki.WikipediaInterface
import edu.berkeley.nlp.entity.joint.FactorGraphFactoryOnto
import edu.berkeley.nlp.entity.joint.JointPredictor
import edu.berkeley.nlp.entity.ner.MCNerFeaturizer
import edu.berkeley.nlp.entity.coref.CorefDoc
import edu.berkeley.nlp.entity.coref.CorefDocAssemblerACE
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.entity.joint.JointFeaturizerShared
import edu.berkeley.nlp.entity.sem.SemClasser
import edu.berkeley.nlp.entity.wiki.ACEMunger
import edu.berkeley.nlp.entity.wiki.DocWikiAnnots
import edu.berkeley.nlp.entity.ner.NerFeaturizer
import edu.berkeley.nlp.entity.coref.CorefPruner
import edu.berkeley.nlp.entity.coref.CorefDocAssembler
import edu.berkeley.nlp.entity.wiki.CorpusWikiAnnots
import edu.berkeley.nlp.entity.coref.LexicalCountsBundle
import edu.berkeley.nlp.entity.ner.NEEvaluator
import edu.berkeley.nlp.entity.wiki.WikiAnnotReaderWriter
import edu.berkeley.nlp.entity.coref.OrderedClusteringBound
import edu.berkeley.nlp.entity.coref.PairwiseIndexingFeaturizerJoint
import edu.berkeley.nlp.entity.ner.NerSystemLabeled
import edu.berkeley.nlp.entity.coref.PairwiseIndexingFeaturizer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.ner.NerPruner
import edu.berkeley.nlp.entity.joint.JointLossFcns
import edu.berkeley.nlp.entity.coref.PairwiseScorer
import edu.berkeley.nlp.entity.coref.OrderedClustering
import edu.berkeley.nlp.entity.coref.PairwiseLossFunctions
import edu.berkeley.nlp.entity.coref.UID
import edu.berkeley.nlp.entity.wiki._
import edu.berkeley.nlp.entity.joint.JointPredictorACE
import edu.berkeley.nlp.entity.coref.CorefSystem

object EntitySystem {
  
  def preprocessDocsCacheResources(allDocGraphs: Seq[DocumentGraph]) {
    if (Driver.wordNetPath != "") {
      val wni = new WordNetInterfacer(Driver.wordNetPath);
      allDocGraphs.foreach(_.cacheWordNetInterfacer(wni));
    }
  }
  
//  def preprocessDocs(path: String,
//                     size: Int,
//                     mentionPropertyComputer: MentionPropertyComputer,
//                     nerPruner: NerPruner,
//                     corefPruner: CorefPruner,
//                     train: Boolean) = {
//    // Read in raw data
//    val rawDocs = ConllDocReader.loadRawConllDocsWithSuffix(path, size, "auto_conll");
//    val goldConllDocs = ConllDocReader.loadRawConllDocsWithSuffix(path, size, "gold_conll");
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
//    jointDocsOrigOrder.foreach(_.cacheNerPruner(Some(nerPruner)));
//    if (train) {
//      // Randomize
//      new scala.util.Random(0).shuffle(jointDocsOrigOrder)
//    } else {
//      jointDocsOrigOrder;
//    }
//  }
//  
//  
//  def preprocessDocs(path: String,
//                     size: Int,
//                     suffix: String,
//                     mentionPropertyComputer: MentionPropertyComputer,
//                     corefPruner: CorefPruner,
//                     nerPruner: NerPruner) = {
//    // Read raw documents
//    val rawDocs = ConllDocReader.loadRawConllDocsWithSuffix(path, size, suffix);
//    // Get mentions
//    val assembler = CorefDocAssembler(Language.ENGLISH, useGoldMentions = false);
//    val corefDocs = rawDocs.map(doc => assembler.createCorefDoc(doc, mentionPropertyComputer));
//    val docGraphs = corefDocs.map(new DocumentGraph(_, false));
//    preprocessDocsCacheResources(docGraphs);
//    // Prune coreference
//    corefPruner.pruneAll(docGraphs);
//    // Build joint document and prune NER
//    val jointDocs = JointDoc.assembleJointDocs(docGraphs, new ArrayBuffer[ConllDoc](), new HashMap[String,HashMap[Int,ArrayBuffer[Chunk[String]]]]);
//    jointDocs.foreach(_.cacheNerPruner(Some(nerPruner)));
//    jointDocs;
//  }
  
  def preprocessDocsForTrain(path: String,
                             size: Int,
                             mentionPropertyComputer: MentionPropertyComputer,
                             nerPruner: NerPruner,
                             corefPruner: CorefPruner) = {
    preprocessDocs(path, "auto_conll", path, "gold_conll", size, mentionPropertyComputer, nerPruner, corefPruner, true);
  }
  
  def preprocessDocsForEval(path: String,
                            size: Int,
                            mentionPropertyComputer: MentionPropertyComputer,
                            nerPruner: NerPruner,
                            corefPruner: CorefPruner) = {
    preprocessDocs(path, "auto_conll", path, "gold_conll", size, mentionPropertyComputer, nerPruner, corefPruner, false);
  }
  
  def preprocessDocsForDecode(path: String,
                              size: Int,
                              suffix: String,
                              mentionPropertyComputer: MentionPropertyComputer,
                              nerPruner: NerPruner,
                              corefPruner: CorefPruner) = {
    preprocessDocs(path, suffix, "", "", size, mentionPropertyComputer, nerPruner, corefPruner, false);
  }
  
  def preprocessDocs(path: String,
                     suffix: String,
                     goldPath: String,
                     goldSuffix: String,
                     size: Int,
                     mentionPropertyComputer: MentionPropertyComputer,
                     nerPruner: NerPruner,
                     corefPruner: CorefPruner,
                     train: Boolean) = {
    // Read in raw data
    val (rawDocs, goldConllDocs) = if (goldPath != "") {
      (ConllDocReader.loadRawConllDocsWithSuffix(path, size, suffix),
       ConllDocReader.loadRawConllDocsWithSuffix(goldPath, size, goldSuffix));
    } else {
      (ConllDocReader.loadRawConllDocsWithSuffix(path, size, suffix),
       new ArrayBuffer[Document]());
    }
    val goldWikification = new HashMap[String,HashMap[Int,ArrayBuffer[Chunk[String]]]];
    val assembler = CorefDocAssembler(Driver.lang, Driver.useGoldMentions);
    val corefDocs = rawDocs.map(doc => assembler.createCorefDoc(doc, mentionPropertyComputer));
    if (train) {
      CorefDocAssembler.checkGoldMentionRecall(corefDocs);
    }
    val docGraphs = corefDocs.map(new DocumentGraph(_, train));
    preprocessDocsCacheResources(docGraphs);
    // Prune coref now that we have mentions
    corefPruner.pruneAll(docGraphs);
    
    val jointDocsOrigOrder = JointDoc.assembleJointDocs(docGraphs, goldConllDocs, goldWikification);
    // Store NER marginals
    jointDocsOrigOrder.foreach(_.cacheNerPruner(Some(nerPruner)));
    if (train) {
      // Randomize
      new scala.util.Random(0).shuffle(jointDocsOrigOrder)
    } else {
      jointDocsOrigOrder;
    }
  }
  
  def runOntoPredict(path: String, size: Int, modelPath: String) {
    val jointPredictor = GUtil.load(modelPath).asInstanceOf[JointPredictor];
    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(Driver.numberGenderDataPath);
    val mentionPropertyComputer = new MentionPropertyComputer(Some(numberGenderComputer));
    val maybeWikipediaInterface: Option[WikipediaInterface] = if (Driver.wikipediaPath != "") Some(GUtil.load(Driver.wikipediaPath).asInstanceOf[WikipediaInterface]) else None;
    val jointDocs = preprocessDocsForDecode(path, size, Driver.docSuffix, mentionPropertyComputer, jointPredictor.nerPruner, jointPredictor.corefPruner);
    jointPredictor.decodeWriteOutput(jointDocs, maybeWikipediaInterface, Driver.doConllPostprocessing);
  }
  
  def runOntoPredictEvaluate(path: String, size: Int, modelPath: String) {
    val jointPredictor = GUtil.load(modelPath).asInstanceOf[JointPredictor];
    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(Driver.numberGenderDataPath);
    val mentionPropertyComputer = new MentionPropertyComputer(Some(numberGenderComputer));
    val maybeWikipediaInterface: Option[WikipediaInterface] = if (Driver.wikipediaPath != "") Some(GUtil.load(Driver.wikipediaPath).asInstanceOf[WikipediaInterface]) else None;
    val jointDocs = preprocessDocsForEval(path, size, mentionPropertyComputer, jointPredictor.nerPruner, jointPredictor.corefPruner);
    jointPredictor.decodeWriteOutputEvaluate(jointDocs, maybeWikipediaInterface, Driver.doConllPostprocessing);
  }
  
  def runTrainEvaluate(trainPath: String, trainSize: Int, testPath: String, testSize: Int) = {
    // Resources needed for document assembly: number/gender computer, NER marginals, coref models and mapping of documents to folds
    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(Driver.numberGenderDataPath);
    val mentionPropertyComputer = new MentionPropertyComputer(Some(numberGenderComputer));
    
    // N.B. DIFFERENT DUE TO NER BEING PRESENT
    // Load NER pruning masks and coref models
    val nerPruner = NerPruner.buildPruner(Driver.nerPruningStrategy);
    val corefPruner = CorefPruner.buildPruner(Driver.pruningStrategy)
    val jointDocs = preprocessDocsForTrain(trainPath, trainSize, mentionPropertyComputer, nerPruner, corefPruner);
    
    // N.B. Only difference here is the NER
    ///////////////////////
    // Build the featurizer, which involves building specific featurizers for each task
    val featureIndexer = new Indexer[String];
    val maybeBrownClusters = if (Driver.brownPath != "") Some(BrownClusterInterface.loadBrownClusters(Driver.brownPath, 0)) else None
    val nerFeaturizer = NerFeaturizer(Driver.nerFeatureSet.split("\\+").toSet, featureIndexer, NerSystemLabeled.StdLabelIndexer, jointDocs.flatMap(_.rawDoc.words), None, maybeBrownClusters);
    val jointFeaturizer = buildFeaturizerShared(jointDocs.map(_.docGraph.corefDoc), featureIndexer, nerFeaturizer, maybeBrownClusters);
    val maybeWikipediaInterface: Option[WikipediaInterface] = if (Driver.wikipediaPath != "") Some(GUtil.load(Driver.wikipediaPath).asInstanceOf[WikipediaInterface]) else None;
    
    
    ///////////////////////
    val fgfOnto = new FactorGraphFactoryOnto(jointFeaturizer, maybeWikipediaInterface);
    val computer = new JointComputerShared(fgfOnto);
    jointDocs.foreach(jointDoc => {
      fgfOnto.getDocFactorGraph(jointDoc, true, true, true, PairwiseLossFunctions(Driver.lossFcn), JointLossFcns.nerLossFcn, JointLossFcns.wikiLossFcn);
      fgfOnto.getDocFactorGraph(jointDoc, false, true, true, PairwiseLossFunctions(Driver.lossFcn), JointLossFcns.nerLossFcn, JointLossFcns.wikiLossFcn);
    });
    PairwiseIndexingFeaturizer.printFeatureTemplateCounts(featureIndexer)
    Logger.logss(featureIndexer.size + " total features");
    
    val finalWeights = new GeneralTrainer[JointDoc].trainAdagrad(jointDocs, computer, featureIndexer.size, Driver.eta.toFloat, Driver.reg.toFloat, Driver.batchSize, Driver.numItrs);
    val model = new JointPredictor(jointFeaturizer, finalWeights, corefPruner, nerPruner).pack;
    if (Driver.modelPath != "") GUtil.save(model, Driver.modelPath);
    
    ///////////////////////
    // Evaluation of each part of the model
    // Build dev docs
    val jointDevDocs = preprocessDocsForEval(testPath, testSize, mentionPropertyComputer, nerPruner, corefPruner);
    model.decodeWriteOutputEvaluate(jointDevDocs, maybeWikipediaInterface, Driver.doConllPostprocessing);
  }
  
  
  //////////////////////////////
  //////////// ACE /////////////
  //////////////////////////////
  
  // N.B. Doubles with JointPredictor.preprocessACEDocs
  def preprocessACEDocsForTrainEval(path: String,
                                    size: Int,
                                    mentionPropertyComputer: MentionPropertyComputer,
                                    corefPruner: CorefPruner,
                                    wikiPath: String,
                                    train: Boolean) = {
    // Read in raw data
    val rawDocs = ConllDocReader.loadRawConllDocsWithSuffix(path, size, "", Language.ENGLISH);
    val goldWikification: CorpusWikiAnnots = if (wikiPath != "") {
      val corpusAnnots = new CorpusWikiAnnots;
      for (entry <- WikiAnnotReaderWriter.readAllStandoffAnnots(wikiPath)) {
        val fileName = entry._1._1;
        val docAnnots = new DocWikiAnnots;
        for (i <- 0 until entry._2.size) {
//          if (!entry._2(i).isEmpty) {
          docAnnots += i -> (new ArrayBuffer[Chunk[Seq[String]]]() ++ entry._2(i))
//          }
        }
        corpusAnnots += fileName -> docAnnots
      }
      corpusAnnots;
    } else {
      Logger.logss("Wikification not loaded");
      new CorpusWikiAnnots;
    }
    val corefDocs = if (Driver.useGoldMentions) {
      val assembler = CorefDocAssembler(Driver.lang, true);
      rawDocs.map(doc => assembler.createCorefDoc(doc, mentionPropertyComputer));
    } else {
      val assembler = new CorefDocAssemblerACE(Driver.allAcePath);
      rawDocs.map(doc => assembler.createCorefDoc(doc, mentionPropertyComputer));
    }
    CorefDocAssembler.checkGoldMentionRecall(corefDocs);
    val docGraphs = corefDocs.map(new DocumentGraph(_, train));
    preprocessDocsCacheResources(docGraphs);
    // Prune coref now that we have mentions
    corefPruner.pruneAll(docGraphs);
    
    
    val jointDocsOrigOrder = JointDocACE.assembleJointDocs(docGraphs, goldWikification);
    // TODO: Apply NER pruning
//    JointDoc.applyNerPruning(jointDocsOrigOrder, nerMarginals);
    if (train) {
      // Randomize
      new scala.util.Random(0).shuffle(jointDocsOrigOrder)
    } else {
      jointDocsOrigOrder;
    }
  }
  
  def preprocessACEDocsForDecode(path: String, size: Int, suffix: String, mentionPropertyComputer: MentionPropertyComputer, corefPruner: CorefPruner) = {
    val rawDocs = ConllDocReader.loadRawConllDocsWithSuffix(path, size, suffix, Language.ENGLISH);
    val assembler = CorefDocAssembler(Driver.lang, Driver.useGoldMentions);
    val corefDocs = rawDocs.map(doc => assembler.createCorefDoc(doc, mentionPropertyComputer));
    val docGraphs = corefDocs.map(new DocumentGraph(_, false));
    CorefSystem.preprocessDocsCacheResources(docGraphs);
    // Prune coref now that we have mentions
    corefPruner.pruneAll(docGraphs);
    // No NER pruning
    JointDocACE.assembleJointDocs(docGraphs, new CorpusWikiAnnots);
  }
  
  def runACEPredict(path: String, size: Int, modelPath: String) {
    val jointPredictor = GUtil.load(modelPath).asInstanceOf[JointPredictorACE];
    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(Driver.numberGenderDataPath);
    val mentionPropertyComputer = new MentionPropertyComputer(Some(numberGenderComputer));
    val maybeWikipediaInterface: Option[WikipediaInterface] = if (Driver.wikipediaPath != "") Some(GUtil.load(Driver.wikipediaPath).asInstanceOf[WikipediaInterface]) else None;
    val jointDocs = preprocessACEDocsForDecode(path, size, Driver.docSuffix, mentionPropertyComputer, jointPredictor.corefPruner);
    jointPredictor.decodeWriteOutput(jointDocs, maybeWikipediaInterface, Driver.doConllPostprocessing);
  }
  
  def runACEPredictEvaluate(path: String, size: Int, modelPath: String) {
    val jointPredictor = GUtil.load(modelPath).asInstanceOf[JointPredictorACE];
    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(Driver.numberGenderDataPath);
    val mentionPropertyComputer = new MentionPropertyComputer(Some(numberGenderComputer));
    val maybeWikipediaInterface: Option[WikipediaInterface] = if (Driver.wikipediaPath != "") Some(GUtil.load(Driver.wikipediaPath).asInstanceOf[WikipediaInterface]) else None;
    val jointDocs = preprocessACEDocsForTrainEval(path, size, mentionPropertyComputer, jointPredictor.corefPruner, Driver.wikiGoldPath, false);
    jointPredictor.decodeWriteOutputEvaluate(jointDocs, maybeWikipediaInterface, Driver.doConllPostprocessing);
  }
  
  def runTrainEvaluateACE(trainPath: String, trainSize: Int, testPath: String, testSize: Int) = {
    // Resources needed for document assembly: number/gender computer, NER marginals, coref models and mapping of documents to folds
    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(Driver.numberGenderDataPath);
    val mentionPropertyComputer = new MentionPropertyComputer(Some(numberGenderComputer));
    
    // Load coref models
    val corefPruner = CorefPruner.buildPruner(Driver.pruningStrategy)
    val jointDocs = preprocessACEDocsForTrainEval(trainPath, trainSize, mentionPropertyComputer, corefPruner, Driver.wikiGoldPath, true);
    // TODO: Are NER models necessary?
    
    ///////////////////////
    // Build the featurizer, which involves building specific featurizers for each task
    val featureIndexer = new Indexer[String]();
    val maybeBrownClusters = if (Driver.brownPath != "") Some(BrownClusterInterface.loadBrownClusters(Driver.brownPath, 0)) else None
    val nerFeaturizer = MCNerFeaturizer(Driver.nerFeatureSet.split("\\+").toSet, featureIndexer, MCNerFeaturizer.StdLabelIndexer, jointDocs.flatMap(_.rawDoc.words), None, maybeBrownClusters)
    val jointFeaturizer = buildFeaturizerShared(jointDocs.map(_.docGraph.corefDoc), featureIndexer, nerFeaturizer, maybeBrownClusters);
    val maybeWikipediaInterface: Option[WikipediaInterface] = if (Driver.wikipediaPath != "") Some(GUtil.load(Driver.wikipediaPath).asInstanceOf[WikipediaInterface]) else None;
    
    ///////////////////////
    // Cache features
    val fgfAce = new FactorGraphFactoryACE(jointFeaturizer, maybeWikipediaInterface);
    val computer = new JointComputerShared(fgfAce);
    jointDocs.foreach(jointDoc => {
      fgfAce.getDocFactorGraph(jointDoc, true, true, true, PairwiseLossFunctions(Driver.lossFcn), JointLossFcns.nerLossFcn, JointLossFcns.wikiLossFcn);
      fgfAce.getDocFactorGraph(jointDoc, false, true, true, PairwiseLossFunctions(Driver.lossFcn), JointLossFcns.nerLossFcn, JointLossFcns.wikiLossFcn);
    });
    PairwiseIndexingFeaturizer.printFeatureTemplateCounts(featureIndexer)
    Logger.logss(featureIndexer.size + " total features");
    
    val finalWeights = new GeneralTrainer[JointDocACE].trainAdagrad(jointDocs, computer, featureIndexer.size, Driver.eta.toFloat, Driver.reg.toFloat, Driver.batchSize, Driver.numItrs);
    val model = new JointPredictorACE(jointFeaturizer, finalWeights, corefPruner).pack;
    if (Driver.modelPath != "") GUtil.save(model, Driver.modelPath);
    
    ///////////////////////
    // Evaluation of each part of the model
    // Build dev docs
    val jointDevDocs = preprocessACEDocsForTrainEval(testPath, testSize, mentionPropertyComputer, corefPruner, Driver.wikiGoldPath, false);
    val wikiLabelsInTrain: Set[String] = jointDocs.flatMap(_.goldWikiChunks.flatMap(_.flatMap(_.label)).toSet).toSet;
    model.decodeWriteOutputEvaluate(jointDevDocs, maybeWikipediaInterface, Driver.doConllPostprocessing, wikiLabelsInTrain)
  }
  
  def buildFeaturizerShared[T](trainDocs: Seq[CorefDoc], featureIndexer: Indexer[String], nerFeaturizer: T, maybeBrownClusters: Option[Map[String,String]]) = {
    featureIndexer.getIndex(PairwiseIndexingFeaturizerJoint.UnkFeatName);
    val queryCounts: Option[QueryCountsBundle] = None;
    val lexicalCounts = LexicalCountsBundle.countLexicalItems(trainDocs, Driver.lexicalFeatCutoff);
    val semClasser: Option[SemClasser] = Some(new BasicWordNetSemClasser);
    val corefFeatureSetSpec = FeatureSetSpecification(Driver.pairwiseFeats, Driver.conjScheme, Driver.conjFeats, Driver.conjMentionTypes, Driver.conjTemplates);
    val corefFeaturizer = new PairwiseIndexingFeaturizerJoint(featureIndexer, corefFeatureSetSpec, lexicalCounts, queryCounts, semClasser);
    new JointFeaturizerShared[T](corefFeaturizer, nerFeaturizer, maybeBrownClusters, Driver.corefNerFeatures, Driver.corefWikiFeatures, Driver.wikiNerFeatures, featureIndexer)
  }
}