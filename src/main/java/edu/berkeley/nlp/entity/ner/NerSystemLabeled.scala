package edu.berkeley.nlp.entity.ner
import edu.berkeley.nlp.futile.fig.basic.Indexer
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._
import edu.berkeley.nlp.entity.ConllDoc
import edu.berkeley.nlp.futile.classify.GeneralLogisticRegression
import edu.berkeley.nlp.entity.coref.CorefSystem
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.futile.classify.SequenceExample
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import java.io.FileInputStream
import java.io.ObjectInputStream
import java.io.File
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.entity.Chunk
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.entity.ConllDocReader
import edu.berkeley.nlp.entity.lang.Language
import scala.util.Random
import edu.berkeley.nlp.entity.ConllDocWriter
import edu.berkeley.nlp.math.SloppyMath
import edu.berkeley.nlp.entity.wiki.WikipediaInterface
import edu.berkeley.nlp.entity.coref.UID
import edu.berkeley.nlp.entity.sem.BrownClusterInterface

@SerialVersionUID(1L)
class NerSystemLabeled(val labelIndexer: Indexer[String],
                       val featurizedTransitionMatrix: Array[Array[Array[Int]]],
                       val featurizer: NerFeaturizer,
                       val weights: Array[Double]) extends Serializable {
  
  val reducedLabelSetSize = NerSystemLabeled.LabelSetReduced.size;
  var cachedStartMarginal = Array.tabulate(reducedLabelSetSize)(i => 0.0F);
  var cachedJointProbs = Array.tabulate(reducedLabelSetSize, reducedLabelSetSize)((i, j) => 0.0F);
  var cachedVariationalProbs = Array.tabulate(50, reducedLabelSetSize, reducedLabelSetSize)((i, j, k) => 0.0F);
  
  def tagBIO(sentenceWords: Array[String], sentencePos: Array[String]): Array[String] = {
    val example = new NerExample(sentenceWords, sentencePos, null);
    val seqExample = new SequenceExample(featurizedTransitionMatrix, featurizer.featurize(example, false), null);
    seqExample.decode(weights).map(labelIndexer.getObject(_));
  }
  
  def chunk(sentenceWords: Array[String], sentencePos: Array[String]): Seq[Chunk[String]] = {
    val example = new NerExample(sentenceWords, sentencePos, null);
    val seqExample = new SequenceExample(featurizedTransitionMatrix, featurizer.featurize(example, false), null);
    NerSystemLabeled.convertToLabeledChunks(seqExample.decode(weights).map(labelIndexer.getObject(_)));
  }
  
  def computeLogMarginals(sentenceWords: Array[String], sentencePos: Array[String]): Array[Array[Float]] = {
    val example = new NerExample(sentenceWords, sentencePos, null);
    val seqExample = new SequenceExample(featurizedTransitionMatrix, featurizer.featurize(example, false), null);
    seqExample.getLogMarginals(weights);
  }
  
  // Computes the optimal chunks and computes marginals for each, thereby maintaining
  // uncertainty about labels but certainty about bundaries
  def chunkWithMarginals(sentenceWords: Array[String], sentencePos: Array[String]): Seq[Chunk[Counter[String]]] = {
    val example = new NerExample(sentenceWords, sentencePos, null);
    val seqExample = new SequenceExample(featurizedTransitionMatrix, featurizer.featurize(example, false), null);
    val chunkSeq = if (NerDriver.variational) {
      seqExample.computeForwardBackwardProbs(weights, true);
      variationalChunk(seqExample);
    } else {
      val chunks = NerSystemLabeled.convertToLabeledChunks(seqExample.decode(weights).map(labelIndexer.getObject(_)));
      // Need the sum probs now!
      seqExample.computeForwardBackwardProbs(weights, true);
      chunks;
    }
    // Now extract top-5 marginals from the decodes
    val marginalsChunkSeq = chunkSeq.map(chunk => {
      val topLabels = new Counter[String];
      for (tag <- NerSystemLabeled.TagSet) {
        val indexedSymbols = (0 until chunk.end - chunk.start).map(i => {
          if (i == 0) labelIndexer.indexOf("B-" + tag) else labelIndexer.indexOf("I-" + tag)
        });
        val marginal = seqExample.getSubsequenceMarginal(chunk.start, indexedSymbols.toArray);
        topLabels.incrementCount(tag, marginal);
      }
      topLabels.pruneKeysBelowThreshold(-5);
      new Chunk(chunk.start, chunk.end, topLabels);
    });
//    Logger.logss(sentenceWords.toSeq);
//    Logger.logss(marginalsChunkSeq);
    marginalsChunkSeq;
  }
  
  private def variationalChunk(seqExample: SequenceExample): Seq[Chunk[String]] = {
    // Compute the variational approximation to the true unlabeled chunk marginal distribution
    if (seqExample.getNumTokens() > cachedVariationalProbs.size) {
      cachedVariationalProbs = Array.tabulate(seqExample.getNumTokens(), reducedLabelSetSize, reducedLabelSetSize)((i, j, k) => 0.0F);
    }
    for (label <- labelIndexer.getObjects().asScala) {
      val reducedLabelIdx = NerSystemLabeled.LabelSetReduced.indexOf(label.substring(0, 1))
          cachedStartMarginal(reducedLabelIdx) += seqExample.getSubsequenceMarginal(0, Array(reducedLabelIdx));
    }
    var normalizer = SloppyMath.logAdd(cachedStartMarginal);
    for (i <- 0 until cachedStartMarginal.size) {
      cachedStartMarginal(i) -= normalizer.toFloat;
    }
    for (idx <- 0 until seqExample.getNumTokens() - 1) {
      for (i <- 0 until reducedLabelSetSize) {
        for (j <- 0 until reducedLabelSetSize) {
          cachedJointProbs(i)(j) = 0.0F;
        }
      }
      for (prevLabel <- labelIndexer.getObjects().asScala) {
        val prevReducedLabelIdx = NerSystemLabeled.LabelSetReduced.indexOf(prevLabel.substring(0, 1))
        for (currLabel <- labelIndexer.getObjects().asScala) {
          val currReducedLabelIdx = NerSystemLabeled.LabelSetReduced.indexOf(currLabel.substring(0, 1))
          cachedJointProbs(prevReducedLabelIdx)(currReducedLabelIdx) = SloppyMath.logAdd(cachedJointProbs(prevReducedLabelIdx)(currReducedLabelIdx),
                                                                                         seqExample.getSubsequenceMarginal(idx, Array(prevReducedLabelIdx, currReducedLabelIdx)));
        }
      }
      for (prevReducedLabelIdx <- 0 until reducedLabelSetSize) {
        var normalizer = 0.0F;
        for (currReducedLabelIdx <- 0 until reducedLabelSetSize) {
          normalizer += cachedJointProbs(prevReducedLabelIdx)(currReducedLabelIdx);
        }
        for (currReducedLabelIdx <- 0 until reducedLabelSetSize) {
          cachedVariationalProbs(idx)(prevReducedLabelIdx)(currReducedLabelIdx) = cachedJointProbs(prevReducedLabelIdx)(currReducedLabelIdx)/normalizer;
        }
      }
    }
    NerSystemLabeled.convertToLabeledChunks(SequenceExample.getBestPath(cachedStartMarginal, cachedVariationalProbs).map(NerSystemLabeled.LabelSetReduced(_)))
  }
  
  def pack: NerSystemLabeled = {
    val (newFeatureIndexer, newWeights) = GUtil.packFeaturesAndWeights(featurizer.featureIndexer, weights);
    Logger.logss("Packing NER model from " + weights.size + " to " + newWeights.size)
    val newFeaturizer = new NerFeaturizer(featurizer.featureSet, newFeatureIndexer, featurizer.labelIndexer, featurizer.corpusCounts, featurizer.wikipediaDB, featurizer.brownClusters) 
    val newFeaturizedTransitionMatrix = Array.tabulate(labelIndexer.size, labelIndexer.size)((prev, curr) => {
      newFeaturizer.featurizeTransition(labelIndexer.getObject(prev), labelIndexer.getObject(curr), true);
    });
    new NerSystemLabeled(labelIndexer, newFeaturizedTransitionMatrix, newFeaturizer, newWeights);
  }
}

object NerSystemLabeled {
  val TagSet = IndexedSeq("CARDINAL", "DATE", "EVENT", "FAC", "GPE", "LANGUAGE", "LAW", "LOC", "MONEY",
                          "NORP", "ORDINAL", "ORG", "PERCENT", "PERSON", "PRODUCT", "QUANTITY", "TIME", "WORK_OF_ART");
  val LabelSetReduced = IndexedSeq("B", "I", "O");
  val StdLabelIndexer = new Indexer[String]();
  StdLabelIndexer.add("O");
  for (tag <- TagSet) {
    StdLabelIndexer.add("B-" + tag);
    StdLabelIndexer.add("I-" + tag);
  }
  
  // Designed to handle both ACE classes like "VEH", "FAC", etc. (multiclass), along with
  // OntoNotes tag sequences like "B-ORG", "I-PER", etc.
  // Structural type for the former is always "B", latter is the correct thing
  def getStructuralType(nerSymbol: String) = {
    if (nerSymbol.contains("-")) {
      nerSymbol.substring(0, 1);
    } else if (nerSymbol == "O") {
      "O"
    } else {
      // Should only fire for ACE-like classes
      "B";
    }
  }
  
  // Designed to handle both ACE classes like "VEH", "FAC", etc. (multiclass), along with
  // OntoNotes tag sequences like "B-ORG", "I-PER", etc.
  // Semantic type is whatever comes after the hyphen or the entire thing if no hyphen
  def getSemanticType(nerSymbol: String) = {
    if (nerSymbol.contains("-")) {
      require(getStructuralType(nerSymbol) != "O");
      nerSymbol.substring(nerSymbol.indexOf("-") + 1);
    } else {
      nerSymbol
    }
  }
  
  def loadNerSystem(modelPath: String) = {
    var nerSystem: NerSystemLabeled = null;
    try {
      val fileIn = new FileInputStream(new File(modelPath));
      val in = new ObjectInputStream(fileIn);
      nerSystem = in.readObject().asInstanceOf[NerSystemLabeled]
      Logger.logss("Model read from " + modelPath);
      in.close();
      fileIn.close();
    } catch {
      case e: Exception => throw new RuntimeException(e);
    }
    nerSystem;
  }
  
//  def featurizeTransitionMatrix(transitionMatrix: Array[Array[Array[String]]], featureIndexer: Indexer[String]): Array[Array[Array[Int]]] = {
//    transitionMatrix.map(_.map(arr => if (arr != null) arr.map(featureIndexer.getIndex(_)) else null));
//  }
  
  def replaceNer(doc: ConllDoc, newChunks: Seq[Seq[Chunk[String]]]) = {
    new ConllDoc(doc.docID, doc.docPartNo, doc.words, doc.pos, doc.trees, newChunks, doc.corefChunks, doc.speakers);
  }
  
  def loadDocs(path: String, size: Int, usePredPos: Boolean) = {
    if (usePredPos) loadRawConllDocsPredWithGoldNer(path, size) else ConllDocReader.loadRawConllDocsWithSuffix(path, size, "gold_conll")
  }
  
  def loadRawConllDocsPredWithGoldNer(path: String, size: Int) = {
    val predDocs = ConllDocReader.loadRawConllDocsWithSuffix(path, size, "auto_conll")
    val goldDocs = ConllDocReader.loadRawConllDocsWithSuffix(path, size, "gold_conll")
    predDocs.zip(goldDocs).map(predAndGoldDoc => {
      val pred = predAndGoldDoc._1;
      val gold = predAndGoldDoc._2;
      if (pred.printableDocName != gold.printableDocName && pred.nerChunks.size != gold.nerChunks.size) {
        throw new RuntimeException("Error: on document " + pred.printableDocName + " / " + gold.printableDocName + ", there are " + pred.nerChunks.size + " pred sentences but " + gold.nerChunks.size + " gold sentences");
      }
      replaceNer(pred, gold.nerChunks); 
    });
  }
  
  def trainEvaluateNerSystem(trainPath: String, trainSize: Int, testPath: String, testSize: Int) {
    val maybeBrownClusters = if (NerDriver.brownClustersPath != "") Some(BrownClusterInterface.loadBrownClusters(NerDriver.brownClustersPath, 0)) else None;
    val trainDocs = loadDocs(trainPath, trainSize, NerDriver.usePredPos)
    val system = trainNerSystem(trainDocs, maybeBrownClusters, NerDriver.featureSet.split("\\+").toSet, NerDriver.reg, NerDriver.numItrs, NerDriver.batchSize);
    if (!NerDriver.modelPath.isEmpty) {
      GUtil.save(system, NerDriver.modelPath);
    }
    evaluateNerSystem(system, loadDocs(testPath, testSize, NerDriver.usePredPos));
  }
  
  // TRAINING
  
  def trainNerSystem(trainDocs: Seq[ConllDoc],
                     maybeBrownClusters: Option[Map[String,String]],
                     nerFeatureSet: Set[String],
                     reg: Double,
                     numItrs: Int, 
                     batchSize: Int) = {
    val labelIndexer = StdLabelIndexer;
    Logger.logss("Extracting training examples");
    val trainExamples = extractNerChunksFromConll(trainDocs);
    val maybeWikipediaDB = None;
    val featureIndexer = new Indexer[String]();
    val nerFeaturizer = NerFeaturizer(nerFeatureSet, featureIndexer, labelIndexer, trainExamples.map(_.words), maybeWikipediaDB, maybeBrownClusters, NerDriver.unigramThreshold, NerDriver.bigramThreshold, NerDriver.prefSuffThreshold);
    // Featurize transitions and then examples
    val featurizedTransitionMatrix = Array.tabulate(labelIndexer.size, labelIndexer.size)((prev, curr) => {
      nerFeaturizer.featurizeTransition(labelIndexer.getObject(prev), labelIndexer.getObject(curr), true);
    });
    Logger.startTrack("Featurizing");
    val trainSequenceExs = for (i <- 0 until trainExamples.size) yield {
      if (i % 100 == 0) {
        Logger.logss("Featurizing train example " + i);
      }
      val ex = trainExamples(i);
      new SequenceExample(featurizedTransitionMatrix, nerFeaturizer.featurize(ex, true), ex.goldLabels.map(labelIndexer.getIndex(_)).toArray);
    };
    Logger.endTrack();
    val featsByType = featureIndexer.getObjects().asScala.groupBy(str => str.substring(0, str.indexOf("=")));
    Logger.logss(featureIndexer.size + " features");
    // Train
    val weights = new Array[Double](featureIndexer.size);
    val eta = 1.0;
    new GeneralLogisticRegression(true, false).trainWeightsAdagradL1R(trainSequenceExs.asJava, reg, eta, numItrs, batchSize, weights);
    val system = new NerSystemLabeled(labelIndexer, featurizedTransitionMatrix, nerFeaturizer, weights).pack;
    val trainGoldChunks = trainSequenceExs.map(ex => convertToLabeledChunks(ex.goldLabels.map(labelIndexer.getObject(_))));
    val trainPredChunks = trainSequenceExs.map(ex => convertToLabeledChunks(ex.decode(weights).map(labelIndexer.getObject(_))));
    NEEvaluator.evaluateChunksBySent(trainGoldChunks, trainPredChunks);
    system;
  }
  
  // EVALUATION
  
  def evaluateNerSystem(nerSystem: NerSystemLabeled, testDocs: Seq[ConllDoc]) {
    val labelIndexer = nerSystem.labelIndexer;
    Logger.logss("Extracting test examples");
    val testExamples = extractNerChunksFromConll(testDocs);
    val testSequenceExs = for (i <- 0 until testExamples.size) yield {
      if (i % 100 == 0) {
        Logger.logss("Featurizing test example " + i);
      }
      val ex = testExamples(i);
      new SequenceExample(nerSystem.featurizedTransitionMatrix, nerSystem.featurizer.featurize(ex, false), ex.goldLabels.map(nerSystem.labelIndexer.getIndex(_)).toArray);
    };
    // Decode and check test set accuracy
    val testGoldChunks = testSequenceExs.map(ex => convertToLabeledChunks(ex.goldLabels.map(labelIndexer.getObject(_))));
    val testPredChunks = testSequenceExs.map(ex => convertToLabeledChunks(ex.decode(nerSystem.weights).map(labelIndexer.getObject(_))));
    NEEvaluator.evaluateChunksBySent(testGoldChunks, testPredChunks);
    if (NerDriver.outputPath != "") {
      Logger.logss("Writing output to " + NerDriver.outputPath)
      NEEvaluator.writeIllinoisNEROutput(NerDriver.outputPath, testExamples.map(_.words), testPredChunks)
    }
  }
  
  def convertToLabeledChunks(labelSeq: Seq[String]): Seq[Chunk[String]] = {
    val chunks = new ArrayBuffer[Chunk[String]];
    var i = 0;
    var inconsistent = false;
    while (i < labelSeq.size) {
      val structuralType = getStructuralType(labelSeq(i));
      if (structuralType == "B") {
        val semanticType = getSemanticType(labelSeq(i));
        val startIdx = i;
        i += 1;
        while (i < labelSeq.size && getStructuralType(labelSeq(i)) == "I") {
          if (getSemanticType(labelSeq(i)) != semanticType) {
            inconsistent = true;
          }
          i += 1;
        }
        chunks += new Chunk[String](startIdx, i, semanticType);
      } else if (structuralType == "I") {
        inconsistent = true;
        i += 1;
      } else {
        i += 1;
      }
    }
    if (inconsistent) {
      Logger.logss("WARNING: Inconsistent NER sequence: " + labelSeq);
    }
    chunks;
  }
  
  def convertToBIO(chunks: Seq[Chunk[String]], sentLen: Int): Seq[String] = {
    (0 until sentLen).map(i => getGoldNETag(chunks, i));
  }
  
  def getGoldNETag(chunks: Seq[Chunk[String]], tokIdx: Int) = {
    val maybeRelevantChunk = chunks.filter(chunk => chunk.start <= tokIdx && tokIdx < chunk.end);
    if (maybeRelevantChunk.isEmpty) {
      "O"
    } else {
      val chunk = maybeRelevantChunk.head;
      if (!TagSet.contains(chunk.label)) {
        throw new RuntimeException("ERROR: Tag set does not include " + chunk.label);
      }
      if (tokIdx == chunk.start) "B-" + chunk.label else "I-" + chunk.label;
    }
  }
  
  def extractNerChunksFromConll(docs: Seq[ConllDoc]): Seq[NerExample] = {
    val chunkTypeCounts = new Counter[String];
    val examples = docs.flatMap(doc => {
      val chunksToUse = doc.nerChunks
      (0 until chunksToUse.size).map(sentIdx => {
        val chunks = chunksToUse(sentIdx);
        chunks.foreach(chunk => chunkTypeCounts.incrementCount(chunk.label, 1.0));
        val labels = convertToBIO(chunks, doc.words(sentIdx).size);
        new NerExample(doc.words(sentIdx), doc.pos(sentIdx), labels);
      })
    });
    Logger.logss("Chunk counts: " + chunkTypeCounts);
    examples;
  }
  
  def trainPredictTokenMarginalsKFold(trainPath: String, trainSize: Int, brownPath: String, testPaths: Array[String], testSize: Int, numFolds: Int, marginalsOutPath: String, modelOutPath: String) = {
    val maybeBrownClusters = if (brownPath != "") Some(BrownClusterInterface.loadBrownClusters(brownPath, 0)) else None;
    val allDocs = loadRawConllDocsPredWithGoldNer(trainPath, trainSize)
    val rand = new Random(0);
    // Shuffle the doc IDs and assign them in equal measure to each fold
    val docIDsShuffled = rand.shuffle(allDocs.map(_.uid))
    val foldSize = docIDsShuffled.size/numFolds;
    val docIDsToFolds = new HashMap[UID,Int] ++ (0 until docIDsShuffled.size).map(i => docIDsShuffled(i) -> Math.min(i/foldSize, numFolds - 1));
    val docIDsToNerMarginals = new HashMap[UID,Seq[Array[Array[Float]]]];
    var lastSystem : NerSystemLabeled = null;
    for (fold <- (0 until numFolds)) {
      // Train on data outside the fold, evaluate on data inside the fold
      val trainingData = allDocs.filter(doc => docIDsToFolds(doc.uid) != fold);
      val evalData = allDocs.filter(doc => docIDsToFolds(doc.uid) == fold);
      Logger.logss("STARTING FOLD " + fold + ": training on " + trainingData.size + " documents, running on " + evalData.size + " documents");
      val system = trainNerSystem(trainingData, maybeBrownClusters, NerDriver.featureSet.split("\\+").toSet, NerDriver.reg, NerDriver.numItrs, NerDriver.batchSize);
      evalData.foreach(doc => {
        val marginals = (0 until doc.numSents).map(sentIdx => system.computeLogMarginals(doc.words(sentIdx).toArray, doc.pos(sentIdx).toArray));
        docIDsToNerMarginals.put(doc.uid, marginals);
      });
      if (fold == numFolds - 1) {
        lastSystem = system.pack;
      }
      Logger.logss(evalData.size + " train documents in fold " + fold + " processed");
      // Do the test set with the first fold
      if (fold == 0) {
        for (testPath <- testPaths) {
          val testDocs = loadRawConllDocsPredWithGoldNer(testPath, testSize)
          testDocs.foreach(doc => {
            val marginals = (0 until doc.numSents).map(sentIdx => system.computeLogMarginals(doc.words(sentIdx).toArray, doc.pos(sentIdx).toArray));
            docIDsToNerMarginals.put(doc.uid, marginals);
          });
          Logger.logss(testDocs.size + " test docs processed from " + testPath);
        }
      }
    }
    Logger.logss("Writing marginals for " + docIDsToNerMarginals.size + " docs");
    if (marginalsOutPath != "") GUtil.save(docIDsToNerMarginals, marginalsOutPath);
    if (modelOutPath != "") GUtil.save(lastSystem, modelOutPath);
    (docIDsToNerMarginals, lastSystem);
  }
}
