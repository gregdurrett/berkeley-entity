package edu.berkeley.nlp.entity.joint

import edu.berkeley.nlp.entity.coref.PairwiseScorer
import edu.berkeley.nlp.entity.ner.NerSystemLabeled
import edu.berkeley.nlp.entity.coref.CorefPruner
import edu.berkeley.nlp.entity.ner.NerPruner
import edu.berkeley.nlp.entity.coref.MentionPropertyComputer
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.entity.coref.CorefSystem
import edu.berkeley.nlp.entity.ConllDocReader
import edu.berkeley.nlp.entity.coref.CorefDocAssembler
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.Chunk
import edu.berkeley.nlp.entity.Document
import edu.berkeley.nlp.entity.coref.DocumentGraph
import edu.berkeley.nlp.futile.fig.exec.Execution
import edu.berkeley.nlp.entity.coref.CorefEvaluator
import edu.berkeley.nlp.entity.wiki.WikipediaInterface
import edu.berkeley.nlp.entity.ConllDocWriter
import edu.berkeley.nlp.entity.coref.OrderedClustering
import edu.berkeley.nlp.entity.ner.NerFeaturizer
import edu.berkeley.nlp.entity.ner.NEEvaluator
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.Driver
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.entity.coref.CorefDocAssemblerACE
import edu.berkeley.nlp.entity.ner.MCNerFeaturizer
import edu.berkeley.nlp.entity.wiki.CorpusWikiAnnots
import edu.berkeley.nlp.entity.coref.CorefDoc
import edu.berkeley.nlp.entity.wiki.ACEMunger
import edu.berkeley.nlp.entity.coref.OrderedClusteringBound
import edu.berkeley.nlp.entity.wiki.WikificationEvaluator
import edu.berkeley.nlp.entity.wiki._
import scala.collection.mutable.HashSet

@SerialVersionUID(1L)
class JointPredictorACE(val jointFeaturizer: JointFeaturizerShared[MCNerFeaturizer],
                        val weights: Array[Float],
                        val corefPruner: CorefPruner) extends Serializable {
  
  def decodeWriteOutput(jointTestDocs: Seq[JointDocACE], maybeWikipediaInterface: Option[WikipediaInterface], doConllPostprocessing: Boolean) {
    val fgfOnto = new FactorGraphFactoryACE(jointFeaturizer, maybeWikipediaInterface);
    val computer = new JointComputerShared(fgfOnto);
    val outWriter = IOUtils.openOutHard(Execution.getFile("output.conll"))
    val outWikiWriter = IOUtils.openOutHard(Execution.getFile("output-wiki.conll"))
    Logger.startTrack("Decoding");
    for (i <- (0 until jointTestDocs.size)) {
      Logger.logss("Decoding " + i);
      val (backptrs, clustering, nerChunks, wikiChunks) = computer.viterbiDecodeProduceAnnotations(jointTestDocs(i), weights);
      ConllDocWriter.writeDocWithPredAnnotationsWikiStandoff(outWriter, outWikiWriter, jointTestDocs(i).rawDoc, nerChunks, clustering.bind(jointTestDocs(i).docGraph.getMentions, doConllPostprocessing), wikiChunks);
    }
    outWriter.close();
    outWikiWriter.close();
    Logger.endTrack();
  }
  
  def decodeWriteOutputEvaluate(jointTestDocs: Seq[JointDocACE], maybeWikipediaInterface: Option[WikipediaInterface], doConllPostprocessing: Boolean, wikiLabelsInTrain: Set[String] = Set[String]()) {
    val allPredBackptrsAndClusterings = new ArrayBuffer[(Array[Int],OrderedClustering)];
    val allPredNEChunks = new ArrayBuffer[Seq[Seq[Chunk[String]]]];
    val allPredWikiChunks = new ArrayBuffer[Seq[Chunk[String]]];
    val allPredWikiTitles = new ArrayBuffer[Set[String]];
    val predWriter = if (Driver.outputPath != "") Some(IOUtils.openOutHard(Driver.outputPath)) else None;
    val goldWriter = if (Driver.writeGold) Some(IOUtils.openOutHard(Execution.getFile("gold.conll"))) else None;
    val predWikiWriter = if (Driver.writeWikiOutput) Some(IOUtils.openOutHard(Execution.getFile("wiki.txt"))) else None;
    val predWikiWriterAux = if (Driver.writeWikiOutput) Some(IOUtils.openOutHard(Execution.getFile("wiki-aux.txt"))) else None;
    val maybeRawChunkNames = if (Driver.writeWikiOutput && Driver.allAcePath != "" && Driver.rawWikiGoldPath != "") {
      Some(ACEMunger.mungeACEToGetChunkLabels(Driver.allAcePath, Driver.rawWikiGoldPath))
    } else {
      None
    }
    val outWriter = IOUtils.openOutHard(Execution.getFile("output.conll"))
    val outWikiWriter = IOUtils.openOutHard(Execution.getFile("output-wiki.conll"))
    Logger.startTrack("Decoding");
    for (i <- (0 until jointTestDocs.size)) {
      Logger.logss("Decoding " + i);
      val jointDevDoc = jointTestDocs(i);
//      val (backptrs, clustering, nerChunks, wikiChunks) = computer.viterbiDecodeProduceAnnotations(jointDevDoc, finalWeights);
      val (backptrs, clustering, nerChunks, wikiChunks) = decode(jointDevDoc, maybeWikipediaInterface);
      val goldNerChunks = jointDevDoc.goldChunks;
      val predClusteringBound = new OrderedClusteringBound(jointDevDoc.docGraph.getMentions, clustering)
      val goldClusteringBound = new OrderedClusteringBound(jointDevDoc.docGraph.corefDoc.goldMentions, jointDevDoc.docGraph.corefDoc.goldClustering)
      if (predWriter.isDefined) ConllDocWriter.writeDocWithPredAnnotations(predWriter.get, jointDevDoc.rawDoc, nerChunks, predClusteringBound, Some(wikiChunks));
      if (goldWriter.isDefined) {
        val goldWikiAnnotsToWrite = Some(jointDevDoc.goldWikiChunks.map(_.map(chunk => {
          new Chunk[String](chunk.start, chunk.end, if (chunk.label.size == 0) NilToken else chunk.label(0).replace("_", " ").toLowerCase)
        })));
        ConllDocWriter.writeDocWithPredAnnotations(goldWriter.get, jointDevDoc.rawDoc, jointDevDoc.goldChunks, goldClusteringBound, goldWikiAnnotsToWrite);
      }
      allPredBackptrsAndClusterings += (backptrs -> clustering);
      allPredNEChunks += nerChunks;
      allPredWikiChunks ++= wikiChunks;
      allPredWikiTitles += WikificationEvaluator.convertChunksToBagOfTitles(wikiChunks);
      if (predWikiWriter.isDefined && maybeRawChunkNames.isDefined) {
        WikificationEvaluator.writeWikificationRightAndWrong(predWikiWriter.get, predWikiWriterAux.get, jointDevDoc, jointDevDoc.goldWikiChunks,
                                                                  maybeRawChunkNames.get, jointDevDoc.rawDoc.docID, wikiChunks, wikiLabelsInTrain);
      }
      ConllDocWriter.writeDocWithPredAnnotationsWikiStandoff(outWriter, outWikiWriter, jointDevDoc.rawDoc, nerChunks, clustering.bind(jointDevDoc.docGraph.getMentions, doConllPostprocessing), wikiChunks);
    }
    outWriter.close();
    outWikiWriter.close();
    if (Driver.writeNerOutput) {
      NEEvaluator.writeIllinoisNEROutput(Execution.getFile("ner.txt"), jointTestDocs.flatMap(_.rawDoc.words), allPredNEChunks.flatten);
    }
    
    Logger.endTrack();
    Logger.logss("MENTION DETECTION")
    CorefDoc.displayMentionPRF1(jointTestDocs.map(_.docGraph.corefDoc));
    Logger.logss("COREF");
    Logger.logss(CorefEvaluator.evaluateAndRender(jointTestDocs.map(_.docGraph), allPredBackptrsAndClusterings.map(_._1), allPredBackptrsAndClusterings.map(_._2),
                                                  Driver.conllEvalScriptPath, "DEV: ", Driver.analysesToPrint));
    Logger.logss("NER");
    NEEvaluator.evaluateChunksBySent(jointTestDocs.flatMap(_.goldChunks), allPredNEChunks.flatten);
    Logger.logss("WIKIFICATION");
    WikificationEvaluator.evaluateWikiChunksBySent(jointTestDocs.flatMap(_.goldWikiChunks), allPredWikiChunks);
    WikificationEvaluator.evaluateBOTF1(jointTestDocs.map(doc => WikificationEvaluator.convertSeqChunksToBagOfTitles(doc.goldWikiChunks)), allPredWikiTitles);
    WikificationEvaluator.evaluateFahrniMetrics(jointTestDocs.flatMap(_.goldWikiChunks), allPredWikiChunks, wikiLabelsInTrain)
    if (predWriter.isDefined) predWriter.get.close
    if (goldWriter.isDefined) goldWriter.get.close
    if (predWikiWriter.isDefined) {
      predWikiWriter.get.close
      predWikiWriterAux.get.close
    }
  }
  
  def decode(jointTestDoc: JointDocACE, maybeWikipediaInterface: Option[WikipediaInterface]) = {
    val fgfOnto = new FactorGraphFactoryACE(jointFeaturizer, maybeWikipediaInterface);
    val computer = new JointComputerShared(fgfOnto);
    computer.viterbiDecodeProduceAnnotations(jointTestDoc, weights);
  }
  
  def pack: JointPredictorACE = {
    if (jointFeaturizer.canReplaceIndexer) {
      val (newIndexer, newWeights) = GUtil.packFeaturesAndWeights(jointFeaturizer.indexer, weights);
      new JointPredictorACE(jointFeaturizer.replaceIndexer(newIndexer), newWeights, corefPruner);
    } else {
      this;
    }
  }
}

object JointPredictorACE {
  
  // N.B. Doubles with EntitySystem.preprocessDocs
  // Differences from above: no NER pruning, allows for gold mentions
  def preprocessDocs(path: String, size: Int, suffix: String, mentionPropertyComputer: MentionPropertyComputer, corefPruner: CorefPruner) = {
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
  
}