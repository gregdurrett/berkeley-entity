package edu.berkeley.nlp.entity.joint

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.entity.Chunk
import edu.berkeley.nlp.entity.ConllDoc
import edu.berkeley.nlp.entity.ConllDocReader
import edu.berkeley.nlp.entity.ConllDocWriter
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.entity.coref.CorefDocAssembler
import edu.berkeley.nlp.entity.coref.CorefDocAssemblerACE
import edu.berkeley.nlp.entity.coref.CorefPruner
import edu.berkeley.nlp.entity.coref.CorefSystem
import edu.berkeley.nlp.entity.coref.DocumentGraph
import edu.berkeley.nlp.entity.coref.MentionPropertyComputer
import edu.berkeley.nlp.entity.coref.OrderedClustering
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.entity.ner.NerFeaturizer
import edu.berkeley.nlp.entity.ner.NerPruner
import edu.berkeley.nlp.entity.wiki.WikipediaInterface
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.futile.fig.exec.Execution
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.ner.NEEvaluator
import edu.berkeley.nlp.entity.coref.CorefEvaluator
import edu.berkeley.nlp.entity.Driver
import scala.collection.GenTraversableOnce
import java.io.PrintWriter

@SerialVersionUID(1L)
class JointPredictor(val jointFeaturizer: JointFeaturizerShared[NerFeaturizer],
                     val weights: Array[Float],
                     val corefPruner: CorefPruner,
                     val nerPruner: NerPruner) extends Serializable {
  
  def makeIndividualDocPredictionWriter(maybeWikipediaInterface: Option[WikipediaInterface], outWriter: PrintWriter, outWikiWriter: PrintWriter): (JointDoc => Unit) = {
    val fgfOnto = new FactorGraphFactoryOnto(jointFeaturizer, maybeWikipediaInterface);
    val computer = new JointComputerShared(fgfOnto);
    (jointDoc: JointDoc) => {
      Logger.logss("Decoding " + jointDoc.rawDoc.printableDocName);
      // Don't decode if there are no mentions because things will break
      if (jointDoc.docGraph.getMentions.size == 0) {
        if (jointDoc.rawDoc.numSents > 0) {
          Logger.logss("WARNING: Document with zero mentions but nonzero number of sentences, not running NER but there could be NE mentions")
        }
        ConllDocWriter.writeDoc(outWriter, jointDoc.rawDoc)
      } else {
        val (backptrs, clustering, nerChunks, wikiChunks) = computer.viterbiDecodeProduceAnnotations(jointDoc, weights);
        ConllDocWriter.writeDocWithPredAnnotationsWikiStandoff(outWriter, outWikiWriter, jointDoc.rawDoc, nerChunks, clustering.bind(jointDoc.docGraph.getMentions, Driver.doConllPostprocessing), wikiChunks);
      }
    }
  }
  
  def decodeWriteOutput(jointTestDocs: Seq[JointDoc], maybeWikipediaInterface: Option[WikipediaInterface], doConllPostprocessing: Boolean) {
    decodeWriteOutputMaybeEvaluate(jointTestDocs, maybeWikipediaInterface, doConllPostprocessing, false);
  }
  
  def decodeWriteOutputEvaluate(jointTestDocs: Seq[JointDoc], maybeWikipediaInterface: Option[WikipediaInterface], doConllPostprocessing: Boolean) {
    decodeWriteOutputMaybeEvaluate(jointTestDocs, maybeWikipediaInterface, doConllPostprocessing, true);
  }
  
  private def decodeWriteOutputMaybeEvaluate(jointTestDocs: Seq[JointDoc], maybeWikipediaInterface: Option[WikipediaInterface], doConllPostprocessing: Boolean, evaluate: Boolean) {
    val fgfOnto = new FactorGraphFactoryOnto(jointFeaturizer, maybeWikipediaInterface);
    val computer = new JointComputerShared(fgfOnto);
    val outWriter = IOUtils.openOutHard(Execution.getFile("output.conll"))
    val outWikiWriter = IOUtils.openOutHard(Execution.getFile("output-wiki.conll"))
    val allPredBackptrsAndClusterings = new ArrayBuffer[(Array[Int],OrderedClustering)];
    val predNEChunks = new ArrayBuffer[Seq[Seq[Chunk[String]]]];
    Logger.startTrack("Decoding");
    for (i <- (0 until jointTestDocs.size)) {
      Logger.logss("Decoding " + i);
      val jointDevDoc = jointTestDocs(i);
      val (backptrs, clustering, nerChunks, wikiChunks) = computer.viterbiDecodeProduceAnnotations(jointDevDoc, weights);
      ConllDocWriter.writeDocWithPredAnnotationsWikiStandoff(outWriter, outWikiWriter, jointDevDoc.rawDoc, nerChunks, clustering.bind(jointDevDoc.docGraph.getMentions, Driver.doConllPostprocessing), wikiChunks);
      if (evaluate) {
        allPredBackptrsAndClusterings += (backptrs -> clustering);
        predNEChunks += nerChunks;
      }
    }
    outWriter.close();
    outWikiWriter.close();
    Logger.endTrack();
    if (evaluate) {
      Logger.logss(CorefEvaluator.evaluateAndRender(jointTestDocs.map(_.docGraph), allPredBackptrsAndClusterings.map(_._1), allPredBackptrsAndClusterings.map(_._2),
                                                    Driver.conllEvalScriptPath, "DEV: ", Driver.analysesToPrint));
      NEEvaluator.evaluateChunksBySent(jointTestDocs.flatMap(_.goldNERChunks), predNEChunks.flatten);
      NEEvaluator.evaluateOnConll2011(jointTestDocs, predNEChunks, Driver.conll2011Path.split(",").flatMap(path => ConllDocReader.readDocNames(path)).toSet, if (Driver.writeNerOutput) Execution.getFile("ner.txt") else "");
    }
  }

  def pack: JointPredictor = {
    if (jointFeaturizer.canReplaceIndexer) {
      val (newIndexer, newWeights) = GUtil.packFeaturesAndWeights(jointFeaturizer.indexer, weights);
      new JointPredictor(jointFeaturizer.replaceIndexer(newIndexer), newWeights, corefPruner, nerPruner);
    } else {
      this;
    }
  }
}
