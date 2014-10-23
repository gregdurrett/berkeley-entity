package edu.berkeley.nlp.entity.joint

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.coref.CorefDoc
import edu.berkeley.nlp.entity.coref.DocumentGraph
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.entity.coref.PairwiseIndexingFeaturizer
import edu.berkeley.nlp.entity.coref.PairwiseIndexingFeaturizerJoint
import edu.berkeley.nlp.entity.coref.PairwiseScorer
import edu.berkeley.nlp.entity.ner.NerSystemLabeled
import edu.berkeley.nlp.entity.sem.SemClass
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.ner.NerFeaturizer
import edu.berkeley.nlp.entity.Chunk
import edu.berkeley.nlp.entity.bp.BetterPropertyFactor
import edu.berkeley.nlp.entity.bp.Factor
import edu.berkeley.nlp.entity.bp.Node
import edu.berkeley.nlp.entity.bp.UnaryFactorOld
import scala.Array.canBuildFrom
import edu.berkeley.nlp.entity.bp.Domain
import edu.berkeley.nlp.entity.bp.UnaryFactorGeneral
import edu.berkeley.nlp.entity.ner.NerExample
import edu.berkeley.nlp.entity.bp.BinaryFactorGeneral
import edu.berkeley.nlp.entity.Driver
import edu.berkeley.nlp.entity.bp.ConstantBinaryFactor
import edu.berkeley.nlp.entity.bp.SimpleFactorGraph
import scala.collection.mutable.HashMap

trait JointDocFactorGraph {
  
  def setWeights(weights: Array[Float]);
  
  def computeAndStoreMarginals(weights: Array[Float],
                               exponentiateMessages: Boolean,
                               numBpIters: Int);
  
  def computeLogNormalizerApprox: Double;
  
  def scrubMessages();
  
  def passMessagesFancy(numItrs: Int, exponentiateMessages: Boolean);
  
  def addExpectedFeatureCountsToGradient(scale: Float, gradient: Array[Float]);
  
  def decodeCorefProduceBackpointers: Array[Int];
  
  def decodeNERProduceChunks: Seq[Seq[Chunk[String]]];
  
  def decodeWikificationProduceChunks: Seq[Seq[Chunk[String]]];
  
  def getRepresentativeFeatures: HashMap[String,String];
}
