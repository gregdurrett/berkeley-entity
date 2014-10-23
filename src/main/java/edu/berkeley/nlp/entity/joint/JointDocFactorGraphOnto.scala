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
import edu.berkeley.nlp.entity.bp.ConstantUnaryFactor
import edu.berkeley.nlp.entity.wiki._

class JointDocFactorGraphOnto(val doc: JointDoc,
                              val featurizer: JointFeaturizerShared[NerFeaturizer],
                              val wikiDB: Option[WikipediaInterface],
                              val gold: Boolean,
                              val addToIndexer: Boolean,
                              val corefLossFcn: (CorefDoc, Int, Int) => Float,
                              val nerLossFcn: (String, String) => Float,
                              val wikiLossFcn: (Seq[String], String) => Float) extends JointDocFactorGraph {
  val docGraph = doc.docGraph;
  val nerLabelIndexer = featurizer.nerFeaturizer.labelIndexer;
  Logger.logss("Instantiating factor graph for " + doc.rawDoc.printableDocName + " with " + doc.rawDoc.words.size + " sentences and " + docGraph.getMentions.size + " mentions");
  
  val corefNodes = new Array[Node[Int]](docGraph.size);
  val nerNodes = new Array[Array[Node[String]]](doc.rawDoc.numSents);
  val neLabelIdx = NerSystemLabeled.StdLabelIndexer;
  val wikiNodes = new Array[Node[String]](docGraph.size);
  val queryNodes = new Array[Node[Query]](docGraph.size);

  val allNodes = new ArrayBuffer[Node[_]]();
  val allNodesEveryIter = new ArrayBuffer[Node[_]]();
  
  val corefUnaryFactors = new Array[UnaryFactorGeneral](docGraph.size);
  val nerUnaryFactors = new Array[Array[UnaryFactorGeneral]](doc.rawDoc.numSents);
  val nerBinaryFactors = new Array[Array[BinaryFactorGeneral]](doc.rawDoc.numSents);
  val wikiUnaryFactors = new Array[ConstantUnaryFactor[String]](docGraph.size);
  
  val queryUnaryFactors = new Array[UnaryFactorGeneral](docGraph.size);
  val queryWikiBinaryFactors = new Array[BinaryFactorGeneral](docGraph.size);
  
  val agreementFactors = Array.tabulate(docGraph.size)(i => new Array[BetterPropertyFactor[String]](i));
  val wikiNerFactors = new Array[BinaryFactorGeneral](docGraph.size);
  
  val corefWikiFactors = Array.tabulate(docGraph.size)(i => new Array[BetterPropertyFactor[String]](i));
  
  val semNodes = new Array[Node[String]](docGraph.size);
  val semNerConversionFactors = new Array[ConstantBinaryFactor[String]](docGraph.size);
  
//  val antecedentSelfFactors = new Array[NodeOwnTypeFactor](docGraph.size);
//  val antecedentOtherFactors = Array.tabulate(docGraph.size)(i => new Array[NodeOtherTypeFactor](i));
  
  val allFactors = new ArrayBuffer[Factor]();
  val allFactorsEveryIter = new ArrayBuffer[Factor]();
  
  private def addAndReturnNode[T](node: Node[T], isEveryItr: Boolean): Node[T] = {
    allNodes += node;
    if (isEveryItr) allNodesEveryIter += node;
    node;
  }
  
  private def addAndReturnFactor[T <: Factor](factor: T, isEveryItr: Boolean): T = {
    allFactors += factor;
    if (isEveryItr) allFactorsEveryIter += factor; 
    factor;
  }
  
  ///////////////////////////////
  // BUILDING THE FACTOR GRAPH //
  ///////////////////////////////
  val featsChart = docGraph.featurizeIndexNonPrunedUseCache(featurizer.corefFeaturizer);
  // COREF NODES AND UNARY FACTORS
  for (i <- 0 until docGraph.size()) {
    val domainArr = docGraph.getPrunedDomain(i, gold);
    corefNodes(i) = addAndReturnNode(new Node[Int](new Domain(domainArr)), true);
    val featsEachDecision = domainArr.map(antIdx => featsChart(i)(antIdx));
//    Logger.logss(i + ": "+  featsEachDecision.map(_.size).toSeq);
    corefUnaryFactors(i) = addAndReturnFactor(new UnaryFactorGeneral(corefNodes(i), featsEachDecision), false);
  }
  // NER NODES AND UNARY FACTORS
  // Build the transition feature matrix and share it across objects
  val allTransitionFeats = Array.tabulate(nerLabelIndexer.size, nerLabelIndexer.size)((prevValIdx, currValIdx) => {
    featurizer.nerFeaturizer.featurizeTransition(nerLabelIndexer.getObject(prevValIdx), nerLabelIndexer.getObject(currValIdx), addToIndexer);
  });
  for (sentIdx <- 0 until doc.rawDoc.numSents) {
    val nerFeatsThisSent = featurizer.nerFeaturizer.featurize(new NerExample(doc.rawDoc.words(sentIdx), doc.rawDoc.pos(sentIdx), null), addToIndexer);
    val prunedDomains = if (gold && !Driver.leaveNERLatent) doc.getGoldNerDomains(sentIdx) else doc.getPredNerDomains(sentIdx);
    nerNodes(sentIdx) = prunedDomains.map(prunedDomain => addAndReturnNode(new Node[String](prunedDomain), true));
    nerUnaryFactors(sentIdx) = Array.tabulate(doc.rawDoc.words(sentIdx).size)(wordIdx => {
      val prunedDomain = nerNodes(sentIdx)(wordIdx).domain;
      val featsEachDecision = prunedDomain.entries.map(neLabel => nerFeatsThisSent(wordIdx)(neLabelIdx.indexOf(neLabel)));
//      for (i <- 0 until featsEachDecision.size) {
//        Logger.logss(i + " " + prunedDomain.entries(i) + ": " + featsEachDecision(i).map(featurizer.indexer.getObject(_)).toSeq);
//      }
      addAndReturnFactor(new UnaryFactorGeneral(nerNodes(sentIdx)(wordIdx), featsEachDecision), false);
    });
    nerBinaryFactors(sentIdx) = Array.tabulate(doc.rawDoc.words(sentIdx).size - 1)(wordIdx => {
      val prevDomain = nerNodes(sentIdx)(wordIdx).domain;
      val currDomain = nerNodes(sentIdx)(wordIdx+1).domain;
      if (prevDomain.size == nerLabelIndexer.size && currDomain.size == nerLabelIndexer.size) {
        addAndReturnFactor(new BinaryFactorGeneral(nerNodes(sentIdx)(wordIdx), nerNodes(sentIdx)(wordIdx+1), allTransitionFeats), true);
      } else {
        val featsEachDecision = Array.tabulate(prevDomain.size, currDomain.size)((prevDomainIdx, currDomainIdx) => {
          allTransitionFeats(nerLabelIndexer.getIndex(prevDomain.entries(prevDomainIdx)))(nerLabelIndexer.getIndex(currDomain.entries(currDomainIdx)));
        });
//        Logger.logss(prevDomain.entries.toSeq + " " + currDomain.entries.toSeq);
//        for (featArrArr <- featsEachDecision; featArr <- featArrArr) {
//          if (featArr != null) Logger.logss(featArr.map(featurizer.indexer.getObject(_)).toSeq);
//        }
        addAndReturnFactor(new BinaryFactorGeneral(nerNodes(sentIdx)(wordIdx), nerNodes(sentIdx)(wordIdx+1), featsEachDecision), true);
      }
    });
  }
  // WIKI FACTORS
  if (wikiDB.isDefined) {
    if (false) {
      throw new RuntimeException("No longer implemented!");
      // QueryChooser
//      for (i <- 0 until docGraph.size) {
//        val wikOptions = featurizer.wikiFeaturizer.wikifier.wikifyGetPriorForJointModel(docGraph.corefDoc.rawDoc.docID, docGraph.getMention(i));
//        val wikDomain = new Domain[String](wikOptions.keySet.asScala.toArray.sorted);
//        wikOptions.keepTopNKeys(Driver.maxNumWikificationOptions);
//        if (Driver.injectNilOption && !wikOptions.containsKey(ExcludeToken) && !wikOptions.containsKey(NilToken)) {
//          wikOptions.incrementCount(NilToken, wikOptions.max()/Math.E);
//        }
//        wikiNodes(i) = addAndReturnNode(new Node[String](wikDomain), true);
//        wikiUnaryFactors(i) = addAndReturnFactor(new ConstantUnaryFactor[String](wikiNodes(i), wikDomain.entries.map(op => wikOptions.getCount(op)/wikOptions.totalCount)), false);
//      }
    } else { // LATENT QUERY WIKIFICATION
      for (i <- 0 until docGraph.size) {
        if (docGraph.getMention(i).mentionType.isClosedClass) {
          val nilQueryDomainArr = Array(Query.makeNilQuery(docGraph.getMention(i)));
          queryNodes(i) = addAndReturnNode(new Node[Query](new Domain(nilQueryDomainArr)), false);
          queryUnaryFactors(i) = addAndReturnFactor(new UnaryFactorGeneral(queryNodes(i), Array(Array())), false);
          wikiNodes(i) = addAndReturnNode(new Node[String](new Domain(Array(ExcludeToken))), true);
          queryWikiBinaryFactors(i) = addAndReturnFactor(new BinaryFactorGeneral(queryNodes(i), wikiNodes(i), Array(Array(Array()))), false);
        } else {
          val queries = Query.extractQueriesBest(docGraph.getMention(i), true);
          val qcComputer = new QueryChoiceComputer(wikiDB.get, featurizer.indexer);
          val queryDisambigs = queries.map(wikiDB.get.disambiguateBestGetAllOptions(_));
          // Build unary factors for queries
          val queryDomain = new Domain(queries.toArray)
          queryNodes(i) = addAndReturnNode(new Node[Query](queryDomain), false);
          val queryFeatures = qcComputer.featurizeQueries(queries, addToIndexer);
          queryUnaryFactors(i) = addAndReturnFactor(new UnaryFactorGeneral(queryNodes(i), queryFeatures), false);
          // Always latent
          val denotations = qcComputer.extractDenotationSetWithNil(queries, queryDisambigs, Driver.maxNumWikificationOptions);
          wikiNodes(i) = addAndReturnNode(new Node[String](new Domain(denotations.toArray)), true);
          queryWikiBinaryFactors(i) = addAndReturnFactor(new BinaryFactorGeneral(queryNodes(i), wikiNodes(i), qcComputer.featurizeQueriesAndDenotations(queries, denotations, addToIndexer)), false);
        }
      }
    }
  }
  // NER+COREF FACTORS
  // Two versions: one where there's an intermediate layer that converts
  if (featurizer.corefNerFeatures != "") {
    if (Driver.addIntermediateTypeLayer) {
      // Extract possible actual labels
      for (i <- 0 until docGraph.size) {
        // Build the intermediate layer
        val alignedNerNode = nerNodes(docGraph.getMention(i).sentIdx)(docGraph.getMention(i).headIdx)
        val semDomainValues = alignedNerNode.domain.entries.map(entry => NerSystemLabeled.getSemanticType(entry)).distinct.sorted;
        val semDomain = new Domain(semDomainValues);
        semNodes(i) = addAndReturnNode(new Node[String](semDomain), true);
        // From the aligned NER node, map B-X or I-X to X deterministically, O to uniform
        val factorValues = Array.tabulate(alignedNerNode.domain.size, semNodes(i).domain.size)((nerValIdx, semValIdx) => {
          val nerValue = alignedNerNode.domain.entries(nerValIdx);
          val semValue = semNodes(i).domain.entries(semValIdx);
          if (NerSystemLabeled.getSemanticType(nerValue) == semValue) 1.0 else Math.exp(-10.0)
        });
//        Logger.logss(alignedNerNode.domain.entries.toSeq + "    " + semNodes(i).domain.entries.toSeq)
//        factorValues.foreach(arr => Logger.logss(arr.toSeq));
        semNerConversionFactors(i) = addAndReturnFactor(new ConstantBinaryFactor[String](alignedNerNode, semNodes(i), factorValues), true);
      }
      for (i <- 0 until docGraph.size) {
        val currSemNode = semNodes(i);
        for (j <- corefNodes(i).domain.entries) {
          // Ensure that two mentions with the same head don't get a factor
          if (j != i && (docGraph.getMention(i).sentIdx != docGraph.getMention(j).sentIdx || docGraph.getMention(i).headIdx != docGraph.getMention(j).headIdx)) {
            val antSemNode = semNodes(j);
            val featsIndexed: Array[Array[Array[Int]]] = Array.tabulate(currSemNode.domain.size, antSemNode.domain.size)((currSemValIdx, antSemValIdx) => {
              featurizer.getCorefNerFeatures(docGraph, i, j, currSemNode.domain.entries(currSemValIdx), antSemNode.domain.entries(antSemValIdx), addToIndexer);
            });
            agreementFactors(i)(j) = addAndReturnFactor(new BetterPropertyFactor[String](j, currSemNode, corefNodes(i), antSemNode, featsIndexed), true);
          }
        }
      }
    } else {
      for (i <- 0 until docGraph.size) {
        val domain = corefNodes(i).domain;
        val currNerNode = nerNodes(docGraph.getMention(i).sentIdx)(docGraph.getMention(i).headIdx);
        for (j <- domain.entries) {
          // Ensure that two mentions with the same head don't get a factor
          if (j != i && (docGraph.getMention(i).sentIdx != docGraph.getMention(j).sentIdx || docGraph.getMention(i).headIdx != docGraph.getMention(j).headIdx)) {
            val antNerNode = nerNodes(docGraph.getMention(j).sentIdx)(docGraph.getMention(j).headIdx);
            val featsIndexed: Array[Array[Array[Int]]] = Array.tabulate(currNerNode.domain.size, antNerNode.domain.size)((currNerValIdx, antNerValIdx) => {
              featurizer.getCorefNerFeatures(docGraph, i, j, currNerNode.domain.entries(currNerValIdx), antNerNode.domain.entries(antNerValIdx), addToIndexer);
            });
            agreementFactors(i)(j) = addAndReturnFactor(new BetterPropertyFactor[String](j, currNerNode, corefNodes(i), antNerNode, featsIndexed), true);
          }
        }
      }
    }
  }
  
  // COREF+WIKIFICATION FACTORS
  if (featurizer.corefWikiFeatures != "") {
    for (i <- 0 until docGraph.size) {
      val domain = corefNodes(i).domain;
      val currWikiNode = wikiNodes(i);
      for (j <- domain.entries) {
        if (j != i) {
          val antWikiNode = wikiNodes(j);
          val featsIndexed: Array[Array[Array[Int]]] = Array.tabulate(currWikiNode.domain.size, antWikiNode.domain.size)((currWikiValIdx, antWikiValIdx) => {
            featurizer.getCorefWikiFeatures(docGraph, i, j, currWikiNode.domain.entries(currWikiValIdx), antWikiNode.domain.entries(antWikiValIdx), wikiDB, addToIndexer);
          });
          corefWikiFactors(i)(j) = addAndReturnFactor(new BetterPropertyFactor[String](j, currWikiNode, corefNodes(i), antWikiNode, featsIndexed), true);
        }
      }
    }
  }
  // WIKI+NER FACTORS
  if (featurizer.wikiNerFeatures != "") {
    require(!Driver.addIntermediateTypeLayer, "Intermediate type layer not supported for Wikification + NER")
    for (i <- 0 until docGraph.size) {
      val wikiNode = wikiNodes(i);
      val nerNode = nerNodes(docGraph.getMention(i).sentIdx)(docGraph.getMention(i).headIdx);
      val featsIndexed: Array[Array[Array[Int]]] = Array.tabulate(wikiNode.domain.size, nerNode.domain.size)((wikiValIdx, nerValIdx) => {
        featurizer.getWikiNerFeatures(docGraph, i, wikiNode.domain.entries(wikiValIdx), nerNode.domain.entries(nerValIdx), wikiDB, addToIndexer);
      });
      wikiNerFactors(i) = addAndReturnFactor(new BinaryFactorGeneral(wikiNode, nerNode, featsIndexed), true);
    }
  }
  
  // Update loss function offsets
  for (i <- 0 until corefNodes.size) {
    val domain = corefNodes(i).domain;
    corefUnaryFactors(i).setConstantOffset(Array.tabulate(domain.size)(entryIdx => corefLossFcn(docGraph.corefDoc, i, domain.entries(entryIdx))));
  }
  for (sentIdx <- 0 until nerUnaryFactors.size) {
    for (wordIdx <- 0 until nerUnaryFactors(sentIdx).size) {
      val domain = nerNodes(sentIdx)(wordIdx).domain;
      nerUnaryFactors(sentIdx)(wordIdx).setConstantOffset(Array.tabulate(domain.size)(entryIdx => nerLossFcn(NerSystemLabeled.getGoldNETag(doc.goldNERChunks(sentIdx), wordIdx), domain.entries(entryIdx))))
    }
  }

  // Initialize received messages at nodes
  allNodes.foreach(_.initializeReceivedMessagesUniform());

  var nerNanos = 0L;
  var agreeNanos = 0L;
  
  Logger.logss("Document factor graph instantiated: " + docGraph.size + " mentions, " + allNodes.size + " nodes (" + allNodesEveryIter.size + " every iter), " +
               allFactors.size + " factors (" + allFactorsEveryIter.size + " every iter): " + corefUnaryFactors.size + " coref unary factors, " +
               nerUnaryFactors.map(_.size).reduce(_ + _) + " NER unary factors, " + nerBinaryFactors.map(_.size).reduce(_ + _) + " NER binary factors");
  
  
  
  def setWeights(weights: Array[Float]) {
    // Update weights of the factors
    for (factor <- allFactors) {
      factor.setWeights(weights);
    }
    // Scrub values of potentials. Can't just reset all to zero because they're
    // still linked to the received messages from the previous iteration, so the
    // arrays themselves need to be reinitialized.
    allNodes.foreach(_.initializeReceivedMessagesUniform());
    for (node <- allNodes) {
      node.sendMessages;
    }
  }
  
  /////////////////////
  // MESSAGE PASSING //
  /////////////////////
  
  def computeAndStoreMarginals(weights: Array[Float],
                               exponentiateMessages: Boolean,
                               numBpIters: Int) {
    setWeights(weights);
    if (Driver.useFancyMessagePassing) {
      passMessagesFancy(numBpIters, exponentiateMessages);
    } else {
      for (i <- 0 until numBpIters) {
        passMessagesOneRound(i == 0 || i == numBpIters - 1, if (exponentiateMessages) 1.0 + 2 * i else 1.0);
      }
    }
//    passMessagesFancy(numBpIters, exponentiateMessages);
  }
  
  def computeLogNormalizerApprox: Double = {
    SimpleFactorGraph.computeLogNormalizerApprox(allNodes, allFactors)
  }
  
  def scrubMessages() {
    allNodes.foreach(_.initializeReceivedMessagesUniform);
    allNodes.foreach(_.clearSentMessages);
    allFactors.foreach(_.clearAllMessages);
  }
  
  def passMessagesFancy(numItrs: Int, exponentiateMessages: Boolean) {
    if (exponentiateMessages) {
      throw new RuntimeException("Exponentiation of messages not implemented");
    }
    // Send messages from unary factors first; these only need to be sent once
    corefUnaryFactors.foreach(_.sendMessages());
    nerUnaryFactors.foreach(_.foreach(_.sendMessages()));
    // Only do the NER backbone at the beginning and end; the assumption is that the
    // interactions are pretty weak so we don't need to forward-backward all the time
    passMessagesFancyNERBackbone();
    if (Driver.addIntermediateTypeLayer) {
      passFactorMessagesNonnull(semNerConversionFactors);
      passNodeMessagesNonnull(semNodes, 1.0);
    }
    // The main propagation has to happen in the middle
    for (i <- 0 until numItrs) {
      if (!Driver.includeExtraNodePasses) {
        passNodeMessagesNonnull(corefNodes, 1.0);
        if (Driver.addIntermediateTypeLayer) {
          passNodeMessagesNonnull(semNodes, 1.0);
        } else {
          passMessagesRelevantNERNodes(1.0);
        }
      }
      val time = System.nanoTime();
      for (i <- 0 until agreementFactors.size) {
        for (agreementFactor <- agreementFactors(i)) {
          if (agreementFactor != null) {
            agreementFactor.sendMessages;
            if (Driver.includeExtraNodePasses) {
              corefNodes(i).sendMessages(1.0);
              if (Driver.addIntermediateTypeLayer) {
                semNodes(i).sendMessages(1.0);
                semNodes(agreementFactor.selectedAntecedentMentionIdx).sendMessages(1.0);
              }
            }
          }
        }
      }
      agreeNanos += System.nanoTime() - time;
    }
    // Send stuff back to unary factors
    passNodeMessagesNonnull(corefNodes, 1.0);
    // Now back out to the NER
    if (Driver.addIntermediateTypeLayer) {
      if (!Driver.includeExtraNodePasses) {
        passNodeMessagesNonnull(semNodes, 1.0);
      }
      passFactorMessagesNonnull(semNerConversionFactors);
    }
    passMessagesFancyNERBackbone();
//    Logger.logss(SimpleFactorGraph.computeLogNormalizerApprox(allNodes, allFactors))
//    Logger.logss("NER: " + nerNanos/1000000);
//    Logger.logss("AGREE: " + agreeNanos/1000000);
  }
  
  def passNodeMessagesNonnull(nodes: Array[_ <: Node[_]], messageMultiplier: Double) {
    for (node <- nodes) {
      if (node != null) node.sendMessages(messageMultiplier);
    }
  }
  
  def passFactorMessagesNonnull(factors: Array[_ <: Factor]) {
    for (factor <- factors) {
      if (factor != null) factor.sendMessages
    }
  }
  
  def passMessagesRelevantNERNodes(messageMultiplier: Double) {
    for (nodeArr <- nerNodes; i <- 0 until nodeArr.size) {
      if (((i == 0 || i == nodeArr.size - 1) && nodeArr(i).factors.size > 2) ||
          ((i > 0 && i < nodeArr.size - 1) && nodeArr(i).factors.size > 3)) {
        nodeArr(i).sendMessages(messageMultiplier);
      }
    }
  }
  
  def passMessagesFancyNERBackbone() {
    val time = System.nanoTime();
    for (sentIdx <- 0 until nerNodes.size) {
      for (i <- 0 until nerNodes(sentIdx).size) {
        nerNodes(sentIdx)(i).sendMessages;
        if (i < nerBinaryFactors(sentIdx).size) {
          nerBinaryFactors(sentIdx)(i).sendMessages;
        }
      }
      for (i <- nerNodes(sentIdx).size - 1 to 0 by -1) {
        nerNodes(sentIdx)(i).sendMessages;
        if (i - 1 >= 0) {
          nerBinaryFactors(sentIdx)(i-1).sendMessages;
        }
      }
    }
    nerNanos += System.nanoTime() - time;
  }

  def passMessagesOneRound(firstOrLastIter: Boolean, messageMultiplier: Double) {
    // Send messages from unary factors first
    corefUnaryFactors.foreach(_.sendMessages());
    nerUnaryFactors.foreach(_.foreach(_.sendMessages()));
    // TODO: Reorder things so that the order is a bit more semantically sensible: outside-in,
    // forwards and backwards
    // Nodes and factors are ordered by position in the graph so later guys get better information from earlier ones
    for (node <- if (firstOrLastIter) allNodes else allNodesEveryIter) {
//    for (node <- allNodes) {
      node.sendMessages(messageMultiplier);
    }
    for (factor <- if (firstOrLastIter) allFactors else allFactorsEveryIter) {
//    for (factor <- allFactors) {
      factor.sendMessages();
    }
    // Fanciness: Do a forward and backward pass through the NER layer
    for (sentIdx <- 0 until nerNodes.size) {
      for (i <- 0 until nerNodes(sentIdx).size) {
        nerNodes(sentIdx)(i).sendMessages;
        if (i < nerBinaryFactors(sentIdx).size) {
          nerBinaryFactors(sentIdx)(i).sendMessages;
        }
      }
      for (i <- nerNodes(sentIdx).size - 1 to 0 by -1) {
        nerNodes(sentIdx)(i).sendMessages;
        if (i - 1 >= 0) {
          nerBinaryFactors(sentIdx)(i-1).sendMessages;
        }
      }
    }
  }

  def addExpectedFeatureCountsToGradient(scale: Float, gradient: Array[Float]) {
    allFactors.foreach(_.addExpectedFeatureCounts(scale, gradient))
  }
  
  def decodeCorefProduceBackpointers = {
    (0 until docGraph.size).map(i => corefNodes(i).domain.entries(GUtil.argMaxIdx(corefNodes(i).getMarginals))).toArray;
  }
  
  def decodeNERProduceChunksSimple = {
    (0 until doc.rawDoc.numSents).map(sentIdx => {
      val bioTags = nerNodes(sentIdx).map(node => node.domain.entries(GUtil.argMaxIdx(node.getMarginals)));
      val labeledChunks = NerSystemLabeled.convertToLabeledChunks(bioTags);
//      Logger.logss("================\n" + bioTags.toSeq + "\n" + labeledChunks + "\n" + doc.goldNERChunks(sentIdx).toSeq + "\n" + doc.rawDoc.words(sentIdx));
      labeledChunks;
    });
  }
  
  def decodeNERProduceChunks = {
    // TODO: Replace this with a call to the method in the object
    (0 until doc.rawDoc.numSents).map(sentIdx => {
      val sentLen = nerNodes(sentIdx).size
      JointDocFactorGraphOnto.decodeNERProduceChunks(Array.tabulate(sentLen)(i => nerNodes(sentIdx)(i).domain), Array.tabulate(sentLen)(i => nerNodes(sentIdx)(i).getMarginals));
    });
//    (0 until doc.rawDoc.numSents).map(sentIdx => {
//      val sentLen = nerNodes(sentIdx).size
//      var forwardScores = Array.tabulate(sentLen)(i => Array.tabulate(nerNodes(sentIdx)(i).domain.size)(j => Double.NegativeInfinity));
//      var backptrs = Array.tabulate(sentLen)(i => new Array[Int](forwardScores(i).size));
//      for (i <- 0 until sentLen) {
//        val marginals = nerNodes(sentIdx)(i).getMarginals;
//        if (i == 0) {
//          forwardScores(i) = marginals;
//        } else {
//          val prevDomain = nerNodes(sentIdx)(i-1).domain;
//          val domain = nerNodes(sentIdx)(i).domain;
//          for (labelIdx <- 0 until domain.size) {
//            for (prevLabelIdx <- 0 until prevDomain.size) {
//              if (NerFeaturizer.isLegalTransition(prevDomain.entries(prevLabelIdx), domain.entries(labelIdx))) {
//                // N.B. Instead of using the marginal we can use the actual emission score plus non-NER messages
//                // plus the transition score
//                var score = forwardScores(i-1)(prevLabelIdx) + marginals(labelIdx)
//                if (score > forwardScores(i)(labelIdx)) {
//                  forwardScores(i)(labelIdx) = score
//                  backptrs(i)(labelIdx) = prevLabelIdx;
//                }
//              }
//            }
//          }
//        }
//      }
//      var bestScore = Double.NegativeInfinity;
//      var bestIdx = -1;
//      for (labelIdx <- 0 until nerNodes(sentIdx).last.domain.size) {
//        if (forwardScores.last(labelIdx) > bestScore) {
//          bestIdx = labelIdx;
//          bestScore = forwardScores.last(labelIdx);
//        }
//      }
//      val tagsRev = new ArrayBuffer[String];
//      for (i <- sentLen - 1 to 0 by -1) {
//        tagsRev += nerNodes(sentIdx)(i).domain.entries(bestIdx);
//        if (i > 0) {
//          bestIdx = backptrs(i)(bestIdx);
//        }
//      }
//      NerSystemLabeled.convertToLabeledChunks(tagsRev.reverse)
//    });
  }
  
  def decodeWikificationProduceChunks = {
    if (wikiDB.isDefined) {
      chunkifyMentionAnnots(wikiNodes.map(node => node.domain.entries(GUtil.argMaxIdx(node.getMarginals))))
    } else {
      (0 until doc.rawDoc.numSents).map(i => Seq[Chunk[String]]());
    }
  }
  
  private def chunkifyMentionAnnots(mentAnnots: Seq[String]) = {
    val chunksPerSentence = (0 until docGraph.corefDoc.rawDoc.numSents).map(i => new ArrayBuffer[Chunk[String]]);
    for (i <- 0 until docGraph.getMentions.size) {
      val ment = docGraph.getMention(i);
      chunksPerSentence(ment.sentIdx) += new Chunk[String](ment.startIdx, ment.endIdx, mentAnnots(i));
    }
    chunksPerSentence;
  }
  
  def getRepresentativeFeatures = {
    val featsByTemplate = new HashMap[String,String];
    val allUnaryFactors: Array[UnaryFactorGeneral] = corefUnaryFactors ++ nerUnaryFactors.toSeq.flatten
    for (factor <- allUnaryFactors) {
      for (featArr <- factor.indexedFeatures; feat <- featArr) {
        val featStr = featurizer.indexer.getObject(feat);
        val template = PairwiseIndexingFeaturizer.getTemplate(featStr);
        if (!featsByTemplate.contains(template)) featsByTemplate.put(template, featStr);
      }
    }
    for (factor <- nerBinaryFactors.flatten) {
      for (featArrArr <- factor.indexedFeatureMatrix; featArr <- featArrArr) {
        if (featArr != null) {
          for (feat <- featArr) {
            val featStr = featurizer.indexer.getObject(feat);
            val template = PairwiseIndexingFeaturizer.getTemplate(featStr);
            featsByTemplate.put(template, featStr);
          }
        }
      }
    }
    for (agreementFactorArr <- agreementFactors; agreementFactor <- agreementFactorArr) {
      if (agreementFactor != null) {
        for (featSeqArr <- agreementFactor.indexedFeatureMatrix; featSeq <- featSeqArr; feat <- featSeq) {
          val featStr = featurizer.indexer.getObject(feat);
          val template = PairwiseIndexingFeaturizer.getTemplate(featStr);
          if (!featsByTemplate.contains(template)) featsByTemplate.put(template, featStr);
        }
      }
    }
    featsByTemplate
  }
}

object JointDocFactorGraphOnto {
  val tagIndexer = new Indexer[String];
  NerSystemLabeled.TagSet.foreach(tagIndexer.getIndex(_));
  SemClass.values.foreach(scType => tagIndexer.getIndex("NOM" + scType.toString));
  
  def decodeNERProduceChunks(domains: Seq[Domain[String]], allMarginals: Array[Array[Double]]) = {
    val sentLen = allMarginals.size
    var forwardScores = Array.tabulate(sentLen)(i => Array.tabulate(domains(i).size)(j => Double.NegativeInfinity));
    var backptrs = Array.tabulate(sentLen)(i => new Array[Int](forwardScores(i).size));
    for (i <- 0 until sentLen) {
      val marginals = allMarginals(i);
      if (i == 0) {
        forwardScores(i) = marginals;
      } else {
        val prevDomain = domains(i-1);
        val domain = domains(i);
        for (labelIdx <- 0 until domain.size) {
          for (prevLabelIdx <- 0 until prevDomain.size) {
            if (NerFeaturizer.isLegalTransition(prevDomain.entries(prevLabelIdx), domain.entries(labelIdx))) {
              // N.B. Instead of using the marginal we can use the actual emission score plus non-NER messages
              // plus the transition score
              var score = forwardScores(i-1)(prevLabelIdx) + marginals(labelIdx)
              if (score > forwardScores(i)(labelIdx)) {
                forwardScores(i)(labelIdx) = score
                backptrs(i)(labelIdx) = prevLabelIdx;
              }
            }
          }
        }
      }
    }
    var bestScore = Double.NegativeInfinity;
    var bestIdx = -1;
    for (labelIdx <- 0 until domains.last.size) {
      if (forwardScores.last(labelIdx) > bestScore) {
        bestIdx = labelIdx;
        bestScore = forwardScores.last(labelIdx);
      }
    }
    val tagsRev = new ArrayBuffer[String];
    for (i <- sentLen - 1 to 0 by -1) {
      tagsRev += domains(i).entries(bestIdx);
      if (i > 0) {
        bestIdx = backptrs(i)(bestIdx);
      }
    }
    NerSystemLabeled.convertToLabeledChunks(tagsRev.reverse)
  }
}
