package edu.berkeley.nlp.entity.wiki

import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.futile.LightRunner
import edu.berkeley.nlp.entity.coref.CorefDocAssembler
import edu.berkeley.nlp.entity._
import edu.berkeley.nlp.entity.coref.MentionPropertyComputer
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.entity.joint.LikelihoodAndGradientComputer
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.coref.CorefDoc
import edu.berkeley.nlp.futile.math.SloppyMath
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.joint.GeneralTrainer

/**
 * Keeps track of queries and an associated set of denotations, some of which are
 * correct for this particular example. Note that queries contain information about
 * the mentions they were derived from (but may need to be augmented to store even
 * more for more sophisticated feature computation).
 */
case class JointQueryDenotationExample(val queries: Seq[Query],
                                       val allDenotations: Seq[String],
                                       val correctDenotations: Seq[String],
                                       val rawCorrectDenotations: Seq[String]) {
  
  val correctDenotationIndices = allDenotations.zipWithIndex.filter(denotationAndIdx => correctDenotations.contains(denotationAndIdx._1)).map(_._2) 
  
  // Feature caches since feature computation is expensive if redone every time
  var cachedFeatsEachQuery: Array[Array[Int]] = null;
  var cachedFeatsEachQueryDenotation: Array[Array[Array[Int]]] = null;
}

/**
 * "Inferencer" that takes examples and weight vectors and does relevant operations: computes train
 * likelihoods, computes gradients, and computes the best denotation for an example given a set of
 * parameters.
 */
class JointQueryDenotationChoiceComputer(val wikiDB: WikipediaInterface,
                                         val featureIndexer: Indexer[String]) extends LikelihoodAndGradientComputer[JointQueryDenotationExample] {
  // Used for feature computation
  val queryChooser = new QueryChoiceComputer(wikiDB, featureIndexer)

  def featurizeUseCache(ex: JointQueryDenotationExample, addToIndexer: Boolean) {
    if (ex.cachedFeatsEachQuery == null) {
      ex.cachedFeatsEachQuery = queryChooser.featurizeQueries(ex.queries, addToIndexer)
      ex.cachedFeatsEachQueryDenotation = queryChooser.featurizeQueriesAndDenotations(ex.queries, ex.allDenotations, addToIndexer)
    }
  }
  
  /**
   * Computes a score matrix for (query, denotation) pairs based on the computed features
   */
  def getUnnormalizedJointScores(ex: JointQueryDenotationExample, weights: Array[Float]): Array[Array[Float]] = {
    featurizeUseCache(ex, false)
    // each example will have a number of features associated with each query
    // each feature is an indicator, so we use the cache of the features indexes
    // and sum the values of the features
    val rawQueryScores = ex.cachedFeatsEachQuery.map(feats => GUtil.scoreIndexedFeats(feats, weights));
    // these are the weights from each query wrt the various word choices
    val queryDenotationMatrix = ex.cachedFeatsEachQueryDenotation.map(_.map(feats => GUtil.scoreIndexedFeats(feats, weights)));
    val scores = Array.tabulate(ex.queries.size, ex.allDenotations.size)((i, j) => Float.NegativeInfinity)
    for (queryIdx <- 0 until ex.queries.size; denotationIdx <- 0 until ex.allDenotations.size) {
      // These are indicator weights, so by summing them we can compute the resulting value of choosing a given word
      // and a given query by combining the results of the dot product of the query and the denotation
      scores(queryIdx)(denotationIdx) = rawQueryScores(queryIdx) + queryDenotationMatrix(queryIdx)(denotationIdx)
    }
    scores
  }
  
  /**
   * Computes denotation marginals (normalized, but still in log space). Query choice
   * gets marginalized out here.
   */
  def getDenotationLogMarginals(ex: JointQueryDenotationExample, weights: Array[Float]): Array[Float] = {
    val scores = getUnnormalizedJointScores(ex, weights)
    // the scores matrix contains log(p_{i,j}), so we are using
    // logAdd to sum the probabilities
    // as p(q,d) \propto e^(w^T f(q,d))
    val rawDenotationMarginals = Array.tabulate(ex.allDenotations.size)(i => SloppyMath.logAdd(scores.map(_(i))).toFloat)
    val normalizer = SloppyMath.logAdd(rawDenotationMarginals).toFloat
    (0 until rawDenotationMarginals.size).foreach(i => rawDenotationMarginals(i) -= normalizer)
    rawDenotationMarginals
  }
  
  /**
   * Computes the gradient on the given example at the point specified by the given weight vector and adds it to the
   * gradient array. 
   */
  def addUnregularizedStochasticGradient(ex: JointQueryDenotationExample, weights: Array[Float], gradient: Array[Float]) {
    // False for adding features here, though it doesn't really matter; we'd better have cached all of them
    // in advance anyway to know how long the weight vector should be
    val allFeats = featurizeUseCache(ex, false); 
    val scores = getUnnormalizedJointScores(ex, weights)
    val logNormalizer = SloppyMath.logAdd(scores.map(SloppyMath.logAdd(_)))
    var goldLogNormalizer = Float.NegativeInfinity
    for (j <- ex.correctDenotationIndices) {
      for (i <- 0 until ex.queries.size) {
        goldLogNormalizer = SloppyMath.logAdd(goldLogNormalizer, scores(i)(j)).toFloat
      }
    }
    for (i <- 0 until ex.queries.size) {
      for (j <- 0 until ex.allDenotations.size) {
        val isCorrect = ex.correctDenotationIndices.contains(j)
        val goldCount = if (isCorrect) (Math.exp(scores(i)(j) - goldLogNormalizer)).toFloat else 0F
        val predCount = Math.exp(scores(i)(j) - logNormalizer).toFloat
        GUtil.addToGradient(ex.cachedFeatsEachQueryDenotation(i)(j), goldCount - predCount, gradient);
        GUtil.addToGradient(ex.cachedFeatsEachQuery(i), goldCount - predCount, gradient);
      }
    }
  }
  
  /**
   * Computes the log likelihood. Since there are multiple correct answers, we have to take a sum.
   */
  def computeLogLikelihood(ex: JointQueryDenotationExample, weights: Array[Float]): Float = {
    val denotationMarginals = getDenotationLogMarginals(ex, weights)
    val correctScores = ex.correctDenotationIndices.map(denotationMarginals(_)).toArray;
    SloppyMath.logAdd(correctScores).toFloat;
  }
  
  /**
   * Finds the highest-scoring denotation as the answer
   */
  def computeDenotation(ex: JointQueryDenotationExample, weights: Array[Float]) = {
    val denotationMarginals = getDenotationLogMarginals(ex, weights)
    ex.allDenotations(GUtil.argMaxIdxFloat(denotationMarginals));
  }
}

/**
 * Pairs a feature indexer with a set of weights; note that the WikipediaInterface is excluded
 * so that it doesn't get serialized in (because it's big) and also so this can be ported
 * to another dataset with a different set of pre-extracted sufficient statistics.
 */
class JointQueryDenotationChooser(val featureIndexer: Indexer[String],
                                  val weights: Array[Float]) extends Serializable {
  
  /*def pickDenotation(queries: Seq[Query], wikiDB: WikipediaInterface): String = {
    val computer = new JointQueryDenotationChoiceComputer(wikiDB, featureIndexer);
    val denotations = queries.map(query => wikiDB.disambiguateBestGetAllOptions(query));
    val ex = new JointQueryDenotationExample(queries, denotations, Array[String](), Array[String]());
    computer.computeDenotation(ex, weights)
  }*/

  def pickDenotations(queries: Seq[Query], wikiDB: WikipediaInterface) : Seq[String] = {
    val computer = new JointQueryDenotationChoiceComputer(wikiDB, featureIndexer);
    val denotations = queries.map(query => wikiDB.disambiguateBestGetAllOptions(query));
    val dden = Query.extractDenotationSetWithNil(queries, denotations, 10)
    val ex = new JointQueryDenotationExample(queries, dden, Array[String](), Array[String]());
    val denotationMarginals = computer.getDenotationLogMarginals(ex, weights)

    ex.allDenotations.zipWithIndex.sortBy(v => denotationMarginals(v._2)).reverse.map(_._1)
  }

  def diffFeatures(correct: Query, choosen: Query, wikiDB: WikipediaInterface) = {

  }
}

object JointQueryDenotationChooser {
  
  /**
   * Extracts an isolated set of entity linking examples from coreference documents
   * and standoff entity link annotations.
   */
  def extractExamples(corefDocs: Seq[CorefDoc], goldWikification: CorpusWikiAnnots, wikiDB: WikipediaInterface, filterImpossible: Boolean = false) = {
    val exs = new ArrayBuffer[JointQueryDenotationExample];
    var numImpossible = 0;
    // Go through all mentions in all documents
    for (corefDoc <- corefDocs) {
      val docName = corefDoc.rawDoc.docID
      for (i <- 0 until corefDoc.predMentions.size) {
        // Discard "closed class" mentions (pronouns) since these don't have interesting entity links
        if (!corefDoc.predMentions(i).mentionType.isClosedClass()) {
          val ment = corefDoc.predMentions(i);
          // There are multiple possible gold Wikipedia titles for some mentions. Note that
          // NIL (no entry in Wikipedia) is included as an explicit choice, so this includes NILs (as
          // it should according to how the task is defined)
          val goldLabel = getGoldWikification(goldWikification(docName), ment)
          if (goldLabel.size >= 1) {
            val queries = Query.extractQueriesBest(ment, true);
            val queryDisambigs = queries.map(wikiDB.disambiguateBestGetAllOptions(_));
//            val denotations = queries.map(wikiDB.disambiguateBestNoDisambig(_));
            val denotations = Query.extractDenotationSetWithNil(queries, queryDisambigs, maxNumWikificationOptions);
            val correctDenotations = denotations.filter(denotation => isCorrect(goldLabel, denotation))
            // N.B. The use of "isCorrect" here is needed to canonicalize 
            val correctIndices = denotations.zipWithIndex.filter(denotationIdx => isCorrect(goldLabel, denotationIdx._1)).map(_._2);
//            if (correctIndices.isEmpty && 
            if (filterImpossible && correctIndices.isEmpty) {
              numImpossible += 1;
            } else {
              exs += new JointQueryDenotationExample(queries, denotations, correctDenotations, goldLabel)
            }
          }
        }
      }
    }
    Logger.logss(exs.size + " possible, " + numImpossible + " impossible");
    exs;
  }


  def loadDocuments(path : String) = {
    val limit = numLoadedSamples//500
    if(path.startsWith("wikiser:")) {
      WikiDocReader.loadRawWikiDocs(path.split(":")(1), limit, "", Language.ENGLISH)
    } else {
      ConllDocReader.loadRawConllDocsWithSuffix(path, limit, "", Language.ENGLISH)
    }
  }

  
  val trainDataPath = "data/ace05/train";
  val testDataPath = "data/ace05/dev";
  val wikiPath = "data/ace05/ace05-all-conll-wiki" // contains the wiki links for both items
  val wikiDBPath = "models/wiki-db-ace.ser.gz"
  
  val lambda = 1e-8F
  val batchSize = 1
  val numItrs = 20
  
  val maxNumWikificationOptions = 7

  val numLoadedSamples = -1 // for debugging by loading less samples
  
  def main(args: Array[String]) {
    LightRunner.initializeOutput(JointQueryDenotationChooser.getClass());
    LightRunner.populateScala(JointQueryDenotationChooser.getClass(), args)
    // Read in CoNLL documents 
    val assembler = CorefDocAssembler(Language.ENGLISH, true);
    val trainDocs = loadDocuments(trainDataPath);
    val trainCorefDocs = trainDocs.map(doc => {
      try {
        assembler.createCorefDoc(doc, new MentionPropertyComputer(None))
      } catch {
        case e : Exception => {
          // TODO: fix the wikidocument parser
          println("failed document "+doc.docID)
          null
        }
      }
    }).filter(_!=null);

    //val testDocs = ConllDocReader.loadRawConllDocsWithSuffix(testDataPath, -1, "", Language.ENGLISH);
    val testDocs = loadDocuments(testDataPath)
    val testCorefDocs = testDocs.map(doc => assembler.createCorefDoc(doc, new MentionPropertyComputer(None)));

    // Read in gold Wikification labels
    val goldWikification = WikiAnnotReaderWriter.readStandoffAnnotsAsCorpusAnnots(wikiPath)
    // Read in the title given surface database
    val wikiDB = GUtil.load(wikiDBPath).asInstanceOf[WikipediaInterface];
    // Make training examples, filtering out those with solutions that are unreachable because
    // they're not good for training
    val trainExs = extractExamples(trainCorefDocs, goldWikification, wikiDB, filterImpossible = true)
    
    // Extract features
    val featIndexer = new Indexer[String]
    val computer = new JointQueryDenotationChoiceComputer(wikiDB, featIndexer);
    for (trainEx <- trainExs) {
      computer.featurizeUseCache(trainEx, true);
    }
    Logger.logss(featIndexer.size + " features");
    // Train
    val gt = new GeneralTrainer[JointQueryDenotationExample]();
    val weights = gt.trainAdagrad(trainExs, computer, featIndexer.size, 1.0F, lambda, batchSize, numItrs);
    
    val chooser = new JointQueryDenotationChooser(featIndexer, weights)
    
    // Build the test examples and decode the test set
    // No filtering now because we're doing test

    val testExs = extractExamples(testCorefDocs, goldWikification, wikiDB, filterImpossible = true)//false);

    var correctItemWasInSet = 0

    val results = testExs.map(t => {
      // TODO: need more then one perdicted title
      val picks = chooser.pickDenotations(t.queries, wikiDB)
      if(!isCorrect(t.rawCorrectDenotations, picks(0))) {
        // the pick is not correct, attempt to determine if there would have
        // been a better pick that is in the picks list (which basically means all of the
        /*if(picks.size > 1 && isCorrect(t.rawCorrectDenotations, picks(1))) {
          // the correct pick was the second answer instead of the first one
          // try and report the differences between the two items
          println("second pick was correct")

        }*/
        var qq = false
        for((p, i) <- picks.drop(1).zipWithIndex) {
          // try: t.correctDenotations here?
          if(isCorrect(t.correctDenotations, p) || isCorrect(t.rawCorrectDenotations, p)) {
            println("Found correct item with "+i)
            correctItemWasInSet += 1
            qq = true
            //println("found correct item")
          }
        }
        if(!qq) {
          println("???")
        }
      }
      (t.rawCorrectDenotations, picks, t.queries(0).originalMent.rawDoc)
    })

    val goldTestDenotationsAsTrivialChunks = (0 until results.size).map(i => new Chunk[Seq[String]](i, i+1, results(i)._1))
    val predTestDenotationsAsTrivialChunks = (0 until results.size).map(i => new Chunk[String](i, i+1, results(i)._2(0)))
    
    // Hacky but lets us reuse some code that normally evaluates things with variable endpoints
//    WikificationEvaluator.evaluateWikiChunksBySent(Seq(goldTestDenotationsAsTrivialChunks), Seq(predTestDenotationsAsTrivialChunks))
    WikificationEvaluator.evaluateFahrniMetrics(Seq(goldTestDenotationsAsTrivialChunks), Seq(predTestDenotationsAsTrivialChunks), Set())

    val mentionsByDoc = results.groupBy(_._3)

    WikificationEvaluator.evaluateBOTF1_mfl(mentionsByDoc)
    println("Number of correct items that were in the set: "+correctItemWasInSet)


    LightRunner.finalizeOutput();
  }
  
}