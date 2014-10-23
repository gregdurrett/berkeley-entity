package edu.berkeley.nlp.entity.coref
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.fig.basic.Indexer

trait DocumentInferencer {
  
  def getInitialWeightVector(featureIndexer: Indexer[String]): Array[Float];
  
  def computeLikelihood(docGraph: DocumentGraph,
                        pairwiseScorer: PairwiseScorer,
                        lossFcn: (CorefDoc, Int, Int) => Float): Float;
  
  def addUnregularizedStochasticGradient(docGraph: DocumentGraph,
                                         pairwiseScorer: PairwiseScorer,
                                         lossFcn: (CorefDoc, Int, Int) => Float,
                                         gradient: Array[Float]);
  
  def viterbiDecode(docGraph: DocumentGraph, pairwiseScorer: PairwiseScorer): Array[Int];
  
  def finishPrintStats();
  
  def viterbiDecodeFormClustering(docGraph: DocumentGraph, pairwiseScorer: PairwiseScorer): (Array[Int], OrderedClustering) = {
    val predBackptrs = viterbiDecode(docGraph, pairwiseScorer);
    (predBackptrs, OrderedClustering.createFromBackpointers(predBackptrs));
  }
  
  def viterbiDecodeAll(docGraphs: Seq[DocumentGraph], pairwiseScorer: PairwiseScorer): Array[Array[Int]] = {
    val allPredBackptrs = new Array[Array[Int]](docGraphs.size);
    for (i <- 0 until docGraphs.size) {
      val docGraph = docGraphs(i);
      Logger.logs("Decoding " + i);
      val predBackptrs = viterbiDecode(docGraph, pairwiseScorer);
      allPredBackptrs(i) = predBackptrs;
    }
    allPredBackptrs;
  }
  
  def viterbiDecodeAllFormClusterings(docGraphs: Seq[DocumentGraph], pairwiseScorer: PairwiseScorer): (Array[Array[Int]], Array[OrderedClustering]) = {
    val allPredBackptrs = viterbiDecodeAll(docGraphs, pairwiseScorer);
    val allPredClusteringsSeq = (0 until docGraphs.size).map(i => OrderedClustering.createFromBackpointers(allPredBackptrs(i)));
    (allPredBackptrs, allPredClusteringsSeq.toArray)
  }
}
