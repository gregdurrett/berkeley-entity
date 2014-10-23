package edu.berkeley.nlp.entity.coref
import edu.berkeley.nlp.futile.fig.basic.Indexer

class DocumentInferencerOracle extends DocumentInferencer {
  
  def getInitialWeightVector(featureIndexer: Indexer[String]): Array[Float] = Array.fill(featureIndexer.size())(0.0F);
  
  def computeLikelihood(docGraph: DocumentGraph,
                        pairwiseScorer: PairwiseScorer,
                        lossFcn: (CorefDoc, Int, Int) => Float) = {
    0.0F;
  }
  
  def addUnregularizedStochasticGradient(docGraph: DocumentGraph,
                                         pairwiseScorer: PairwiseScorer,
                                         lossFcn: (CorefDoc, Int, Int) => Float,
                                         gradient: Array[Float]) = {
  }
  
  def viterbiDecode(docGraph: DocumentGraph,
                    pairwiseScorer: PairwiseScorer): Array[Int] = {
    val clustering = docGraph.getOraclePredClustering();
    val resultSeq = for (i <- 0 until docGraph.size) yield {
      val immediateAntecedentOrMinus1 = clustering.getImmediateAntecedent(i);
      if (immediateAntecedentOrMinus1 == -1) {
        i;
      } else {
        docGraph.getMentions.indexOf(immediateAntecedentOrMinus1);
      }
    }
    resultSeq.toArray;
  }
  
  def finishPrintStats() = {}
}
