package edu.berkeley.nlp.entity.xdistrib

import edu.berkeley.nlp.entity.coref.DocumentGraph

class DocumentGraphComponents(val docGraph: DocumentGraph,
                              val components: Array[Array[Int]]) {
  val cachedSummedVects: Array[Array[Float]] = Array.tabulate(docGraph.size)(i => null);
}

object DocumentGraphComponents {
  
  def cacheComponents(docGraph: DocumentGraph, componentFeaturizer: ComponentFeaturizer, addToFeaturizer: Boolean) = {
    new DocumentGraphComponents(docGraph, Array.tabulate(docGraph.size)(i => componentFeaturizer.featurizeComponents(docGraph, i, addToFeaturizer)));
  }
}
