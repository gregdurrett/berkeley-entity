package edu.berkeley.nlp.entity.coref
import scala.collection.JavaConverters._

class OrderedClusteringBound(val ments: Seq[Mention],
                             val clustering: OrderedClustering) {
  
  def postprocessForConll(): OrderedClusteringBound = {
    val mentIdxsToKeep = (0 until ments.size).filter(i => !clustering.isSingleton(i));
    new OrderedClusteringBound(mentIdxsToKeep.map(i => ments(i)), clustering.getSubclustering(mentIdxsToKeep));
  }
  
  def getClusterIdx(ment: Mention) = {
    clustering.getClusterIdx(ments.indexOf(ment));
  }
}
