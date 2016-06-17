package edu.berkeley.nlp.entity.coref
import scala.collection.JavaConverters._

@SerialVersionUID(1L)
class OrderedClusteringBound(val ments: Seq[Mention],
                             val clustering: OrderedClustering) extends Serializable {
  
  def postprocessForConll(): OrderedClusteringBound = {
    val mentIdxsToKeep = (0 until ments.size).filter(i => !clustering.isSingleton(i));
    new OrderedClusteringBound(mentIdxsToKeep.map(i => ments(i)), clustering.getSubclustering(mentIdxsToKeep));
  }
  
  def getClusterIdx(ment: Mention) = {
    clustering.getClusterIdx(ments.indexOf(ment));
  }
  
  def toSimple = new OrderedClusteringBoundSimple(ments.map(ment => (ment.sentIdx, ment.startIdx, ment.endIdx)), clustering)
}

class OrderedClusteringBoundSimple(val ments: Seq[(Int,Int,Int)],
                                   val clustering: OrderedClustering) {
  
}