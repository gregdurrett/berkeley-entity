package edu.berkeley.nlp.entity.coref
import scala.collection.mutable.HashMap
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

class OrderedClustering(val clusters: Seq[Seq[Int]]) {
  // Elements must be consecutive integers from 0 up to n
  private val allIndicesSorted = clusters.foldLeft(new ArrayBuffer[Int])(_ ++ _).sorted; 
  require(allIndicesSorted.sameElements((0 until allIndicesSorted.size).toSeq), allIndicesSorted);
  private val mentionToClusterIdMap = new HashMap[Int,Int];
  private val mentionToClusterMap = new HashMap[Int,Seq[Int]];
  for (clusterIdx <- 0 until clusters.size) {
    val cluster = clusters(clusterIdx)
    for (i <- cluster) {
      mentionToClusterIdMap.put(i, clusterIdx) 
      mentionToClusterMap.put(i, cluster);
    }
  }
  
  def getCluster(idx: Int) = mentionToClusterMap(idx);
  
  def isSingleton(idx: Int) = mentionToClusterMap(idx).size == 1;
  
  def startsCluster(idx: Int) = mentionToClusterMap(idx)(0) == idx;
  
  def areInSameCluster(idx1: Int, idx2: Int) = mentionToClusterMap(idx1).contains(idx2);
  
  def getImmediateAntecedent(idx: Int) = {
    val cluster = mentionToClusterMap(idx);
    val mentIdxInCluster = cluster.indexOf(idx);
    if (mentIdxInCluster == 0) {
      -1
    } else {
      cluster(mentIdxInCluster - 1);
    }
  }
  
  def getAllAntecedents(idx: Int) = {
    val cluster = mentionToClusterMap(idx);
    cluster.slice(0, cluster.indexOf(idx));
  }
  
  def getAllConsequents(idx: Int) = {
    val cluster = mentionToClusterMap(idx);
    cluster.slice(cluster.indexOf(idx) + 1, cluster.size);
  }
  
  def getClusterIdxMap = mentionToClusterIdMap
  
  def getClusterIdx(idx: Int) = mentionToClusterIdMap(idx)
  
  def getSubclustering(mentIdxsToKeep: Seq[Int]): OrderedClustering = {
    val oldIndicesToNewIndicesMap = new HashMap[Int,Int]();
    (0 until mentIdxsToKeep.size).map(i => oldIndicesToNewIndicesMap.put(mentIdxsToKeep(i), i));
    val filteredConvertedClusters = clusters.map(cluster => cluster.filter(mentIdxsToKeep.contains(_)).map(mentIdx => oldIndicesToNewIndicesMap(mentIdx)));
    val filteredConvertedClustersNoEmpties = filteredConvertedClusters.filter(cluster => !cluster.isEmpty); 
    new OrderedClustering(filteredConvertedClustersNoEmpties);
  }
  
  def bind(ments: Seq[Mention], doConllPostprocessing: Boolean): OrderedClusteringBound = {
    if (doConllPostprocessing) new OrderedClusteringBound(ments, this).postprocessForConll() else new OrderedClusteringBound(ments, this);
  }
}

object OrderedClustering {
  
  def createFromClusterIds(clusterIds: Seq[Int]) = {
    val mentIdAndClusterId = (0 until clusterIds.size).map(i => (i, clusterIds(i)));
    val clustersUnsorted = mentIdAndClusterId.groupBy(_._2).values;
    val finalClusters = clustersUnsorted.toSeq.sortBy(_.head).map(clusterWithClusterId => clusterWithClusterId.map(_._1));
    new OrderedClustering(finalClusters.toSeq);
  }
  
  def createFromBackpointers(backpointers: Seq[Int]) = {
    var nextClusterID = 0;
    val clusters = new ArrayBuffer[ArrayBuffer[Int]]();
    val mentionToCluster = new HashMap[Int,ArrayBuffer[Int]]();
    for (i <- 0 until backpointers.size) {
      if (backpointers(i) == i) {
        val cluster = ArrayBuffer(i);
        clusters += cluster;
        mentionToCluster.put(i, cluster); 
      } else {
        val cluster = mentionToCluster(backpointers(i));
        cluster += i;
        mentionToCluster.put(i, cluster);
      }
    }
    new OrderedClustering(clusters);
  }
}

class OrderedClusteringFromBackpointers(val backpointers: Seq[Int],
                                        val oc: OrderedClustering) {
  val adjacencyMap = new HashMap[Int,ArrayBuffer[Int]]
  for (i <- 0 until backpointers.size) {
    adjacencyMap(i) = new ArrayBuffer[Int]
  }
  for (i <- 0 until backpointers.size) {
    if (backpointers(i) != i) {
      adjacencyMap(i) += backpointers(i)
      adjacencyMap(backpointers(i)) += i
    }
  }
  
  def computeFromFrontier(seeds: Set[Int], blocked: Set[Int]) = {
    var frontier = new HashSet[Int]
    frontier ++= seeds
    var newFrontier = new HashSet[Int]
    val cluster = new HashSet[Int]
    while ((frontier -- cluster).size > 0) {
      for (node <- (frontier -- cluster)) {
        cluster += node
        newFrontier ++= (adjacencyMap(node) -- blocked)
      }
      frontier = newFrontier -- cluster
    }
    cluster.toSet
  }
  
  def changeBackpointerGetClusters(i: Int, newBackpointer: Int): (Seq[Int], Seq[Int]) = {
    // Split the cluster out
    val (oldAntCluster, partialCurrCluster) = if (backpointers(i) != i) {
      val oldAnt = backpointers(i)
      val oldAntAdjacent = adjacencyMap(oldAnt)
      computeFromFrontier(Set(backpointers(i)), Set(i)) -> computeFromFrontier(Set(i), Set(backpointers(i)))
    } else {
      (Seq[Int](), oc.getCluster(i))
    }
    
    val newCurrCluster = if (newBackpointer != i) partialCurrCluster ++ oc.getCluster(newBackpointer) else partialCurrCluster
    oldAntCluster.toSeq -> newCurrCluster.toSeq
  }
}
