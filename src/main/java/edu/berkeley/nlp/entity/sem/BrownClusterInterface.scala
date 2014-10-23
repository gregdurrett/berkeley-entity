package edu.berkeley.nlp.entity.sem

import edu.berkeley.nlp.futile.fig.basic.IOUtils
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.futile.util.Logger

object BrownClusterInterface {
  
  def loadBrownClusters(path: String, cutoff: Int): Map[String,String] = {
    val wordsToClusters = new HashMap[String,String];
    val iterator = IOUtils.lineIterator(path);
    while (iterator.hasNext) {
      val nextLine = iterator.next;
      val fields = nextLine.split("\\s+");
      if (fields.size == 3 && fields(fields.size - 1).toInt >= cutoff) {
        wordsToClusters.put(fields(1), fields(0));
      }
    }
    Logger.logss(wordsToClusters.size + " Brown cluster definitions read in");
    wordsToClusters.toMap;
  }
}