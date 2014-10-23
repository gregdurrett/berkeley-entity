package edu.berkeley.nlp.entity.wiki

import scala.collection.mutable.HashSet
import scala.collection.JavaConverters._
import edu.berkeley.nlp.futile.util.CounterMap
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.coref.Mention
import edu.berkeley.nlp.futile.util.Counter
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

// Need to know all titles (including redirects)
@SerialVersionUID(1L)
class WikipediaTitleGivenSurfaceDB(val surfaceToTitle: CounterMap[String,String]) extends Serializable {
  val truecaseMap = new HashMap[String,ArrayBuffer[String]];
  val allPossibleTitles = new HashSet[String];
  val allPossibleTitlesWikicase = new HashSet[String];
  val allPossibleTitlesLowercase = new HashSet[String];
  for (surface <- surfaceToTitle.keySet().asScala) {
    val surfaceLc = surface.toLowerCase 
    if (surfaceLc != surface) {
      if (!truecaseMap.contains(surfaceLc)) {
        truecaseMap.put(surfaceLc, new ArrayBuffer[String]);
      }
      truecaseMap(surfaceLc) += surface;
    }
    allPossibleTitles ++= surfaceToTitle.getCounter(surface).keySet.asScala;
    allPossibleTitlesWikicase ++= surfaceToTitle.getCounter(surface).keySet.asScala.filter(_.size > 0).map(title => Character.toUpperCase(title.charAt(0)) + title.substring(1));
    allPossibleTitlesLowercase ++= surfaceToTitle.getCounter(surface).keySet.asScala.map(_.toLowerCase);
  }
  
  def disambiguateQueries(queries: Seq[String]) = {
    var result = "";
    for (query <- queries) {
      if (result == "" && surfaceToTitle.containsKey(query) && !surfaceToTitle.getCounter(query).isEmpty) {
        result = surfaceToTitle.getCounter(query).argMax;
      }
    }
    result;
  }
  
  def disambiguateQueriesGetAllReasonableOptions(queries: Seq[String]) = {
    val counter = new Counter[String];
    for (query <- queries) {
      if (counter.isEmpty && surfaceToTitle.containsKey(query)) {
        counter.incrementAll(surfaceToTitle.getCounter(query));
        counter.pruneKeysBelowThreshold(WikipediaTitleGivenSurfaceDB.PruningThreshold);
      }
    }
    counter;
  }
  
  def disambiguateQueriesGetAllOptions(queries: Seq[String]): Counter[String] = {
    val counter = new Counter[String];
    for (query <- queries) {
      if (surfaceToTitle.containsKey(query)) {
        counter.incrementAll(surfaceToTitle.getCounter(query));
        counter.pruneKeysBelowThreshold(WikipediaTitleGivenSurfaceDB.PruningThreshold);
      }
    }
    counter;
  }
  
  def disambiguateQueriesGetAllOneBestOptions(queries: Seq[String]) = {
    val counter = new Counter[String];
    for (query <- queries) {
      if (surfaceToTitle.containsKey(query)) {
        val resultsThisQuery = surfaceToTitle.getCounter(query);
        counter.incrementCount(resultsThisQuery.argMax(), resultsThisQuery.max);
        counter.pruneKeysBelowThreshold(WikipediaTitleGivenSurfaceDB.PruningThreshold);
      }
    }
    counter;
  }
}


object WikipediaTitleGivenSurfaceDB {
  
  val PruningThreshold = 1;
  
  val CapitalizationQueryExpand = false;
  val PluralQueryExpand = true;
  val RemovePuncFromQuery = true;
  val UseFirstHead = true;
  val BlackList = Set("the", "a", "my", "your", "his", "her", "our", "their", "its", "this", "that")
  val PuncList = Set(',', '.', '!', '?', ':', ';', '\'', '"', '(', ')', '[', ']', '{', '}', ' ');

  def isGoodTitle(str: String) = !str.contains("#") && !str.contains(":") && !str.contains("Wikipedia") && !str.startsWith("List of") && !str.startsWith("List_of");
  
  def processWikipedia(wikipediaPath: String, querySet: Set[String]): WikipediaTitleGivenSurfaceDB = {
    val lowercase = false;
    val surfaceToTitle = new CounterMap[String,String];
    val lines = IOUtils.lineIterator(IOUtils.openInHard(wikipediaPath));
    var counter = 0L;
    while (lines.hasNext) {
      counter += 1;
      val line = lines.next;
      if (line.startsWith("    <title>") && line.contains("</title>")) {
        val title = maybeLc(line.substring(line.indexOf("<title>") + 7, line.indexOf("</title>")), lowercase);
        if (querySet.contains(title) && isGoodTitle(title)) {
          // Give titles a fairly hefty count
          surfaceToTitle.incrementCount(title, title, 100.0);
        }
      }
      var startIdx = line.indexOf("[[");
      while (startIdx >= 0 ) {
        val endIdx = line.indexOf("]]", startIdx);
        val pipeIdx = line.indexOf("|", startIdx);
        if (pipeIdx >= 0 && pipeIdx < endIdx) {
          val query = maybeLc(line.substring(pipeIdx + 1, endIdx), lowercase)
          if (querySet.contains(query)) {
            val title = maybeLc(line.substring(startIdx + 2, pipeIdx), lowercase);
            if (isGoodTitle(title)) {
              surfaceToTitle.incrementCount(query, title, 1.0);
            }
          }
        } else if (endIdx >= startIdx + 2) {
          val title = maybeLc(line.substring(startIdx + 2, endIdx), lowercase);
          if (querySet.contains(title) && isGoodTitle(title)) {
            surfaceToTitle.incrementCount(title, title, 1.0);
          }
        }
        startIdx = line.indexOf("[[", startIdx + 2);
      }
    }
    Logger.logss(querySet.size + " queries, " + counter + " lines processed, " + surfaceToTitle.size + " surface strings found, " +
                 surfaceToTitle.totalCount + " total count");
    // .toSeq here to avoid a ConcurrentModificationException
    for (key <- surfaceToTitle.keySet.asScala.toSeq) {
      surfaceToTitle.getCounter(key).pruneKeysBelowThreshold(1.5);
      surfaceToTitle.getCounter(key).removeKey("");
      if (surfaceToTitle.getCounter(key).isEmpty) {
        surfaceToTitle.removeKey(key);
      }
    }
    new WikipediaTitleGivenSurfaceDB(surfaceToTitle);
  }
  
  def main(args: Array[String]) {
//    val querySet = Set("causes", "heavy metals");
//    processWikipedia("data/wikipedia/enwiki-small.xml", querySet);
    
//    Logger.logss(maybeCapitalizationQueryExpansion(Seq("Noncap")));
//    Logger.logss(maybeCapitalizationQueryExpansion(Seq("Atlanta", "GA")));
//    Logger.logss(maybeCapitalizationQueryExpansion(Seq("ATLANTA", "GA")));
  }
}
