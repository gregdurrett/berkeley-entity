package edu.berkeley.nlp.entity.wiki

import edu.berkeley.nlp.futile.fig.basic.{IOUtils, Indexer}
import edu.berkeley.nlp.futile.util.Counter

import scala.collection.JavaConversions._


import scala.StringBuilder
import scala.collection.mutable

/**
 * Created by matthewfl
 *
 * Provide proxy bow counts for documents so we can compute the similarity between two documents
 */
@SerialVersionUID(1L)
class WikipediaTextDB (val indexer: Indexer[String], val words: mutable.HashMap[String, Counter[Int]]) {


}

object WikipediaTextDB {
  def processWikipedia(wikipediaPath:String, querySet: Set[String]) : WikipediaTextDB = {
    val lines = IOUtils.lineIterator(IOUtils.openInHard(wikipediaPath));
    var currentPageTitle: String = null
    val indexer = new Indexer[String]
    val totalWordCounts = new Counter[Int]
    var currentWordCounts = new Counter[Int]
    val documentResults = new mutable.HashMap[String,Counter[Int]]
    var lineIdx = 0
    var numPagesSeen = 0
    var doneWithThisPage = false

    while(lines.hasNext) {
      val line = lines.next
      if (lineIdx % 100000 == 0) {
        println("Line: " + lineIdx + ", processed " + numPagesSeen + " pages");
      }
      lineIdx += 1;
      if (line.size > 8 && doneWithThisPage) {
        // Do nothing
      } else {
        if(line.contains("<page>")) {
          doneWithThisPage = false
          numPagesSeen += 1
        } else if (line.contains("<title>")) {
          // 7 = "<title>".length()
          currentPageTitle = line.substring(line.indexOf("<title>") + 7, line.indexOf("</title>"));
          if (!querySet.contains(currentPageTitle.toLowerCase)) {
            doneWithThisPage = true;
          } else {
            currentWordCounts = new Counter[Int]()
            documentResults += (currentPageTitle -> currentWordCounts)
          }
        } else if(line.contains("<text")) {
          val textStart = line.indexOf(">") + 1
          var document = new StringBuilder()
          var textEnd = line.indexOf("</text>")
          if(textEnd != -1) {
            document.append(line.substring(textStart, textEnd))
          } else {
            var curLine = line.substring(textStart)
            while(textEnd == -1) {
              document.append(curLine)
              curLine = lines.next
              textEnd = curLine.indexOf("</text>")
            }
            document.append(curLine.substring(0, textEnd))
          }
          // TODO: maybe toSet
          document.toString.split("[^A-Za-z]").foreach(w => {
            val i = indexer.getIndex(w)
            totalWordCounts.incrementCount(i, 1.0)
            currentWordCounts.incrementCount(i, 1.0)
          })
        }
      }
    }

    // get the 300 most common words and remove them from all the documents
    val wrdsq = totalWordCounts.asPriorityQueue
    val removeWords = new mutable.HashSet[Int]()
    for(i <- 0 until 300; if wrdsq.hasNext)
      removeWords += wrdsq.next
    documentResults.foreach(_._2.prune(removeWords))


    new WikipediaTextDB(indexer, documentResults)
  }
}
