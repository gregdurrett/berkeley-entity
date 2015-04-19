package edu.berkeley.nlp.entity.wiki

import edu.berkeley.nlp.futile.fig.basic.{IOUtils, Indexer}
import edu.berkeley.nlp.futile.util.Counter

import scala.collection.JavaConversions._


import scala.StringBuilder
import scala.collection.mutable

/**
 * Created by matthewfl
 *
 * Provide bow counts for documents so we can compute the similarity between two documents
 */
@SerialVersionUID(1L)
class WikipediaTextDB (val indexer: Indexer[String], val words: mutable.HashMap[String, Array[Int]]) extends Serializable {

  def getDocument(title: String) = words.getOrElse(title, Array[Int]())

  def compareVectors(a: Array[Int], b: Array[Int]) = {
    var ai = 0
    var bi = 0
    var simcnt = 0
    while(ai < a.size && bi < b.size) {
      if(a(ai) == b(bi)) {
        simcnt += 1
        ai += 1
        bi += 1
      } else if(a(ai) > b(bi)) {
        bi += 1
      } else {
        ai += 1
      }
    }
    simcnt
  }

  def compareTitles(atitle: String, btitle: String) = compareVectors(getDocument(atitle), getDocument(btitle))

  def makeVector(document: Seq[Seq[String]]) = {
    document.flatMap(_.map(v => indexer.indexOf(v.toLowerCase))).toSet.filter(_ != -1).toArray.sorted
  }

  def compareDocument(doc: Array[Int], title: String) = compareVectors(doc, getDocument(title))

  def compareDocumentC(doc: Array[Int], title: String) = {
    val tdoc = getDocument(title)
    compareVectors(doc, tdoc).asInstanceOf[Double] / (doc.size * tdoc.size)
  }

}

object WikipediaTextDB {
  def processWikipedia(wikipediaPath:String, querySet: Set[String]) : WikipediaTextDB = {
    val lines = IOUtils.lineIterator(IOUtils.openInHard(wikipediaPath));
    var currentPageTitle: String = null
    val indexer = new Indexer[String]
    val totalWordCounts = new Counter[Int]
    var currentWordCounts = new mutable.HashSet[Int]
    val documentResults = new mutable.HashMap[String,Array[Int]]
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
          val newPageTitle = line.substring(line.indexOf("<title>") + 7, line.indexOf("</title>"));
          if (!querySet.contains(newPageTitle.toLowerCase)) {
            doneWithThisPage = true;
          } else {
            if(currentPageTitle != null) {
              documentResults += (currentPageTitle -> currentWordCounts.toArray)
            }
            currentWordCounts = new mutable.HashSet[Int]()
            currentPageTitle = newPageTitle
          }
        } else if(line.contains("<text")) {
          val textStart = line.indexOf(">") + 1
          val document = new StringBuilder()
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
            val i = indexer.getIndex(w.toLowerCase)
            totalWordCounts.incrementCount(i, 1.0)
            currentWordCounts += i
            //currentWordCounts.incrementCount(i, 1.0)
          })
        }
      }
    }

    // get the 300 most common words and remove them from all the documents
    val wrdsq = totalWordCounts.asPriorityQueue
    val removeWords = new mutable.HashSet[Int]()
    for(i <- 0 until 300; if wrdsq.hasNext)
      removeWords += wrdsq.next
    for(k <- documentResults) {
      documentResults(k._1) = k._2.filter(!removeWords.contains(_)).sorted
    }


    new WikipediaTextDB(indexer, documentResults)
  }
}
