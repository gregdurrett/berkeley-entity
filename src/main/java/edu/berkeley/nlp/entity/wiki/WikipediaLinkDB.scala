package edu.berkeley.nlp.entity.wiki

import edu.berkeley.nlp.futile.fig.basic.Indexer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import scala.collection.mutable.HashSet
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.entity.coref.MentionPropertyComputer
import edu.berkeley.nlp.entity.coref.CorefDocAssembler
import edu.berkeley.nlp.entity.ConllDocReader
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.entity.wiki._

@SerialVersionUID(9084163557546777842L)
class WikipediaLinkDB(val pageNameIndex: Indexer[String],
                      val inLinksMap: HashMap[String,Array[Int]],
                      val outLinksMap: HashMap[String,Array[Int]]) extends Serializable {
  var outLinksSetCache: HashMap[String,Set[Int]] = null;
  
  def getOutLinks(title: String) = {
    if (outLinksMap.contains(title)) {
      outLinksMap(title);
    } else {
      Array[Int]();
    }
  }
  
  def getOutLinksSetUseCache(title: String) = {
    if (outLinksMap.contains(title)) {
      if (outLinksSetCache == null) {
        outLinksSetCache = new HashMap[String,Set[Int]];
      }
      if (!outLinksSetCache.contains(title)) {
        if (outLinksSetCache.size > 1000) {
          outLinksSetCache.dropRight(1);
        }
        outLinksSetCache.put(title, outLinksMap(title).toSet);
      }
      outLinksSetCache(title);
    } else {
      Set[Int]();
    }
  }
  
  def computeOutLinkSuffStats(title1: String, title2: String): (Int, Int, Int) = {
    val outLinksTitle1 = getOutLinksSetUseCache(title1);
    val outLinksTitle2 = getOutLinksSetUseCache(title2);
    val intersection = (outLinksTitle1 & outLinksTitle2);
    (intersection.size, outLinksTitle1.size, outLinksTitle2.size);
  }
  
  def doPagesShareOutLink(title1: String, title2: String): Boolean = {
    computeOutLinkSuffStats(title1, title2)._1 > 0;
  }
  
  def doesOneLinkToOther(title1: String, title2: String): Boolean = {
    val outLinksTitle1 = getOutLinks(title1);
    val outLinksTitle2 = getOutLinks(title2);
    outLinksTitle1.contains(pageNameIndex.indexOf(title2)) || outLinksTitle2.contains(pageNameIndex.indexOf(title1)) 
  }
}

object WikipediaLinkDB {
  
  def processWikipedia(wikipediaPath: String, pageTitleSetLc: Set[String]): WikipediaLinkDB = {
    val pageNamesIndex = new Indexer[String];
    val inLinksMap = new HashMap[String,HashSet[Int]];
    val outLinksMap = new HashMap[String,HashSet[Int]];
    val lines = IOUtils.lineIterator(IOUtils.openInHard(wikipediaPath));
    var currentPageTitle = "";
    var linksThisPage = new StringBuilder();
    var doneWithThisPage = false;
    var numPagesSeen = 0;
    var lineIdx = 0;
    var isInText = false;
    val categoryMap = new HashMap[String,ArrayBuffer[String]];
    val infoboxMap = new HashMap[String,String];
    val appositiveMap = new HashMap[String,String];
    // Extract first line that's not in brackets
    while (lines.hasNext) {
      val line = lines.next;
      if (lineIdx % 100000 == 0) {
        println("Line: " + lineIdx + ", processed " + numPagesSeen + " pages");
      }
      lineIdx += 1;
      // 8 because all page lines look like "  <page>" so we just need to catch the next one and skip
      // longer lines
      if (line.size > 8 && doneWithThisPage) {
        // Do nothing
      } else {
        if (line.contains("<page>")) {
          doneWithThisPage = false;
          numPagesSeen += 1;
        } else if (line.contains("<title>")) {
          // 7 = "<title>".length()
          currentPageTitle = line.substring(line.indexOf("<title>") + 7, line.indexOf("</title>"));
          if (!pageTitleSetLc.contains(currentPageTitle.toLowerCase)) {
            doneWithThisPage = true;
          }
        } else if (line.contains("<redirect title")) {
          doneWithThisPage = true;
        }
        var startIdx = line.indexOf("[[");
        while (startIdx >= 0 ) {
          val endIdx = line.indexOf("]]", startIdx);
          val pipeIdx = line.indexOf("|", startIdx);
          val linkDest: String = if (pipeIdx >= 0 && pipeIdx < endIdx) {
            line.substring(startIdx + 2, pipeIdx);
          } else if (endIdx >= startIdx + 2) {
            line.substring(startIdx + 2, endIdx);
          } else {
            ""
          }
          if (linkDest != "") {
            val idx = pageNamesIndex.getIndex(linkDest);
            if (!outLinksMap.contains(currentPageTitle)) {
              outLinksMap.put(currentPageTitle, new HashSet[Int]);
            }
            outLinksMap(currentPageTitle) += idx;
          }
          startIdx = line.indexOf("[[", startIdx + 2);
        }
      }
    }
    val inLinksMapArrs = inLinksMap.map(entry => entry._1 -> entry._2.toArray);
    val outLinksMapArrs = outLinksMap.map(entry => entry._1 -> entry._2.toArray);
    val sizes = Array.tabulate(10)(i => 0);
    for (key <- outLinksMapArrs.keySet) {
      val size = outLinksMapArrs(key).size;
//      Logger.logss(size);
      val exponent = Math.floor(Math.log(size)/Math.log(10)).toInt;
      sizes(exponent) += 1;
    }
    Logger.logss("SIZES: " + sizes.toSeq);
    new WikipediaLinkDB(pageNamesIndex, inLinksMapArrs, outLinksMapArrs);
  }
  
  def main(args: Array[String]) {
    val wi = GUtil.load("data/wikipedia/wiki-model-ace-links.ser.gz").asInstanceOf[WikipediaInterface];
    val linkDB = wi.linksDB;
    val categoryDB = wi.categoryDB;
    val set = Set("Bill Clinton", "President", "White House", "Hospital", "Prime Minister", "Judge")
    for (title1 <- set) {
      for (title2 <- set) {
        if (title1 != title2) {
          Logger.logss(linkDB.computeOutLinkSuffStats(title1, title2));
          Logger.logss(categoryDB.getCategories(title1).toSet.toString + " " + categoryDB.getCategories(title2).toSet.toString);
          Logger.logss((categoryDB.getCategories(title1).toSet & categoryDB.getCategories(title2).toSet).toString);
        }
      }
    }
    
    val basicWikifier = new BasicWikifier(wi);
    val mentionPropertyComputer = new MentionPropertyComputer(None);
    val pmAssembler = CorefDocAssembler(Language.ENGLISH, useGoldMentions = false);
    val gmAssembler = CorefDocAssembler(Language.ENGLISH, useGoldMentions = true);
    val corefDocs = ConllDocReader.loadRawConllDocsWithSuffix("data/ace05/dev", -1, "", Language.ENGLISH).map(doc => gmAssembler.createCorefDoc(doc, mentionPropertyComputer));
    Logger.logss("Loaded docs");
    var countsMat = Array.tabulate(2, 2)((i, j) => 0);
    var countsMatNe = Array.tabulate(2, 2)((i, j) => 0);
    var linksToOtherMatNe = Array.tabulate(2, 2)((i, j) => 0);
    var linksToOtherAndOverlapMatNe = Array.tabulate(2, 2)((i, j) => 0);
    var categoryMatchMatNe = Array.tabulate(2, 2)((i, j) => 0);
    for (corefDoc <- corefDocs.slice(0, 10)) {
      val goldClusters = corefDoc.getOraclePredClustering;
      val wiks = (0 until corefDoc.predMentions.size).map(i => basicWikifier.wikify(corefDoc.rawDoc.docID, corefDoc.predMentions(i)));
      for (mentIdx <- 0 until corefDoc.predMentions.size) {
        for (antIdx <- 0 until mentIdx) {
          if (wiks(mentIdx) != NilToken && wiks(antIdx) != NilToken) {
            var firstIdx = if (goldClusters.areInSameCluster(mentIdx, antIdx)) 0 else 1;
            var secondIdx = if (linkDB.computeOutLinkSuffStats(wiks(mentIdx), wiks(antIdx))._1 > 0) 0 else 1;
            countsMat(firstIdx)(secondIdx) += 1;
            if (wiks(mentIdx) != wiks(antIdx)) {
              countsMatNe(firstIdx)(secondIdx) += 1;
              linksToOtherMatNe(firstIdx)(if (linkDB.doesOneLinkToOther(wiks(mentIdx), wiks(antIdx))) 0 else 1) += 1;
              linksToOtherAndOverlapMatNe(firstIdx)(if (linkDB.doesOneLinkToOther(wiks(mentIdx), wiks(antIdx)) && secondIdx == 0) 0 else 1) += 1;
              categoryMatchMatNe(firstIdx)(if ((categoryDB.getCategories(wiks(mentIdx)).toSet & categoryDB.getCategories(wiks(antIdx)).toSet).size > 0) 0 else 1) += 1;
            }
          }
        }
      }
    }
    Logger.logss(countsMat(0)(0) + "\t" + countsMat(0)(1))
    Logger.logss(countsMat(1)(0) + "\t" + countsMat(1)(1))
    Logger.logss("Restricted to pairs with unequal Wikification");
    Logger.logss(countsMatNe(0)(0) + "\t" + countsMatNe(0)(1))
    Logger.logss(countsMatNe(1)(0) + "\t" + countsMatNe(1)(1))
    Logger.logss("Links to other");
    Logger.logss(linksToOtherMatNe(0)(0) + "\t" + linksToOtherMatNe(0)(1))
    Logger.logss(linksToOtherMatNe(1)(0) + "\t" + linksToOtherMatNe(1)(1))
    Logger.logss("Links to other and overlap");
    Logger.logss(linksToOtherAndOverlapMatNe(0)(0) + "\t" + linksToOtherAndOverlapMatNe(0)(1))
    Logger.logss(linksToOtherAndOverlapMatNe(1)(0) + "\t" + linksToOtherAndOverlapMatNe(1)(1))
    Logger.logss("Category overlap");
    Logger.logss(categoryMatchMatNe(0)(0) + "\t" + categoryMatchMatNe(0)(1))
    Logger.logss(categoryMatchMatNe(1)(0) + "\t" + categoryMatchMatNe(1)(1))
  }
}
