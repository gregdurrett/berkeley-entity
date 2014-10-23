package edu.berkeley.nlp.entity.wiki

import edu.berkeley.nlp.futile.fig.basic.IOUtils
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.futile.util.Counter
import scala.collection.JavaConverters._
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.wiki._

@SerialVersionUID(1L)
class WikipediaRedirectsDB(val redirects: HashMap[String,String]) extends Serializable {
  val redirectsWikicase = new HashMap[String,String];
  redirects.foreach(redirect => redirectsWikicase += wikiCase(redirect._1) -> redirect._2);
  val possibleRedirectTargets = redirects.map(_._2).toSet;
  val possibleRedirectTargetsLc = redirects.map(_._2.toLowerCase).toSet;
  
  def followRedirect(title: String) = {
//    val print = title == "student_association";
    val print = false
    // Try to redirect
    val result = if (redirects.contains(title)) {
      if (print) Logger.logss("1 " + redirects(title));
      redirects(title);
    } else if (redirectsWikicase.contains(wikiCase(title))){
      if (print) Logger.logss("3 " + redirectsWikicase(wikiCase(title)));
      redirectsWikicase(wikiCase(title));
    } else if (WikipediaRedirectsDB.CapitalizeInitial) {
      if (print) Logger.logss("4 " + wikiCase(title));
      wikiCase(title)
    } else {
      if (print) Logger.logss("5 " + title);
      title;
    }
    WikipediaRedirectsDB.removeWeirdMarkup(result);
  }
  
  def followRedirectsCounter(titleCounts: Counter[String]) = {
    val newTitleCounts = new Counter[String];
    for (title <- titleCounts.keySet.asScala) {
      newTitleCounts.incrementCount(followRedirect(title), titleCounts.getCount(title));
    }
    newTitleCounts;
  }
}

object WikipediaRedirectsDB {
  
  val CapitalizeInitial = true;
  
  def removeWeirdMarkup(str: String) = {
    str.replace("&#039;", "'");
  }
  
  def processWikipediaGetRedirects(wikipediaPath: String, redirectCandidates: Set[String]) = {
    val redirects = new HashMap[String,String];
    val lines = IOUtils.lineIterator(IOUtils.openInHard(wikipediaPath));
    var lineIdx = 0;
    
    var currentPageTitle = "";
    var doneWithThisPage = true;
    while (lines.hasNext) {
      val line = lines.next;
      if (lineIdx % 100000 == 0) {
        println("Line: " + lineIdx + ", processed");
      }
      lineIdx += 1;
      if (line.contains("<page>")) {
          doneWithThisPage = false;
      } else if (!doneWithThisPage && line.contains("<title>")) {
        // 7 = "<title>".length()
        currentPageTitle = line.substring(line.indexOf("<title>") + 7, line.indexOf("</title>")).replace(" ", "_");
        if (!redirectCandidates.contains(currentPageTitle)) {
          doneWithThisPage = true;
        }
      } else if (!doneWithThisPage && line.contains("<redirect title")) {
        val startIdx = line.indexOf("\"") + 1;
        val endIdx = line.indexOf("\"", startIdx);
        val redirectTitle = line.substring(startIdx, endIdx).replace(" ", "_");
        if (redirectCandidates.contains(currentPageTitle)) {
          redirects.put(currentPageTitle, redirectTitle);
        }
      }
    }
    redirects;
  }
  
  def processWikipedia(wikipediaPath: String, titleGivenSurfaceDB: WikipediaTitleGivenSurfaceDB): WikipediaRedirectsDB = {
    val lowercase = false
    val lines = IOUtils.lineIterator(IOUtils.openInHard(wikipediaPath));
    var currentPageTitle = "";
    var doneWithThisPage = false;
    
    var numPagesSeen = 0;
    var lineIdx = 0;
    
    val redirects = new HashMap[String,String];
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
          currentPageTitle = maybeLc(line.substring(line.indexOf("<title>") + 7, line.indexOf("</title>")), lowercase);
        } else if (line.contains("<redirect title")) {
          val startIdx = line.indexOf("\"") + 1;
          val endIdx = line.indexOf("\"", startIdx);
          val redirectTitle = maybeLc(line.substring(startIdx, endIdx), lowercase);
          if (titleGivenSurfaceDB.allPossibleTitlesLowercase.contains(currentPageTitle.toLowerCase)) {
            redirects.put(currentPageTitle, redirectTitle);
          }
          doneWithThisPage = true;
        }
      }
    }
    new WikipediaRedirectsDB(redirects);
  }
}
