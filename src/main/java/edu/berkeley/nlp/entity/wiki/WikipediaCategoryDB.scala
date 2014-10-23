package edu.berkeley.nlp.entity.wiki

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.entity.wiki.BlikiInterface
import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser
import edu.berkeley.nlp.entity.preprocess.Reprocessor
import edu.berkeley.nlp.entity.preprocess.PreprocessingDriver
import edu.berkeley.nlp.entity.preprocess.SentenceSplitter
import edu.berkeley.nlp.entity.DepConstTree

@SerialVersionUID(1L)
class WikipediaCategoryDB(val categoryMap: HashMap[String,ArrayBuffer[String]],
                          val infoboxMap: HashMap[String,String],
                          val appositiveMap: HashMap[String,String]) extends Serializable {
  val categoryFreqs = new Counter[String]();
  categoryMap.foreach(entry => entry._2.foreach(categoryFreqs.incrementCount(_, 1.0)));
  val infoboxFreqs = new Counter[String];
  infoboxMap.foreach(entry => infoboxFreqs.incrementCount(entry._2, 1.0));
  
  def getCategories(pageName: String): Seq[String] = {
    if (categoryMap.contains(pageName)) {
      require(categoryMap(pageName) != null);
      categoryMap(pageName).distinct;
    } else {
      Seq[String]();
    }
  }
  
  def getCategoriesSortedByFrequency(pageName: String): Seq[String] = {
    val categories = getCategories(pageName);
    val categoriesAndCounts = categories.map(category => category -> categoryFreqs.getCount(category));
    categoriesAndCounts.sortBy(_._2).map(_._1).distinct;
  }
  
  def getTopKCategoriesByFrequency(title: String, k: Int) = {
    val categories = getCategoriesSortedByFrequency(title);
    categories.slice(0, Math.min(k, categories.size));
  }
  
  def getInfobox(pageName: String): String = {
    if (infoboxMap.contains(pageName)) {
      infoboxMap(pageName);
    } else {
      ""
    }
  }
  
  def getInfoboxHead(pageName: String): String = {
    if (infoboxMap.contains(pageName)) {
      infoboxMap(pageName).split("\\s+").last;
    } else {
      ""
    }
  }
  
  def getAppositive(pageName: String): String = {
    if (appositiveMap.contains(pageName)) {
      appositiveMap(pageName);
    } else {
      ""
    }
  }
}

object WikipediaCategoryDB {
  
  val Debug = false;
  
  val RatinovRelationTokens = Set("of", "in", "with", "from", ",", "at", "who", "which", "for", "and", "by");
//  val BlacklistedCategories = Set[String]("birth", "death");
  val BlacklistedCategories = Set[String]();
  val CopulaVerbs = Set("is", "was", "are", "were", "be")
  
  def processWikipedia(wikipediaPath: String,
                       pageTitleSetLc: Set[String],
                       parser: CoarseToFineMaxRuleParser,
                       backoffParser: CoarseToFineMaxRuleParser): WikipediaCategoryDB = {
    processWikipediaOld(wikipediaPath, pageTitleSetLc, parser, backoffParser);
//    Logger.logss("NEW");
//    processWikipediaNew(wikipediaPath, pageTitleSetLc);
  }
  
  def identifyFirstGoodPeriodIdx(line: String): Int = {
    var foundGoodPeriodIdx = false;
    var periodIdx = line.indexOf(".")
    while (!foundGoodPeriodIdx && periodIdx != -1) {
      // Check for badness
      if (periodIdx >= 2 && line.substring(periodIdx - 2, periodIdx + 2) == " c. ") {
        periodIdx = line.indexOf(".", periodIdx + 1);
      } else {
        foundGoodPeriodIdx = true;
      }
    }
    periodIdx
  }
  
  def identifySentenceBreakIdx(line: String): Int = {
    val periodIndex = if (line.indexOf(".") != -1) line.indexOf(".") + 1 else -1;
    val brIndex = if (line.indexOf("<br>") != -1) line.indexOf("<br>") else -1;
    if (periodIndex >= 0 || brIndex >= 0) {
      Math.min(if (periodIndex < 0) line.size else periodIndex, if (brIndex < 0) line.size else brIndex)
    } else {
      -1;
    }
  }
  
  def identifyAppositiveNounOrNone(sentence: String, parser: CoarseToFineMaxRuleParser, backoffParser: CoarseToFineMaxRuleParser) = {
    val sentenceNoParens = BlikiInterface.removeParentheticals(sentence);
    var result = "";
    if (sentenceNoParens.contains("==") || sentenceNoParens.split("\\s+").size > 50) {
      if (Debug) Logger.logss("Bad sentence: " + sentenceNoParens)
    } else {
      val sentWords = SentenceSplitter.tokenizeSingle(sentenceNoParens.replaceAll("\\s+", " ")).toSeq;
      val rawParserOutputTree = PreprocessingDriver.parseSoft(parser, backoffParser, sentWords.asJava);
      if (rawParserOutputTree != null) {
        val tree = Reprocessor.convertToFutileTree(rawParserOutputTree);
        // TODO: Look for is, was, are, were, be
        val depConstTree = DepConstTree(tree);
        val copulaIndices = (0 until depConstTree.size).filter(i => CopulaVerbs.contains(depConstTree.words(i).toLowerCase));
        val copulaImmediateRightChildren = copulaIndices.flatMap(copulaIdx => {
          val allChildren = (copulaIdx + 1 until depConstTree.size).filter(childIdx => depConstTree.childParentDepMap(childIdx) == copulaIdx);
          if (allChildren.size > 0) Seq(allChildren.min) else Seq();
        });
        if (copulaImmediateRightChildren.size >= 1) {
          result = depConstTree.words(copulaImmediateRightChildren(0)).toLowerCase;
        }
        if (copulaImmediateRightChildren.size != 1) {
          if (Debug) Logger.logss("Wrong number of copula right children: " + copulaImmediateRightChildren.map(depConstTree.words(_)));
        }
      }
    }
    result;
  }

  def processWikipediaOld(wikipediaPath: String,
                          pageTitleSetLc: Set[String],
                          parser: CoarseToFineMaxRuleParser,
                          backoffParser: CoarseToFineMaxRuleParser): WikipediaCategoryDB = {
    val lines = IOUtils.lineIterator(IOUtils.openInHard(wikipediaPath));
    var currentPageTitle = "";
    var firstParagraphStringBuilder = new StringBuilder();
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
        var lineStart = 0;
        if ((line.startsWith("{{infobox ") || line.startsWith("{{Infobox ")) && !infoboxMap.contains(currentPageTitle)) {
          val startIdx = 10; // "{{infobox ".size
          // Some infoboxes are followed by pipes (|) or comments (<!-- = &lt;!--)
          val pipeIdx = if (line.indexOf("|") >= 0) line.indexOf("|") else line.size;
          val ampIdx = if (line.indexOf("&") >= 0) line.indexOf("&") else line.size;
          val endIdx = Math.min(Math.min(pipeIdx, ampIdx), line.size);
          infoboxMap.put(currentPageTitle, line.substring(startIdx, endIdx).trim.toLowerCase);
        }
        if (!doneWithThisPage && line.contains("<text")) {
          val textIdx = line.indexOf("<text");
          val endTagIdx = line.indexOf(">", textIdx) + 1;
          lineStart = endTagIdx;
          isInText = true;
        }
        if (!doneWithThisPage && isInText) {
          // Category identification
          if (line.startsWith("[[Category:") && line.contains("]]")) {
            val category = line.substring(line.indexOf("[[Category:") + "[[Category:".size, line.indexOf("]]"));
            if (!categoryMap.contains(currentPageTitle)) {
              categoryMap.put(currentPageTitle, new ArrayBuffer[String]);
            }
            categoryMap(currentPageTitle) ++= WikipediaCategoryDB.extractFromCategory(category);
          }
          // First sentence identification
          if (line.contains("</text>")) {
            val restOfLine = if (line.contains("</text>")) line.substring(lineStart, line.indexOf("</text>")) else line;
            firstParagraphStringBuilder = firstParagraphStringBuilder.append(restOfLine);
            val plaintext = BlikiInterface.renderPlaintext(firstParagraphStringBuilder.toString).trim.replaceAll("\n", " ");
            if (Debug && plaintext.size == 0) {
              Logger.logss("WARNING: zero-length first snippet for " + currentPageTitle + " after removing markup");
            }
            if (Debug && !plaintext.contains(".")) {
              Logger.logss("WARNING: no period in snippet for " + currentPageTitle + ": " + plaintext);
            }
            val endOfSnippet = identifySentenceBreakIdx(plaintext);
            val sentence = plaintext.substring(0, if (endOfSnippet == -1) plaintext.size else endOfSnippet);
            if (parser != null && backoffParser != null) {
              val appositive = identifyAppositiveNounOrNone(sentence, parser, backoffParser);
              if (appositive != "") {
                if (Debug) Logger.logss(currentPageTitle + " -> " + appositive)
                appositiveMap.put(currentPageTitle, appositive);
              }
            }
            doneWithThisPage = true;
            isInText = false;
            firstParagraphStringBuilder = new StringBuilder();
          } else {
            val partialLine = if (lineStart == 0) line else line.substring(lineStart);
            firstParagraphStringBuilder = firstParagraphStringBuilder.append(" ").append(partialLine);
          }
        }
      }
    }
    // Prune low-frequency infobox entries
    val freqCutoff = 1;
    val infoboxCounts = new Counter[String];
    infoboxMap.values.foreach(infoboxCounts.incrementCount(_, 1.0));
    Logger.logss(infoboxMap.size + " entries before filtering for one-counts");
    for (key <- infoboxMap.keySet.toSeq) {
      // Prune low-count keys
      if (infoboxCounts.getCount(infoboxMap(key)) <= freqCutoff) {
        if (Debug) Logger.logss("Removing " + key + " -> " + infoboxMap(key) + " with count " + infoboxCounts.getCount(infoboxMap(key)));
        infoboxMap.remove(key);
      } else {
        if (Debug) Logger.logss("Keeping " + key + " -> " + infoboxMap(key))
      }
    }
    infoboxCounts.pruneKeysBelowThreshold(freqCutoff + 0.5)
    Logger.logss(infoboxMap.size + " infobox entries retrieved, with " + infoboxCounts.size + " values above frequency cutoff")
    new WikipediaCategoryDB(categoryMap, infoboxMap, appositiveMap);
  }
  
//  def processWikipediaNew(wikipediaPath: String, pageTitleSetLowercase: Set[String]): WikipediaCategoryDB = {
//    val categoryMap = new HashMap[String,ArrayBuffer[String]];
//    val lines = IOUtils.lineIterator(IOUtils.openInHard(wikipediaPath));
//    var counter = 0L;
//    var currTitle = "";
//    while (lines.hasNext) {
//      counter += 1;
//      val line = lines.next;
//      if (line.startsWith("    <title>") && line.contains("</title>")) {
//        val title = line.substring(line.indexOf("<title>") + 7, line.indexOf("</title>")).replace(" ", "_");
//        if (pageTitleSetLowercase.contains(title.toLowerCase)) {
//          currTitle = title;
//        } else {
//          currTitle = "";
//        }
//      }
//      if (!currTitle.isEmpty && line.startsWith("[[Category") && line.contains("]]")) {
//        val category = line.substring(line.indexOf("[[Category:") + "[[Category:".size, line.indexOf("]]"));
//        if (!categoryMap.contains(currTitle)) {
//          categoryMap.put(currTitle, new ArrayBuffer[String]);
//        }
//        categoryMap(currTitle) ++= extractFromCategory(category);
//      }
//    }
//    Logger.logss(categoryMap.size + " category map entries extracted out of " + pageTitleSetLowercase.size + " possible titles");
//    new WikipediaCategoryDB(categoryMap);
//  }
  
  def extractFromCategory(fullCategoryName: String): Set[String] = {
    val tokens = fullCategoryName.split("\\s+");
    // Tag and only extract things which are NNS
    val relTokenIndices = tokens.map(RatinovRelationTokens.contains(_)).zipWithIndex.filter(_._1).map(_._2);
    val relevantTokenIdx = if (relTokenIndices.size == 0) {
      tokens.size - 1
    } else {
      val firstIdx = relTokenIndices.head;
      if (firstIdx > 0) firstIdx - 1 else -1;
    }
    if (relevantTokenIdx == -1 || !tokens(relevantTokenIdx).endsWith("s")) {
      Set();
    } else {
      val shortenedLcToken = tokens(relevantTokenIdx).dropRight(1).toLowerCase;
      if (!BlacklistedCategories.contains(shortenedLcToken)) {
        Set(shortenedLcToken)
      } else {
        Set();
      }
    }
  }
}
