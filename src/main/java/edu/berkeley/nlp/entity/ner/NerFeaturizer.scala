package edu.berkeley.nlp.entity.ner

import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.entity.wiki.WikipediaInterface
import scala.collection.mutable.HashSet

@SerialVersionUID(1L)
class NerFeaturizer(val featureSet: Set[String],
                    val featureIndexer: Indexer[String],
                    val labelIndexer: Indexer[String],
                    val corpusCounts: CorpusCounts,
                    val wikipediaDB: Option[WikipediaInterface] = None,
                    val brownClusters: Option[Map[String,String]] = None) extends Serializable {
  val useBrown = brownClusters.isDefined;
  val useBackoffs = featureSet.contains("backoff");
  val useWikipedia = featureSet.contains("wikipedia") && wikipediaDB.isDefined;
  val useExtraBrown = featureSet.contains("extra-brown") && brownClusters.isDefined;
  val usePrefSuff = featureSet.contains("prefsuff");
  
  def replaceIndexer(newIndexer: Indexer[String]) = {
    new NerFeaturizer(featureSet, newIndexer, labelIndexer, corpusCounts, wikipediaDB, brownClusters);
  }
      
  def featurize(ex: NerExample, addToIndexer: Boolean, rangeRestriction: Option[(Int,Int)] = None): Array[Array[Array[Int]]] = {
    val range = if (rangeRestriction.isDefined) rangeRestriction.get else (0, ex.words.size);
    val cachedShapes = ex.words.map(NerFeaturizer.shapeFor(_));
    val cachedClasses = ex.words.map(NerFeaturizer.classFor(_));
    val cachedPrefixes = ex.words.map(NerFeaturizer.prefixFor(_))
    val cachedSuffixes = ex.words.map(NerFeaturizer.suffixFor(_))
    val wikiFeats = if (useWikipedia) {
      computeWikipediaSurfaceFeatures(ex.words);
    } else {
      (0 until ex.words.size).map(tokIdx => Seq[String]()); 
    }
    Array.tabulate(range._2 - range._1)(tokIdxOffset => {
      val tokIdx = tokIdxOffset + range._1;
      val wordAt = (i: Int) => ex.wordAt(tokIdx + i); 
      val posAt = (i: Int) => ex.posAt(tokIdx + i);
      val wordShapeAt = (i: Int) => if (tokIdx + i < 0) "<<START>>" else if (tokIdx + i >= ex.words.size) "<<END>>" else cachedShapes(tokIdx + i);
      val wordClassAt = (i: Int) => if (tokIdx + i < 0) "<<START>>" else if (tokIdx + i >= ex.words.size) "<<END>>" else cachedClasses(tokIdx + i);
      val prefixAt = (i: Int) => if (tokIdx + i < 0) "<ST>" else if (tokIdx + i >= ex.words.size) "<EN>" else cachedPrefixes(tokIdx + i);
      val suffixAt = (i: Int) => if (tokIdx + i < 0) "<ST>" else if (tokIdx + i >= ex.words.size) "<EN>" else cachedSuffixes(tokIdx + i);
      val cachedSurfaceFeats = new ArrayBuffer[String];
      
      for (offset <- -2 to 2) {
        if (corpusCounts.unigramCounts.containsKey(wordAt(offset))) cachedSurfaceFeats += offset + "W=" + wordAt(offset);
        cachedSurfaceFeats += offset + "P=" + posAt(offset);
        cachedSurfaceFeats += offset + "S=" + wordShapeAt(offset);
        cachedSurfaceFeats += offset + "C=" + wordClassAt(offset);
        if (usePrefSuff) {
          if (corpusCounts.prefixCounts.containsKey(prefixAt(offset))) cachedSurfaceFeats += offset + "PR=" + prefixAt(offset);
          if (corpusCounts.suffixCounts.containsKey(suffixAt(offset))) cachedSurfaceFeats += offset + "SU=" + suffixAt(offset);
        }
        if (offset < 2) {
          cachedSurfaceFeats += offset + "SS=" + wordShapeAt(offset) + "," + wordShapeAt(offset+1);
          val bigram = wordAt(offset) -> wordAt(offset+1);
          if (corpusCounts.bigramCounts.containsKey(bigram)) {
            cachedSurfaceFeats += offset + "WW=" + wordAt(offset) + "," + wordAt(offset+1);
          }
        }
      }
      cachedSurfaceFeats ++= wikiFeats(tokIdx);
      if (useBrown) {
        for (offset <- if (useExtraBrown) (-1 to 1) else (0 to 0)) {
          val brownStr = if (brownClusters.get.contains(wordAt(offset))) brownClusters.get(wordAt(offset)) else "";
          // 4, 6, 10, 20 is prescribed by Ratinov et al. (2009) and Turian et al. (2010)
          cachedSurfaceFeats += offset + "B4=" + brownStr.substring(0, Math.min(brownStr.size, 4));
          cachedSurfaceFeats += offset + "B6=" + brownStr.substring(0, Math.min(brownStr.size, 6));
          cachedSurfaceFeats += offset + "B10=" + brownStr.substring(0, Math.min(brownStr.size, 10));
          cachedSurfaceFeats += offset + "B20=" + brownStr.substring(0, Math.min(brownStr.size, 20));
        }
      }
      Array.tabulate(labelIndexer.size)(labelIdx => {
        val nerLabel = labelIndexer.getObject(labelIdx);
        val structuralType = NerSystemLabeled.getStructuralType(nerLabel);
        if (tokIdx == 0 && structuralType == "I") {
          null;
        } else {
          val feats = new ArrayBuffer[Int]();
          for (feat <- cachedSurfaceFeats) {
            val labeledFeat = feat + ":" + nerLabel;
            if (addToIndexer || featureIndexer.contains(labeledFeat)) feats += featureIndexer.getIndex(labeledFeat)
            if (useBackoffs && structuralType != "O") {
              val partiallyLabeledFeat = feat + ":" + structuralType;
              if (addToIndexer || featureIndexer.contains(partiallyLabeledFeat)) feats += featureIndexer.getIndex(partiallyLabeledFeat)
            }
          }
          feats.toArray;
        }
      });
    });
  }
  
  private def computeWikipediaSurfaceFeatures(words: Seq[String], numCategories: Int = 1): Seq[Seq[String]] = {
    val surfaceToTitle = wikipediaDB.get.titleGivenSurfaceDB.surfaceToTitle;
//    val possibleSurfaceStrings = .keySet;
    val featsEachSpan = Array.tabulate(words.size)(i => new ArrayBuffer[String]);
    for (i <- 0 until words.size; j <- 0 until words.size + 1) {
      // Cap at length 5
      if (i < j && i + 5 >= j) {
        val span = words.slice(i, j).reduce(_ + " " + _);
        if (!corpusCounts.mostCommonUnigrams.contains(span) && surfaceToTitle.keySet.contains(span)) {
          val title = surfaceToTitle.getCounter(span).argMax;
//          Logger.logss("Identified span " + span + " as referring to " + title);
          val categories = wikipediaDB.get.getCategoriesSortedByFrequency(title);
          val categoriesToUse = categories.slice(0, Math.min(categories.size, numCategories));
          for (k <- i until j) {
            for (category <- categoriesToUse) {
              featsEachSpan(k) += "Wiki=" + (if (k == i) "B-" else if (k == j - 1) "E-" else "I-") + category;  
            }
          }
          title
        } else {
          ""
        }
      } else {
        ""
      }
    }
    featsEachSpan;
  }
  
  def featurizeTransition(prevLabel: String, currLabel: String, addToIndexer: Boolean): Array[Int] = {
    if (!NerFeaturizer.isLegalTransition(prevLabel, currLabel)) {
      null;
    } else {
      val feat = "T=" + prevLabel + ":" + currLabel;
      if (addToIndexer || featureIndexer.contains(feat)) Array(featureIndexer.getIndex(feat)) else Array[Int]();
    }
  }
}

object NerFeaturizer {
  
  def apply(featureSet: Set[String],
            featureIndexer: Indexer[String],
            labelIndexer: Indexer[String],
            rawSents: Seq[Seq[String]],
            wikipediaDB: Option[WikipediaInterface],
            brownClusters: Option[Map[String,String]],
            unigramThreshold: Int = 1,
            bigramThreshold: Int = 10,
            prefSuffThreshold: Int = 2) = {
    val corpusCounts = CorpusCounts.countUnigramsBigrams(rawSents, unigramThreshold, bigramThreshold, prefSuffThreshold);
    new NerFeaturizer(featureSet, featureIndexer, labelIndexer, corpusCounts, wikipediaDB, brownClusters);
  }
  
  // Going to I-X, we can only come from B-X or I-X
  def isLegalTransition(prevLabel: String, currLabel: String) = {
    if (currLabel.startsWith("I")) {
      if (prevLabel == "O") {
        false
      } else {
        val prevType = if (prevLabel.indexOf("-") != -1) prevLabel.substring(prevLabel.indexOf("-") + 1) else "";
        val currType = if (currLabel.indexOf("-") != -1) currLabel.substring(currLabel.indexOf("-") + 1) else "";
        prevType == currType;
      }
    } else {
      true;
    }
  }
  
  def prefixFor(word: String) = word.substring(0, Math.min(4, word.size));
  
  def suffixFor(word: String) = word.substring(Math.max(0, word.size - 4), word.size);
  
  def shapeFor(word: String) = {
    val result = new StringBuilder(word.length);
    var i = 0;
    while (i < word.length) {
      val c = word(i);
      val x = if (c.isLetter && c.isUpper) 'X' else if (c.isLetter) 'x' else if (c.isDigit) 'd' else c;
      if (result.length > 1 && (result.last == x) && result(result.length - 2) == x) {
        result += 'e'
      } else if (result.length > 1 && result.last == 'e' && result(result.length - 2) == x) {
        () // nothing
      } else {
        result += x;
      }
      i += 1;
    }
    result.toString
  }
    
  def classFor(word: String) = {
    val sb = new StringBuilder;
    val wlen = word.length();
    val numCaps = (word: Seq[Char]).count(_.isUpper);
    val hasDigit = word.exists(_.isDigit);
    val hasDash = word.contains('-');
    val hasLower = numCaps < wlen;
    val ch0 = word.charAt(0);
    val lowered = word.toLowerCase();
    if (Character.isUpperCase(ch0) || Character.isTitleCase(ch0)) {
      if (numCaps == 1) {
        sb.append("-INITC");
      } else {
        sb.append("-CAPS");
      }
    } else if (!Character.isLetter(ch0) && numCaps > 0) {
      sb.append("-CAPS");
    } else if (hasLower) {
      sb.append("-LC");
    }

    if (hasDigit) {
      sb.append("-NUM");
    }
    if (hasDash) {
      sb.append("-DASH");
    }
    if (lowered.endsWith("s") && wlen >= 3) {
      // here length 3, so you don't miss out on ones like 80s
      val ch2 = lowered.charAt(wlen - 2);
      // not -ess suffixes or greek/latin -us, -is
      if (ch2 != 's' && ch2 != 'i' && ch2 != 'u') {
        sb.append("-s");
      }
    } else if (word.length() >= 5 && !hasDash && !(hasDigit && numCaps > 0)) {
      if (lowered.endsWith("ed")) {
        sb.append("-ed");
      } else if (lowered.endsWith("ing")) {
        sb.append("-ing");
      } else if (lowered.endsWith("ion")) {
        sb.append("-ion");
      } else if (lowered.endsWith("er")) {
        sb.append("-er");
      } else if (lowered.endsWith("est")) {
        sb.append("-est");
      } else if (lowered.endsWith("ly")) {
        sb.append("-ly");
      } else if (lowered.endsWith("ity")) {
        sb.append("-ity");
      } else if (lowered.endsWith("y")) {
        sb.append("-y");
      } else if (lowered.endsWith("al")) {
        sb.append("-al");
      }
    }
    sb.toString;
  }
  
  def main(args: Array[String]) {
    for (i <- 0 to 0) {
      println(i);
    }
    val col = new ArrayBuffer[String];
    col += "a" + "b";
    println(col);
  }
}
