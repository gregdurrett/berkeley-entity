package edu.berkeley.nlp.entity

import java.net.URL

import scala.IndexedSeq
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

import edu.berkeley.nlp.entity.coref.DocumentGraph
import edu.berkeley.nlp.entity.coref.Mention
import edu.berkeley.nlp.entity.preprocess.PreprocessingDriver
import edu.berkeley.nlp.futile.ling.CollinsHeadFinder
import edu.berkeley.nlp.futile.syntax.Trees.PennTreeRenderer
import edu.berkeley.nlp.futile.util.Logger
import edu.mit.jwi.Dictionary
import edu.mit.jwi.RAMDictionary
import edu.mit.jwi.data.ILoadPolicy
import edu.mit.jwi.item.IIndexWord
import edu.mit.jwi.item.ISynset
import edu.mit.jwi.item.IWord
import edu.mit.jwi.item.IWordID
import edu.mit.jwi.item.POS
import edu.mit.jwi.item.Pointer
import edu.mit.jwi.morph.WordnetStemmer

// VERY MUCH NOT SERIALIZABLE
class WordNetInterfacer(path: String) {
  val url = new URL("file", null, path);
  
//  val dict = new Dictionary(url);
//  dict.open();
  private val originalDict = new Dictionary(url);
  Logger.logss("Loading WordNet from " + path + " which has URL " + url);
  originalDict.open();
  private val dict = new RAMDictionary(originalDict, ILoadPolicy.IMMEDIATE_LOAD);
  dict.open();
  
  private val wns = new WordnetStemmer(dict);
  private val wnsDictLock = new Object();
  
  val entitySynset = getAllSynsets("entity").filter(_.getID.toString == "SID-00001740-N").head;
  
  val personSynset = getAllSynsets("person").filter(_.getID.toString == "SID-00007846-N").head;
  val locationSynset = getAllSynsets("location").filter(_.getID.toString == "SID-00027167-N").head;
  val organizationSynset = getAllSynsets("organization").filter(_.getID.toString == "SID-08008335-N").head;
  val eventSynset = getAllSynsets("event").filter(_.getID.toString == "SID-00029378-N").head;
  
  def getAllSynsets(unstemmedWord: String) = {
    getNounStemSet(unstemmedWord).flatMap(getAllWordSynsets(_));
  }
  
  def followSynsetRelationsAccumulate(synset: ISynset, relationPath: Seq[Pointer]): IndexedSeq[ISynset] = {
    var allSynsets = IndexedSeq(synset);
    var frontier = IndexedSeq(synset);
    for (rel <- relationPath) {
      wnsDictLock.synchronized {
        frontier = frontier.flatMap(_.getRelatedSynsets(rel).asScala.map(dict.getSynset(_)));
      }
      allSynsets ++= frontier;
    }
    allSynsets;
  }
  
  def followSynsetRelations(synset: ISynset, relationPath: Seq[Pointer]): IndexedSeq[ISynset] = {
    var currSynsets = IndexedSeq(synset);
    var newSynsets = IndexedSeq[ISynset]();
    for (rel <- relationPath) {
      wnsDictLock.synchronized {
        for (synset <- currSynsets) {
          newSynsets ++= synset.getRelatedSynsets(rel).asScala.map(dict.getSynset(_));
        }
      }
      currSynsets = newSynsets;
      newSynsets = IndexedSeq[ISynset]();
    }
    currSynsets;
  }
  
  def getDepth(synset: ISynset) {
    
  }
  
  def getWordsUpToSynsetRelation(unstemmedWord: String, relationPath: Seq[Pointer]) = {
    val words = getAllSynsets(unstemmedWord).flatMap(synset => followSynsetRelationsAccumulate(synset, relationPath)).flatMap(_.getWords().asScala);
    words.map(_.getLemma.toLowerCase);
  }
  
  def getWordsOnSynsetRelation(unstemmedWord: String, relationPath: Seq[Pointer]) = {
    val words = getAllSynsets(unstemmedWord).flatMap(synset => followSynsetRelations(synset, relationPath)).flatMap(_.getWords().asScala);
    words.map(_.getLemma.toLowerCase);
  }
  
  def getWordsUpToSynsetRelation(ment: Mention, relationPath: Seq[Pointer]) = {
    val words = getAllMentionSynsets(ment).flatMap(synset => followSynsetRelationsAccumulate(synset, relationPath)).flatMap(_.getWords().asScala);
    words.map(_.getLemma.toLowerCase);
  }
  
  def getWordsOnSynsetRelation(ment: Mention, relationPath: Seq[Pointer]) = {
    val words = getAllMentionSynsets(ment).flatMap(synset => followSynsetRelations(synset, relationPath)).flatMap(_.getWords().asScala);
    words.map(_.getLemma.toLowerCase);
  }
  
  /////////////////////////////////////
  
  
  def getLemmas(head: String): Set[String] = {
    getNounStemSet(head);
  }
  
  def isPluralApprox(head: String): Boolean = {
    val stems = wnsDictLock.synchronized { wns.findStems(head, POS.NOUN) };
    stems.size > 1 || (stems.size == 1 && stems.get(0) != head);
  }
  
  def shareLemma(head1: String, head2: String) = {
    getLemmas(head1).intersect(getLemmas(head2)).size > 0;
  }
  
  def isAnySynsetHypernym(head: String, possibleHypernymSynset: ISynset): Boolean = {
    isAnySynsetHypernym(head, possibleHypernymSynset, 10);
  }
  
  def isAnySynsetHypernym(head: String, possibleHypernymSynset: ISynset, maxLevel: Int): Boolean = {
    val stemsHead = getNounStemSet(head);
    var isHypernym = false;
    isAnySynsetHypernym(stemsHead.toSeq.flatMap(getAllWordSynsets(_)), possibleHypernymSynset, maxLevel);
  }
  
  def isAnySynsetHypernym(synsets: Seq[ISynset], possibleHypernymSynset: ISynset, maxLevel: Int): Boolean = {
    var isHypernym = false;
    for (headSynset <- synsets) {
      val hypernyms = getHypernyms(maxLevel, Set(headSynset));
      for (hypernym <- hypernyms) {
        if (hypernym == possibleHypernymSynset) {
          isHypernym = true;
        }
      }
    }
    isHypernym
  }
  
  private def getHypernyms(numLevelsToRecurse: Int, synsets: Set[ISynset]): HashSet[ISynset] = {
    var synsetsThisLevel = new HashSet[ISynset]() ++ synsets;
    var synsetsNextLevel = new HashSet[ISynset]();
    val allSynsets = new HashSet[ISynset]();
    for (i <- 0 until numLevelsToRecurse) {
      if (!synsetsThisLevel.isEmpty) {
        wnsDictLock.synchronized {
          for (synset <- synsetsThisLevel) {
            synsetsNextLevel ++= synset.getRelatedSynsets(Pointer.HYPERNYM).asScala.map(dict.getSynset(_));
          }
        }
        // Don't visit nodes we've already been to
        synsetsThisLevel = (synsetsNextLevel -- allSynsets);
        allSynsets ++= synsetsNextLevel;
        synsetsNextLevel = new HashSet[ISynset]();
      }
    }
    allSynsets;
  }
  
  def getWordSynset(stemmedWord: String) = {
    val idxWord: IIndexWord = wnsDictLock.synchronized { dict.getIndexWord(stemmedWord, POS.NOUN); }
    if (idxWord != null) {
      val wordID: IWordID = idxWord.getWordIDs().get(0);
      val word: IWord = dict.getWord(wordID);
      word.getSynset();
    } else {
      null;
    }
  }
  
  def getAllWordSynsets(stemmedWord: String): IndexedSeq[ISynset] = {
    val idxWord: IIndexWord = wnsDictLock.synchronized { dict.getIndexWord(stemmedWord, POS.NOUN); }
    if (idxWord != null) {
      wnsDictLock.synchronized { idxWord.getWordIDs().asScala.map(dict.getWord(_).getSynset()).toIndexedSeq; }
    } else {
      IndexedSeq[ISynset]();
    }
  }
  
  def getAllSelectedWordSynsets(stemmedWord: String, selectedList: IndexedSeq[Int]): IndexedSeq[ISynset] = {
    val idxWord: IIndexWord = wnsDictLock.synchronized { dict.getIndexWord(stemmedWord, POS.NOUN); }
    if (idxWord != null) {
      val synsets = wnsDictLock.synchronized { idxWord.getWordIDs().asScala.map(dict.getWord(_).getSynset()).toIndexedSeq; }
      selectedList.flatMap(i => if (i >= 0 && i < synsets.size) Seq(synsets(i)) else Seq());
    } else {
      IndexedSeq[ISynset]();
    }
  }
  
  def getAllMentionSynsets(ment: Mention): IndexedSeq[ISynset] = {
    getAllMentionSynsets(ment.words.slice(0, ment.headIdx - ment.startIdx + 1));
  }
  
  def getAllMentionSynsets(headAndPreModifiers: Seq[String]): IndexedSeq[ISynset] = {
    val stemmedHeadSet = getNounStemSet(WordNetInterfacer.sanitize(headAndPreModifiers.last));
    var resultSet = new ArrayBuffer[ISynset]();
    var wnItemLen = Math.min(headAndPreModifiers.size, 4);
    while (resultSet.isEmpty && wnItemLen >= 1) {
      for (possibleHead <- stemmedHeadSet) {
        val hypothesisList: Seq[String] = headAndPreModifiers.slice(headAndPreModifiers.size - wnItemLen, headAndPreModifiers.size - 1) ++ Seq(possibleHead);
        val hypothesis = hypothesisList.reduce(_ + "_" + _).toLowerCase;
        resultSet ++= getAllWordSynsets(hypothesis);
//        if (!resultSet.isEmpty && wnItemLen > 1) {
//          Logger.logss(">1 len found: " + hypothesis);
//        }
      }
      wnItemLen -= 1;
    }
    if (resultSet.isEmpty) {
      if (headAndPreModifiers.last == "Co." || headAndPreModifiers.last == "Corp." || headAndPreModifiers.last == "Inc." || headAndPreModifiers.last == "Ltd.") {
        resultSet ++= getAllWordSynsets("corporation");
      }
    }
    resultSet;
  }
  
  def getNounStemSet(head: String): Set[String] = {
    require(head != null && !head.isEmpty());
    var toReturn = Set[String]();
    try {
      toReturn = wnsDictLock.synchronized { wns.findStems(head, POS.NOUN).asScala.toSet; }
    } catch {
      case e: IllegalArgumentException => Logger.logss("IllegalArgumentException on " + head);
      case e: Throwable => throw new RuntimeException(e);
    }
    toReturn;
  }
}

object WordNetInterfacer {
  
  var wniCached: Option[WordNetInterfacer] = None;
  
  def getCachedInstance = {
    if (!wniCached.isDefined) {
      wniCached = Some(new WordNetInterfacer(Driver.wordNetPath));
    }
    wniCached.get;
  }
  
  def sanitize(str: String) = {
    var result = str;
    while (result.startsWith("_")) {
      result = result.substring(1);
    }
    while (result.endsWith("_")) {
      result = result.substring(0, result.size - 1);
    }
    if (result.contains("__")) {
      result = result.replaceAll("_+", "_");
    }
    if (result.isEmpty) {
      "badinputforwordnet";
    } else {
      result;
    }
  }
  
  def satisfiesWordNetSignal(docGraph: DocumentGraph, antIdx: Int, mentIdx: Int) = {
    val currHeadSanitized = WordNetInterfacer.sanitize(docGraph.getMention(mentIdx).headStringLc);
    val antecedentHeadSanitized = WordNetInterfacer.sanitize(docGraph.getMention(antIdx).headStringLc);
    docGraph.getWordNetRelsBetterCumulativeUseCache(docGraph.cachedWni, Seq(Pointer.HYPERNYM, Pointer.HYPERNYM, Pointer.HYPERNYM, Pointer.HYPERNYM, Pointer.HYPERNYM), antIdx).contains(currHeadSanitized) ||
          docGraph.getWordNetRelsBetterCumulativeUseCache(docGraph.cachedWni, Seq(Pointer.HYPERNYM, Pointer.HYPERNYM, Pointer.HYPERNYM, Pointer.HYPERNYM, Pointer.HYPERNYM), mentIdx).contains(antecedentHeadSanitized) ||
          docGraph.getWordNetRelsBetterCumulativeUseCache(docGraph.cachedWni, Seq(Pointer.HYPERNYM_INSTANCE, Pointer.HYPERNYM, Pointer.HYPERNYM), antIdx).contains(currHeadSanitized) ||
          docGraph.getWordNetRelsBetterCumulativeUseCache(docGraph.cachedWni, Seq(Pointer.HYPERNYM_INSTANCE, Pointer.HYPERNYM, Pointer.HYPERNYM), mentIdx).contains(antecedentHeadSanitized);
  }
  
  def generalTest() {
    val path = "/Users/gdurrett/Documents/Berkeley/Utils/WNdb-3.0/dict/";
    val url = new URL("file", null, path);
    
    val dict = new Dictionary(url);
    dict.open();
    val idxWord: IIndexWord = dict.getIndexWord("dog", POS.NOUN);
    val wordID: IWordID = idxWord.getWordIDs().get(0);
    val word: IWord = dict.getWord(wordID);
    println("Id = " + wordID);
    println("Lemma = " + word.getLemma());
    println("Gloss = " + word.getSynset().getGloss());
    
    val synset: ISynset = word.getSynset();
    // iterate over words associated with the synset
    println("Synonyms");
    synset.getWords().asScala.foreach(word => println(word.getLemma()))
    
    val hypernyms = synset.getRelatedSynsets(Pointer.HYPERNYM);
    println("Hypernyms");
    for(sid <- hypernyms.asScala){
      println(sid + ": " + dict.getSynset(sid).getWords().asScala.map(_.getLemma()));
    }
    
    val wns = new WordnetStemmer(dict);
    println(wns.findStems("dogs", POS.NOUN));
    println(wns.findStems("DOGS", POS.NOUN));
    println(wns.findStems("Presidents", POS.NOUN));
    
    
    println("===============");
    val wordNetInterfacer = new WordNetInterfacer(path);
    println(wordNetInterfacer.getLemmas("dog"));

  }
  
  def stateTest() {
    val path = "/Users/gdurrett/Documents/Berkeley/Utils/WNdb-3.0/dict/";
    val url = new URL("file", null, path);
    
    val dict = new Dictionary(url);
    dict.open();
    
    val idxWord = dict.getIndexWord("state", POS.NOUN);
    for (wordID <- idxWord.getWordIDs().asScala) {
      val word: IWord = dict.getWord(wordID);
      println("Id = " + wordID);
      println("Lemma = " + word.getLemma());
      println("Gloss = " + word.getSynset().getGloss());
      val synset: ISynset = word.getSynset();
      // iterate over words associated with the synset
      println("Synonyms");
      synset.getWords().asScala.foreach(word => println(word.getLemma()))
    }
  }
  
  def stemmerTest() {
    
    val path = "/Users/gdurrett/Documents/Berkeley/Utils/WNdb-3.0/dict/";
    val wni = new WordNetInterfacer(path);
    println(wni.wns.findStems("woman", POS.NOUN))
    println(wni.wns.findStems("women", POS.NOUN))
    println(wni.wns.findStems("girl", POS.NOUN))
    println(wni.wns.findStems("girls", POS.NOUN))
    println(wni.wns.findStems("country", POS.NOUN))
    println(wni.wns.findStems("countries", POS.NOUN))
    println(wni.wns.findStems("order", POS.NOUN))
    println(wni.wns.findStems("orders", POS.NOUN))
    println(wni.wns.findStems("plant", POS.NOUN))
    println(wni.wns.findStems("plants", POS.NOUN))
    println(wni.wns.findStems("number", POS.NOUN))
    println(wni.wns.findStems("numbers", POS.NOUN))
    println(wni.wns.findStems("meeting", POS.NOUN))
    println(wni.wns.findStems("meetings", POS.NOUN))
    println(wni.wns.findStems("session", POS.NOUN))
    println(wni.wns.findStems("sessions", POS.NOUN))
  }
  
  def instanceTest() {
    
    val wni = new WordNetInterfacer("/Users/gdurrett/Documents/Berkeley/Utils/WNdb-3.0/dict/");
    
//    val stemSet = wni.getNounStemSet("iraq");
//    for (stem <- stemSet) {
//      println("Stem: " + stem);
//      for (wordSynset <- wni.getAllWordSynsets(stem)) {
//        println("  Synset: " + wordSynset.getWords());
//        for (relatedSynset <- wni.followSynsetRelations(wordSynset, Seq(Pointer.HYPERNYM_INSTANCE))) {
//          println("  HI Related synset: " + relatedSynset.getWords());
//        }
//        for (relatedSynset <- wni.followSynsetRelations(wordSynset, Seq(Pointer.HYPERNYM_INSTANCE, Pointer.HYPERNYM))) {
//          println("  HI->H Related synset: " + relatedSynset.getWords());
//        }
//      }
//    }
//    wni.getWordsOnSynsetRelation("iraq", Seq(Pointer.HYPERNYM_INSTANCE, Pointer.HYPERNYM))
//    println(wni.getWordsUpToSynsetRelation("allah", Seq(Pointer.HYPERNYM_INSTANCE, Pointer.HYPERNYM)))
//    println(wni.getWordsUpToSynsetRelation("region", Seq()))
//    println(wni.getWordsUpToSynsetRelation("manufacturers", Seq(Pointer.HYPERNYM)))
    println(wni.getWordsUpToSynsetRelation("asian_nation", Seq(Pointer.HYPERNYM)))
  }
  
  def sanitizeTest() {
    println(sanitize("_way___messedup_"));
  }
  
  def fancySynsetTest() {
    val wni = new WordNetInterfacer("/Users/gdurrett/Documents/Berkeley/Utils/WNdb-3.0/dict/");
    val synsets = wni.getAllMentionSynsets(Seq("Kingdom", "of", "Saudi", "Arabia"));
    for (synset <- synsets) {
      println(synset.getWords().asScala.map(_.getLemma));
    }
  }
  
//  def depthTest() {
//    val wni = new WordNetInterfacer("/Users/gdurrett/Documents/Berkeley/Utils/WNdb-3.0/dict/");
//    var currSynsets = new HashSet[ISynset]() ++ wni.getAllMentionSynsets(Seq("corporation"));
//    var nextSynsets = new HashSet[ISynset]();
//    while (!currSynsets.isEmpty) {
//      for (synset <- currSynsets) {
//        for (parent <- wni.followSynsetRelations(synset, Seq(Pointer.HYPERNYM))) {
//          nextSynsets += parent;
//        }
//      }
//      currSynsets = nextSynsets;
//      nextSynsets = new HashSet[ISynset]();
//    }
//    
//  }
  
  def findSynsetsTest() = {
    val wni = new WordNetInterfacer("/Users/gdurrett/Documents/Berkeley/Utils/WNdb-3.0/dict/");
//    wni.getAllWordSynsets("event").foreach(synset => println(synset.getID() + ": " + synset.getGloss()));
    wni.getAllWordSynsets("entity").foreach(synset => println(synset.getID() + ": " + synset.getGloss()));
  }
  
  def main(args: Array[String]) = {
    findSynsetsTest();
    
    
  }
}
