package edu.berkeley.nlp.entity.coref
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.futile.util.Logger
import scala.collection.mutable.HashMap
import scala.util.MurmurHash
import edu.berkeley.nlp.futile.fig.basic.SysInfoUtils
import scala.collection.mutable.ArrayBuffer

class NumberGenderComputer(val wordIndex: Indexer[String],
                           val numGendMap: HashMap[BergsmaLinKey,Array[Int]]) {

  private def sanitizeWord(word: String) = {
    word.toLowerCase.replaceAll("\\d+", "#");
  }
  
  def accessCounts(words: Seq[String]) = {
    val convertedWords = words.map(sanitizeWord(_));
    val blKey = new BergsmaLinKey(convertedWords.map(wordIndex.getIndex(_)).toArray);
    if (!numGendMap.contains(blKey)) {
      NumberGenderComputer.NullCounts;
    } else {
      numGendMap(blKey);
    }
  }
  
  def computeNumber(words: Seq[String], head: String): Number = {
    // Try the main word and then back off to the head
    var counts = accessCounts(words);
    if (counts.sameElements(NumberGenderComputer.NullCounts)) {
      counts = accessCounts(Seq(head));
    }
    // Default to SINGULAR
    if (counts(0) + counts(1) + counts(2) >= counts(3)) {
      Number.SINGULAR;
    } else {
      Number.PLURAL;
    }
  }
  
  def computeGenderNonPerson(words: Seq[String], head: String): Gender = {
    var counts = accessCounts(words);
    if (counts.sameElements(NumberGenderComputer.NullCounts)) {
      counts = accessCounts(Seq(head));
    }
    // Require some confidence to decide (taken from the Stanford system)
    if (counts(0) >= 2*(counts(1)+counts(2)) && counts(0) >= 3) {
      Gender.MALE
    } else if (counts(1) >= 2*(counts(0)+counts(2)) && counts(1) >= 3) {
      Gender.FEMALE;
    } else if (counts(2) >= 2*(counts(0)+counts(1)) && counts(2) >= 3) {
      Gender.NEUTRAL;
    } else {
      Gender.UNKNOWN;
    }
  }
  
  /**
   * Special case for people: rather than use the head, try to find a first
   * name and decide based on that.
   */
  def computeGenderPerson(words: Seq[String], headIdxRelativeToWords: Int): Gender = {
    // If the head is upper case, assume it's a last name
    if (Character.isUpperCase(words(headIdxRelativeToWords).charAt(0))) {
      // If the word before the head is upper case, assume it's a first name, and decide
      // based on that
      if (headIdxRelativeToWords > 0 && Character.isUpperCase(words(headIdxRelativeToWords-1).charAt(0))) {
        val firstNameLc = words(headIdxRelativeToWords-1).toLowerCase();
        computeGenderNonPerson(Seq(firstNameLc), firstNameLc);
      } else {
        computeGenderNonPerson(words, words(headIdxRelativeToWords));
      }
    } else {
      computeGenderNonPerson(words, words(headIdxRelativeToWords));
    }
  }
}

/**
 * Thin wrapper around Array[Int] to implement equality and hash code
 */
case class BergsmaLinKey(val entries: Array[Int]) {
  
  override def equals(other: Any) = {
    other match {
      case that: BergsmaLinKey => entries.sameElements(that.entries);
      case _ => false;
    }
  }
  
  override def hashCode() = {
    MurmurHash.arrayHash(entries);
  }
}

object NumberGenderComputer {
  
  val ElisionCharacter = "!";
  val NumberCharacter = "#";
  val NullCounts = Array(0, 0, 0, 0);
  val zeroCharVal = '0'.toInt;
  
  /**
   * Fairly optimized reader for the Bergsma and Lin data. I kind of went overboard
   * with this, but it should load in about 6 seconds instead of 10, which is nice
   * because it loads every time the system runs.
   */
  def readBergsmaLinData(fileName: String): NumberGenderComputer = {
    Logger.logss("Reading Bergsma and Lin file from " + fileName);
    val index = new Indexer[String];
    val numGendMap = new HashMap[BergsmaLinKey, Array[Int]]();
    val reader = IOUtils.openInHard(fileName);
    while (reader.ready()) {
      val line = reader.readLine();
      // Read the words and values, awkward and non-Scala-y to be
      // faster.
      var tokenEnd = findTokenEnd(line, line.size - 1);
      var tokenStart = findTokenStart(line, tokenEnd)
      // Read the four numbers at the end of the string
      val vals = new Array[Int](4);
      var i = 3;
      while (i >= 0) {
        vals(i) = readInt(line, tokenStart, tokenEnd);
        tokenEnd = findTokenEnd(line, tokenStart - 1);
        tokenStart = findTokenStart(line, tokenEnd);
        i -= 1;
      }
      // Count the number of words so we can initialize the array with the correct size
      val oldTokenEnd = tokenEnd;
      val oldTokenStart = tokenStart;
      var numWords = 0;
      while (tokenStart > 0) {
        numWords += 1;
        tokenEnd = findTokenEnd(line, tokenStart - 1);
        tokenStart = findTokenStart(line, tokenEnd);
      }
      // Add one for the word starting at 0
      numWords += 1;
      // Now go back and actually read the words themselves
      val words = new Array[Int](numWords);
      tokenEnd = oldTokenEnd;
      tokenStart = oldTokenStart;
      var idx = numWords - 1;
      while (tokenStart > 0) {
        words(idx) = index.getIndex(line.substring(tokenStart, tokenEnd + 1));
        idx -= 1;
        tokenEnd = findTokenEnd(line, tokenStart - 1);
        tokenStart = findTokenStart(line, tokenEnd);
      }
      // Read the last word
      words(0) = index.getIndex(line.substring(tokenStart, tokenEnd + 1));
      
      // N.B. You can replace everything above with these 12 lines for only
      // about a 30% slowdown (it's slower primarily because of the tokenization
      // and duplicating the string when you do that).
//      val tokenized = line.split("\\s+");
//      val words = new Array[Int](tokenized.size - 4);
//      var i = 0;
//      while (i < tokenized.size - 4) {
//        words(i) = index.getIndex(tokenized(i));
//        i += 1;
//      }
//      val vals = new Array[Int](4);
//      vals(0) = tokenized(tokenized.size - 4).toInt;
//      vals(1) = tokenized(tokenized.size - 3).toInt;
//      vals(2) = tokenized(tokenized.size - 2).toInt;
//      vals(3) = tokenized(tokenized.size - 1).toInt;
      
      
//      Logger.logss(line + ": " + words.map(index.getObject(_)).toSeq + " " + vals.toSeq);
      val blKey = new BergsmaLinKey(words);
      // Duplicate entry (quite rare): just add the counts (usually one has much lower count anyway)
      if (numGendMap.contains(blKey)) {
        val currCounts = numGendMap(blKey);
        numGendMap.put(blKey, (0 until vals.size).map(i => vals(i) + currCounts(i)).toArray);
      } else {
        numGendMap.put(blKey, vals);
      }
    }
    reader.close();
    Logger.logss("Done!");
    new NumberGenderComputer(index, numGendMap);
  }
  
  /**
   * Returns the index of the first character in str back from idx that is not
   * whitespace 
   */
  private def findTokenEnd(str: String, idx: Int) = {
    var endIdx = idx;
    while (endIdx >= 0 && Character.isWhitespace(str.charAt(endIdx))) {
      endIdx -= 1;
    }
    endIdx;
  }
  
  /**
   * Walks back in str from tokenEnd and returns the last character before
   * you hit whitespace. 
   */
  private def findTokenStart(str: String, tokenEnd: Int) = {
    var startIdx = tokenEnd;
    while (startIdx >= 0 && !Character.isWhitespace(str.charAt(startIdx))) {
      startIdx -= 1;
    }
    startIdx + 1;
  }
  
  /**
   * Token end is *INCLUSIVE*
   */
  private def readInt(str: String, tokenStart: Int, tokenEnd: Int) = {
    var num = 0;
    var idx = tokenStart;
    while (idx <= tokenEnd) {
      num = (num * 10) + (str.charAt(idx).toInt - zeroCharVal);
      idx += 1;
    }
    num;
  }
  
  def main(args: Array[String]) {
    val time = System.nanoTime();
    val ngComputer = readBergsmaLinData("data/gender.data");
    Logger.logss("Time: " + (System.nanoTime() - time)/1000000);
    Logger.logss(ngComputer.accessCounts(Seq("things")));
    Logger.logss(ngComputer.accessCounts(Seq("jack"))); // Counts: WrappedArray(5165, 158, 377, 120)
    Logger.logss(ngComputer.accessCounts(Seq("rose"))); // Counts: WrappedArray(21161, 1292, 1910, 524)
    Logger.logss(ngComputer.accessCounts(Seq("lucy"))); // Counts: WrappedArray(119, 684, 25, 18)
    Logger.logss(ngComputer.accessCounts(Seq("walkers", "sensations"))); // Counts: WrappedArray(0, 0, 1, 0)
    // Not present
    Logger.logss(ngComputer.accessCounts(Seq("sensations", "walkers"))); // Counts: WrappedArray(0, 0, 0, 0)
    // Test sanitizing word, w# is what's in the dictionary
    Logger.logss(ngComputer.accessCounts(Seq("w0312"))); // Counts: WrappedArray(1, 0, 38, 1)
  }
}
