package edu.berkeley.nlp.entity.coref
import scala.collection.mutable.HashMap

object PronounDictionary {
  val firstPersonPronouns = Set("i", "me", "myself", "mine", "my", "we", "us", "ourself", "ourselves", "ours", "our");
  val secondPersonPronouns = Set("you", "yourself", "yours", "your", "yourselves");
  val thirdPersonPronouns = Set("he", "him", "himself", "his", "she", "her", "herself", "hers", "her", "it", "itself", "its", "one", "oneself", "one's", "they", "them", "themself", "themselves", "theirs", "their", "they", "them", "'em", "themselves");
  val otherPronouns = Set("who", "whom", "whose", "where", "when","which");
  
  val demonstratives = Set("this", "that", "these", "those");
  
  // Borrowed from Stanford
  val singularPronouns = Set("i", "me", "myself", "mine", "my", "yourself", "he", "him", "himself", "his", "she", "her", "herself", "hers", "her", "it", "itself", "its", "one", "oneself", "one's");
  val pluralPronouns = Set("we", "us", "ourself", "ourselves", "ours", "our", "yourself", "yourselves", "they", "them", "themself", "themselves", "theirs", "their");
  val malePronouns = Set("he", "him", "himself", "his");
  val femalePronouns = Set("her", "hers", "herself", "she");
  val neutralPronouns = Set("it", "its", "itself", "where", "here", "there", "which");
  
  
  val allPronouns = firstPersonPronouns ++ secondPersonPronouns ++ thirdPersonPronouns ++ otherPronouns;

  // Constructed based on Stanford's Dictionaries class
  val canonicalizations = new HashMap[String,String]();
  canonicalizations.put("i", "i");
  canonicalizations.put("me", "i");
  canonicalizations.put("my", "i");
  canonicalizations.put("myself", "i");
  canonicalizations.put("mine", "i");
  canonicalizations.put("you", "you");
  canonicalizations.put("your", "you");
  canonicalizations.put("yourself", "you");
  canonicalizations.put("yourselves", "you");
  canonicalizations.put("yours", "you");
  canonicalizations.put("he", "he");
  canonicalizations.put("him", "he");
  canonicalizations.put("his", "he");
  canonicalizations.put("himself", "he");
  canonicalizations.put("she", "she");
  canonicalizations.put("her", "she");
  canonicalizations.put("herself", "she");
  canonicalizations.put("hers", "she");
  
  canonicalizations.put("we", "we");
  canonicalizations.put("us", "we");
  canonicalizations.put("our", "we");
  canonicalizations.put("ourself", "we");
  canonicalizations.put("ourselves", "we");
  canonicalizations.put("ours", "we");
  canonicalizations.put("they", "they");
  canonicalizations.put("them", "they");
  canonicalizations.put("their", "they");
  canonicalizations.put("themself", "they");
  canonicalizations.put("themselves", "they");
  canonicalizations.put("theirs", "they");
  canonicalizations.put("'em", "they");
  canonicalizations.put("it", "it");
  canonicalizations.put("itself", "it");
  canonicalizations.put("its", "it");
  canonicalizations.put("one", "one");
  canonicalizations.put("oneself", "one");
  canonicalizations.put("one's", "one");
  
  canonicalizations.put("this", "this");
  canonicalizations.put("that", "that");
  canonicalizations.put("these", "these");
  canonicalizations.put("those", "those");
  canonicalizations.put("which", "which");
  canonicalizations.put("who", "who");
  canonicalizations.put("whom", "who");
//  canonicalizations.put("where", "where");
//  canonicalizations.put("whose", "whose");
  // This entry is here just to make results consistent with earlier ones
  // on our very small dev set
  canonicalizations.put("thy", "thy");
  canonicalizations.put("y'all", "you");
  canonicalizations.put("you're", "you");
  canonicalizations.put("you'll", "you");
  canonicalizations.put("'s", "'s");
  
  def isPronLc(str: String): Boolean = {
    !mightBeAcronym(str) && allPronouns.contains(str.toLowerCase());
  }
  
  def isDemonstrative(str: String): Boolean = {
    !mightBeAcronym(str) && demonstratives.contains(str.toLowerCase());
  }
  
  def mightBeAcronym(str: String) = {
    if (str.size <= 4) {
      var acronym = true;
      var i = 0;
      while (acronym && i < str.size) {
        if (!Character.isUpperCase(str.charAt(i))) {
          acronym = false;
        }
        i += 1;
      }
      acronym;
    } else {
      false;
    }
  }
  
  def canonicalize(str: String): String = {
    if (!canonicalizations.contains(str.toLowerCase())) {
      "";
    } else {
      canonicalizations(str.toLowerCase());
    }
  }
  
  def main(args: Array[String]) {
    println(PronounDictionary.canonicalizations("'em"));
    println(PronounDictionary.isPronLc("them"));
    println(PronounDictionary.isPronLc("Them"));
    println(PronounDictionary.isPronLc("NotThem"));
    println(PronounDictionary.mightBeAcronym("them"));
    println(PronounDictionary.mightBeAcronym("Them"));
    println(PronounDictionary.mightBeAcronym("THEM"));
  }
}
