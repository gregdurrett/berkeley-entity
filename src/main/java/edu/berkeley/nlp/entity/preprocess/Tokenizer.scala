package edu.berkeley.nlp.entity.preprocess

import edu.berkeley.nlp.futile.tokenizer.PTBLineLexer
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.futile.util.Logger

trait Tokenizer {
  def tokenize(sentence: String): Array[String];
}

case class StandardPTBTokenizer() extends Tokenizer {
  val tokenizer = new PTBLineLexer();
  
  def tokenize(sentence: String) = {
    tokenizer.tokenize(sentence).toArray(new Array[String](0));
  }
}

/**
 * Taken from
 * http://www.cis.upenn.edu/~treebank/tokenizer.sed
 * 
 * Main things this doesn't do
 * --Hyphens aren't split out
 * --Respects the sentence boundary detector which fails sometimes
 */
case class CustomPTBTokenizer() extends Tokenizer {
  
  def tokenize(sentence: String) = {
    // Beginning and end are padded with a space to make matching simpler
    var currSentence = " " + sentence.trim() + " ";
    // Fix quotes
    currSentence = currSentence.replace(" \"", " `` ");
    currSentence = currSentence.replace("\"", "''");
    // Do this before periods
    currSentence = currSentence.replace("...", " ... ");
    // Split out final periods, possibly followed by ', ", ), ], }
//    currSentence = currSentence.replaceAll(".(['\"\\)}\\]]) ", " . $1 ");
    currSentence = currSentence.replaceAll("\\.(['\"\\)}\\]]|(''))? $", " . $1 ");
    // Break out quotes
    currSentence = currSentence.replaceAll("''", " '' ");
    // Dashes
    currSentence = currSentence.replace("--", " -- ");
    // Split out punctuation, brackets, etc.
    // Exception: keep commas in numbers
    currSentence = currSentence.replaceAll("(\\d),(\\d)", "$1COMMAMARKER$2")
    for (symbol <- Tokenizer.AllSymbols) {
      currSentence = currSentence.replace(symbol, " " + symbol + " ");
    }
    currSentence = currSentence.replace("COMMAMARKER", ",");
    // Fix brackets, etc.
    for (entry <- Tokenizer.ReplacementMap) {
      currSentence = currSentence.replace(entry._1, entry._2);
    }
    // Split out suffixes
    for (entry <- Tokenizer.SuffixesMap) {
      currSentence = currSentence.replace(entry._1, entry._2);
    }
    // Seems like the tokenizer doesn't do this
//    for (entry <- Tokenizer.CompoundsMap) {
//      currSentence = currSentence.replace(entry._1, entry._2);
//    }
    currSentence = currSentence.replaceAll("([^'])' ", "$1 ' ");
    currSentence = currSentence.replaceAll(" '([^'\\s])", " ' $1");
    currSentence = currSentence.replaceAll("([^\\s])'([sSmMdD])", "$1 '$2 ");
    currSentence.trim.split("\\s+");
  }
}

object Tokenizer {
  val PuncSymbols = Set("?", "!", ",", ";", ":", "@", "#", "$", "%", "&");
  val BracketSymbols = Set("(", ")", "[", "]", "{", "}");
  val AllSymbols = PuncSymbols ++ BracketSymbols;
  
  val ReplacementMap = HashMap("(" -> "-LRB-",
                               ")" -> "-RRB-",
                               "[" -> "-LSB-",
                               "]" -> "-RSB-",
                               "{" -> "-LCB-",
                               "}" -> "-RCB-");
  
  val SuffixesMap = HashMap("'ll " -> " 'll ",
                            "'re " -> " 're ",
                            "'ve " -> " 've ",
                            "n't " -> " n't ");
  SuffixesMap ++= SuffixesMap.map(entry => entry._1.toUpperCase -> entry._2.toUpperCase);
  
  val CompoundsMap = HashMap(" Cannot " -> " Can not ",
                             " D'ye " -> " D' ye ",
                             " Gimme " -> " Gim me ",
                             " Gonna " -> " Gon na ",
                             " Gotta " -> " Got ta ",
                             " Lemme " -> " Lem me ",
                             " More'n " -> " More 'n ",
                             " 'Tis " -> " 'T is ",
                             " 'Twas " -> " 'T was ",
                             " Wanna " -> " Wan na ");
  CompoundsMap ++= CompoundsMap.map(entry => entry._1.toLowerCase -> entry._2.toLowerCase);
}
