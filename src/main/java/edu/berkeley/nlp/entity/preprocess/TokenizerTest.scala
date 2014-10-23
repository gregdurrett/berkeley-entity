package edu.berkeley.nlp.entity.preprocess

import scala.collection.JavaConverters._
import edu.berkeley.nlp.futile.tokenizer.PTBLineLexer
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object TokenizerTest {

  
//  def main(args: Array[String]) {
//    val tokenizer = new PTBLineLexer();
//    val s1 = "In pp collisions at a centre-of-mass energy s=7 TeV the emergence of similar long-range (2<|Δη|<4) near-side (Δφ≈0) correlations was reported in events with significantly higher-than-average particle multiplicity [21].";
//    val s2 = "The coefficient v2p is significantly lower than v2π for 0.5<pT<1.5 GeV/c, and larger than v2π for pT>2.5 GeV/c."
//    println(tokenizer.tokenize(s1).asScala.reduce(_ + "::" + _));
//    println(tokenizer.tokenize(s2).asScala.reduce(_ + "::" + _));
//  }
//  
//  
  
  def main(args: Array[String]) = {
//    val str = "a.) ";
//    println(str.replaceAll("\\.(['\"\\)}\\]])? $", " . $1 "));
//    System.exit(0);
    
//    val str = "a.'' ";
//    println(str.replaceAll("\\.(['\"\\)}\\]]|(''))? $", " . $1 "));
//    System.exit(0);
    
    
    val tok1 = new CustomPTBTokenizer;
    // To test: possessives (plural and singular), quotes, quote + punc, all punc
//    println(tok.tokenize());
//    println(" D'ye ".replaceAll("D'ye", " D' ye"));
//    var currSentence = "Jesus' thing and I'm I'd";
//    currSentence = currSentence.replaceAll("([^'])' ", "$1 ' ");
//    currSentence = currSentence.replaceAll("'([sSmMdD])", " '$1 ");
//    var currSentence = "(((";
//    currSentence = currSentence.replace("(", "-LRB-");
//    
//    println("things.\" ".replaceAll(".(['\"\\)}\\]]) ", " . $1 "));
//    println("things.' ".replaceAll(".(['\"\\)}\\]]) ", " . $1 "));
//    println("things.+ ".replaceAll(".(['\"\\)}\\]]) ", " . $1 "));
    
    val tok2 = new PTBLineLexer;
    var count = 0;
    
    val splitter = SentenceSplitter.loadSentenceSplitter("models/sentsplit.txt.gz");
    val lines = Source.fromFile("data/ace-tokenization-test-2k.txt").getLines.toSeq;
    val paras = splitter.formCanonicalizedParagraphs(lines.toArray, false, true);
    val sentences = splitter.splitSentences(paras)
    println(paras.size + " paras, " + sentences.size + " sents");
//    val sent = "This is a test. No it's really a test. Like actually a test. Like what.";
//    println(splitter.splitSentences(Array(sent)).toSeq);
    for (sentence <- sentences) {
//      println(sentence);
      val tokenized1 = new ArrayBuffer[String] ++ tok1.tokenize(sentence);
      val tokenized2 = new ArrayBuffer[String] ++ tok2.tokenize(sentence).asScala;
      if (tokenized1.sameElements(tokenized2)) {
        count += 1;
      } else {
        println(tokenized1)
        println(tokenized2);
      }
    }
    println(count);
    
    
  }
}
