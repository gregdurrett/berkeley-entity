package edu.berkeley.nlp.entity

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.futile.util.Logger
import java.io.PrintWriter
import edu.berkeley.nlp.entity.coref.Mention

package object wiki {
  
  type SentAnnots[T] = ArrayBuffer[Chunk[T]];
  type DocAnnots[T] = HashMap[Int,SentAnnots[T]];
  type CorpusAnnots[T] = HashMap[String,DocAnnots[T]];
  
  type SentWikiAnnots = ArrayBuffer[Chunk[Seq[String]]];
  type DocWikiAnnots = HashMap[Int,SentWikiAnnots];
  type CorpusWikiAnnots = HashMap[String,DocWikiAnnots];
  
  val WikiUrl = "http://en.wikipedia.org/wiki/"
  val NilToken = "-NIL-";
  val ExcludeToken = "-EXCLUDE-";
  
  def maybeLc(str: String, lowercase: Boolean) = if (lowercase) str.toLowerCase else str;
  
  def corpusAnnotsSize[T](annots: CorpusAnnots[T]) = annots.map(_._2.map(_._2.size).reduce(_ + _)).reduce(_ + _)
  
  def getGoldWikification(goldWiki: DocWikiAnnots, ment: Mention): Seq[String] = {
    if (!goldWiki.contains(ment.sentIdx)) {
      Seq[String]();
    } else {
      val matchingChunks = goldWiki(ment.sentIdx).filter(chunk => chunk.start == ment.startIdx && chunk.end == ment.endIdx);
      if (matchingChunks.isEmpty) Seq[String]() else matchingChunks(0).label;
    }
  }
  
  def isCorrect(gold: Seq[String], guess: String): Boolean = {
//    (gold.isEmpty && guess == NilToken) ||
    (gold.map(_.toLowerCase).contains(guess.toLowerCase.replace(" ", "_"))); // handles the -NIL- case too
  }
  
  def containsCorrect(gold: Seq[String], guesses: Iterable[String]): Boolean = {
    guesses.foldLeft(false)(_ || isCorrect(gold, _));
  }
  
  def accessACEHead(aceHeads: HashMap[String,HashMap[Int,Seq[Chunk[Int]]]], docName: String, sentIdx: Int, startIdx: Int, endIdx: Int) = {
    if (aceHeads.contains(docName) && aceHeads(docName).contains(sentIdx)) {
      val possibleMatchingChunk = aceHeads(docName)(sentIdx).filter(chunk => chunk.start == startIdx && chunk.end == endIdx);
      if (possibleMatchingChunk.size >= 1) {
        possibleMatchingChunk.head.label;
      } else {
        -1;
      }
    } else {
      -1;
    }
  }
  
  def extractAnnotation[T](annots: CorpusAnnots[T], docName: String, sentIdx: Int, startIdx: Int, endIdx: Int): Option[T] = {
    if (annots.contains(docName) && annots(docName).contains(sentIdx)) {
      extractChunkLabel(annots(docName)(sentIdx), startIdx, endIdx);
    } else {
      None;
    }
  }
  
  def extractChunkLabel[T](chunks: Seq[Chunk[T]], startIdx: Int, endIdx: Int): Option[T] = {
    val maybeResult = chunks.filter(chunk => chunk.start == startIdx && chunk.end == endIdx);
    if (maybeResult.size != 1) None else Some(maybeResult.head.label);
  }
  
  def wikiCase(str: String) = {
    require(str != null);
    if (str.size == 0) "" else Character.toUpperCase(str.charAt(0)) + str.substring(1)
  }
}
