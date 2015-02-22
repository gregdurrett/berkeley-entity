package edu.berkeley.nlp.entity

import java.io.File

import edu.berkeley.nlp.entity.lang.{ModCollinsHeadFinder, Language}
import edu.berkeley.nlp.entity.preprocess.SentenceSplitter
import edu.berkeley.nlp.futile.syntax.Tree

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.xml._

/**
 * Created by matthew on 2/18/15.
 */
class WikiDocReader (val lang : Language, val betterParsesFile : String = "") {

  val betterParses = new HashMap[ArrayBuffer[String], Tree[String]]

  // TODO: betterParsesFile

  val headFinder = lang match {
    case Language.ENGLISH => new ModCollinsHeadFinder()
    case _ => throw new RuntimeException()
  }

  val sentenceSplitter = SentenceSplitter.loadSentenceSplitter("models/sentsplit.txt.gz")

  def readWikiDocs(fileName : String) : Seq[WikiDoc] = {
    val referencesFile = fileName.replace("RawTexts", "Problems");
    val refxml = XML.loadFile(referencesFile);
    val document = scala.io.Source.fromFile(fileName).mkString

    //val splits = sentenceSplitter.formCanonicalizedParagraphs(document.split(" "), false, false)
    val splits  = sentenceSplitter.splitSentences(document.split("\n").filter(!_.trim.isEmpty))



    for(reference <- refxml \ "ReferenceInstance") {
      val surfaceForm = (reference \ "SurfaceForm")(0).text.trim
      val offset = (reference \ "Offset")(0).text.trim.toInt
      val length = (reference \ "Length")(0).text.trim.toInt
      val chosenAnnotation = (reference \ "ChosenAnnotation")(0).text.trim
      val annotatorId = (reference \ "AnnotatorId")(0).text.trim
      val annotation = (reference \ "Annotation")(0).text.trim


    }

    // docID some unique identifier, filename
    // partNo some int cnt
    // words an array of sentences
    // trees set of parse trees for a given sentence entity.DepConstTree
    // nerchunks entity.Chunk


    Seq[WikiDoc]()
  }

}

object WikiDocReader {
  def loadRawWikiDocs(path : String, size : Int, suffix : String, lang : Language = Language.ENGLISH, betterParsesFile : String = "") : Seq[Document] = {
    val rawDir = new File(path)
    if (!rawDir.exists() || !rawDir.canRead() || rawDir.listFiles == null || rawDir.listFiles.isEmpty) {
      throw new RuntimeException("Couldn't find directory " + path);
    }
    var rawFiles = rawDir.listFiles.map(_.getAbsolutePath())
    //val files = rawFiles.filter(file => file.getAbsolutePath.endsWith(suffix));
    val reader = new WikiDocReader(lang, betterParsesFile)
    val docs = new ArrayBuffer[Document]
    for(fname <- rawFiles) {
      docs ++= reader.readWikiDocs(fname)
    }
    docs
  }
}