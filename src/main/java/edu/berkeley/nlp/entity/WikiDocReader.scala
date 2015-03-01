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

  def readWikiDocs(fileName : String) : Seq[WikiDoc] = {



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