package edu.berkeley.nlp.entity

import java.io.File

import edu.berkeley.nlp.entity.lang.Language

import scala.collection.mutable.ArrayBuffer

/**
 * Created by matthew on 2/18/15.
 */
class WikiDocReader (lang : Language, better : String) {} // TODO: remove

object WikiDocReader {
  def loadRawWikiDocs(path : String, size : Int, suffix : String, lang : Language = Language.ENGLISH, betterParsesFile : String = "") : Seq[Document] = {

    var docs = GUtil.load(path).asInstanceOf[List[WikiDoc]]

    if(size != -1 && docs.size > size)
      docs.map(_.asInstanceOf[Document]).slice(0, size).toSeq
    else
      docs.map(_.asInstanceOf[Document]).toSeq
  }
}