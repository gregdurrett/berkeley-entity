package edu.berkeley.nlp.entity.preprocess

import edu.berkeley.nlp.futile.LightRunner
import edu.berkeley.nlp.entity.ConllDoc
import edu.berkeley.nlp.entity.ConllDocReader
import java.io.File
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.entity.ConllDocWriter
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.futile.util.Logger

/**
 * When given a file as input and a file as output, reads in CoNLL documents from the
 * file and writes each document to its own file in the directory. 
 * N.B. Currently only works on CoNLL docs without standoff entity link annotations
 */
object ConllDocSharder {

  val inputFile = ""
  val outputDirectory = ""
  
  def main(args: Array[String]) {
    LightRunner.populateScala(ConllDocSharder.getClass(), args)
    require(!inputFile.isEmpty && !outputDirectory.isEmpty)
    var tokens = 0
    new ConllDocReader(Language.ENGLISH).readConllDocsProcessStreaming(inputFile, (doc: ConllDoc) => {
      val outputName = outputDirectory + "/" + doc.docID
      val writer = IOUtils.openOutHard(outputName)
      tokens += doc.words.map(_.size).foldLeft(0)(_ + _)
      ConllDocWriter.writeDoc(writer, doc)
      writer.close
    })
    Logger.logss("Wrote " + tokens + " tokens")
  }
}