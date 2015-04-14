package edu.berkeley.nlp.entity.wiki

import edu.berkeley.nlp.futile.LightRunner

/**
 * Created by matthewfl
 *
 * We want to work with the who document at a time rather then just a single link
 * this will allow us to
 */
class DocumentedSetChooser {

}


object DocumentedSetChooser {

  val trainDataPath = "data/ace05/train";
  val testDataPath = "data/ace05/dev";
  val wikiPath = "data/ace05/ace05-all-conll-wiki" // contains the wiki links for both items
  val wikiDBPath = "models/wiki-db-ace.ser.gz"

  val lambda = 1e-8F
  val batchSize = 1
  val numItrs = 20

  val maxNumWikificationOptions = 20 //7

  val numLoadedSamples = -1 // for debugging by loading less samples


  def main(args: Array[String]) = {
    LightRunner.initializeOutput(DocumentedSetChooser.getClass)
    LightRunner.populateScala(DocumentedSetChooser.getClass, args)

    // load the documents



    LightRunner.finalizeOutput()
  }
}