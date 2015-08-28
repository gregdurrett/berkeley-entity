package edu.berkeley.nlp.entity.coref

import edu.berkeley.nlp.futile.util.Logger

object ErrorAnalyzer {

  /**
   * Prints error analysis on a dev set. Predictions are passed as both predicted backpointers
   * as well as the final clusterings those backpointers yield when you take the transitive
   * closure. Finally, the scorer (featurizer + weights) are also provided in case you want
   * to recompute anything, look at how badly the gold scored, etc.
   * 
   */
  def analyzeErrors(docGraphs: Seq[DocumentGraph],
                    allPredBackptrs: Seq[Array[Int]],
                    allPredClusterings: Seq[OrderedClustering],
                    scorer: PairwiseScorer) {
    for (docIdx <- 0 until docGraphs.size) {
      val doc = docGraphs(docIdx)
      val clustering = allPredClusterings(docIdx)
      for (i <- 0 until doc.size) {
        val prediction = allPredBackptrs(docIdx)(i)
        val ment = doc.getMention(i)
        val isCorrect = doc.isGoldCurrentPruning(i, prediction)
        val goldLinks = doc.getGoldAntecedentsUnderCurrentPruning(i)
        if (!isCorrect) {
          Logger.logss(doc.corefDoc.rawDoc.uid)
          Logger.logss("  Error on mention " + i + " " + doc.getMention(i).spanToStringWithHeadAndContext(2))
          Logger.logss("       Prediction: " + prediction + " " + (if (prediction == i) "SELF" else  doc.getMention(prediction).spanToStringWithHeadAndContext(2)))
          if (goldLinks.size == 1 && goldLinks(0) == i) {
            Logger.logss("       Gold: singleton")
          } else {
            Logger.logss("       Gold: " + goldLinks.toSeq)
            Logger.logss("         First antecedent: " + doc.getMention(goldLinks(0)).spanToStringWithHeadAndContext(2))
            if (goldLinks.size > 1) {
              Logger.logss("         Most recent antecedent: " + doc.getMention(goldLinks.last).spanToStringWithHeadAndContext(2))
            }
          }
        }
      }
    }
  }
}