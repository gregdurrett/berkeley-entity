package edu.berkeley.nlp.entity.joint

import edu.berkeley.nlp.entity.wiki._
import edu.berkeley.nlp.entity.Driver

object JointLossFcns {

  val nerLossFcn = (gold: String, pred: String) => if (gold == pred) 0.0F else Driver.nerLossScale.toFloat;
  
  val noNerLossFcn = (gold: String, pred: String) => 0.0F
  
  val wikiLossFcn = (gold: Seq[String], pred: String) => {
    if (gold.contains(NilToken) && pred != NilToken) {
      Driver.wikiNilKbLoss.toFloat
    } else if (!gold.contains(NilToken) && pred == NilToken){
      Driver.wikiKbNilLoss.toFloat
    } else if (!isCorrect(gold, pred)) {
      Driver.wikiKbKbLoss.toFloat
    } else {
      0.0F;
    }
  }
  
  val noWikiLossFcn = (gold: Seq[String], pred: String) => 0.0F;
}
