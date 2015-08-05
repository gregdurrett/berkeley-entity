package edu.berkeley.nlp.entity.coref

import edu.berkeley.nlp.entity.joint.LikelihoodAndGradientComputer

// TODO: Modify GeneralTrainer to match the new one
class MentionRankingComputer extends LikelihoodAndGradientComputer[(DocumentGraph,Int)] {

  def addUnregularizedStochasticGradient(ex: (DocumentGraph,Int), weights: Array[Float], gradient: Array[Float]) = {
    
  }
  
  def computeLogLikelihood(ex: (DocumentGraph,Int), weights: Array[Float]): Float = {
    0.0F
  }
}