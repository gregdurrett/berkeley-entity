//package edu.berkeley.nlp.entity.coref
//
//case class PruningStrategy(val strategy: String) {
//  
//  def getDistanceArgs(): (Int, Int) = {
//    require(strategy.startsWith("distance"));
//    (splitStrategy(1).toInt, splitStrategy(2).toInt);
//  }
//  
//  def getModelPath: String = PruningStrategy.getModelPath(strategy);
//  def getModelLogRatio: Float = PruningStrategy.getModelLogRatio(strategy);
//}
//
//object PruningStrategy {
//  
//  def buildPruner(strategy: String) {
//    if (strategy.startsWith("distance")) {
//      val splitStrategy = strategy.split(":");
//      val (maxSent(splitStrategy(1).toInt, splitStrategy(2).toInt);
//    } else if (strategy.startsWith("models")) {
//      
//    }
//  }
//
//  def getModelPath(strategy: String): String = {
//    require(strategy.startsWith("models"));
//    strategy.split(":")(1);
//  }
//  
//  def getModelLogRatio(strategy: String): Float = {
//    require(strategy.startsWith("models"));
//    strategy.split(":")(2).toFloat;
//  }
//}
