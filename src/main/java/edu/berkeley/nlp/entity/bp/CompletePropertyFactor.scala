//package edu.berkeley.nlp.entity.bp
//
//import edu.berkeley.nlp.entity.GUtil
//import edu.berkeley.nlp.futile.util.Logger
//
//class CompletePropertyFactor[T](val antecedentNode: Node[Int],
//                                val myPropertyNode: Node[T],
//                                val antPropertyNodes: Array[Node[T]],
//                                val indexedFeatureMatrix: Array[Array[Array[Array[Int]]]]) extends Factor {
//  // Indices in the feature matrix: antecedentIdx, current property value, antecedent property value, <features>
//  require(antPropertyNodes.size >= 1);
//  
//  var cachedWeights: Array[Float] = null;
//  antecedentNode.registerFactor(this);
//  myPropertyNode.registerFactor(this);
//  antPropertyNodes.foreach(_.registerFactor(this));
//  
//  var receivedAntecedentMessage: Array[Double] = null;
//  var receivedMyPropertyMessage: Array[Double] = null;
//  var receivedAntPropertyMessages: Array[Array[Double]] = Array.tabulate(antPropertyNodes.size)(i => null);
//  var sentAntecedentMessage: Array[Double] = new Array[Double](antecedentNode.domain.size);
//  var sentMyPropertyMessage: Array[Double] = new Array[Double](myPropertyNode.domain.size);
//  var sentAntPropertyMessages: Array[Array[Double]] = Array.tabulate(antPropertyNodes.size)(i => new Array[Double](antPropertyNodes(i).domain.size))
//  
//  // Some precomputed stuff
//  var antPropertySums: Array[Double] = Array.tabulate(antPropertyNodes.size)(i => 0.0);
//  var productOfAntPropertySums = 0.0;
//  var myPropertySum = 0.0;
//  
//  def setWeights(newWeights: Array[Float]) {
//    this.cachedWeights = newWeights;
//    for (i <- 0 until sentAntecedentMessage.length) {
//      sentAntecedentMessage(i) = 0;
//    }
//    for (i <- 0 until sentMyPropertyMessage.length) {
//      sentMyPropertyMessage(i) = 0;
//    }
//    for (nodeIdx <- 0 until sentAntPropertyMessages.size) {
//      for (i <- 0 until sentAntPropertyMessages(nodeIdx).size) {
//        sentAntPropertyMessages(nodeIdx)(i) = 0;
//      }
//    }
//  }
//  
//  def receiveMessage(node: Node[_], message: Array[Double]) {
//    require(!GUtil.containsNaN(message));
//    require(message.size == node.domain.size);
//    if (node == antecedentNode) {
//      receivedAntecedentMessage = message;
//    } else if (node == myPropertyNode) {
//      receivedMyPropertyMessage = message;
//    } else {
//      val idx = antPropertyNodes.indexOf(node);
//      if (idx == -1) {
//        throw new RuntimeException("Bad node in graph");
//      } else {
//        receivedAntPropertyMessages(idx) = message;
//      }
//    }
//  }
//  
//  def factorValue(antecedentValueIdx: Int, myPropertyValueIdx: Int, antPropertyValueIndices: Array[Int]): Double = {
//    if (antecedentValueIdx == antecedentNode.domain.size - 1) {
//      1.0;
//    } else {
//      factorValue(antecedentValueIdx, myPropertyValueIdx, antPropertyValueIndices(antecedentValueIdx));
//    }
//  }
//  
//  def factorValue(antecedentValueIdx: Int, myPropertyValueIdx: Int, antPropertyValueIdx: Int): Double = {
//    require(antecedentValueIdx < antecedentNode.domain.size - 1);
//    var featValue = 1.0;
//    var featIdx = 0;
//    while (featIdx < indexedFeatureMatrix(antecedentValueIdx)(myPropertyValueIdx)(antPropertyValueIdx).size) {
//      featValue *= Math.exp(cachedWeights(indexedFeatureMatrix(antecedentValueIdx)(myPropertyValueIdx)(antPropertyValueIdx)(featIdx)));
//      featIdx += 1;
//    }
//    featValue;
//  }
//  
//  
//  
//  def getAllAssociatedFeatures(): Array[String] = Array[String]();
//  
//  private def precomputeReceivedMessagesProductOfSums {
//    productOfAntPropertySums = 1.0
//    for (i <- 0 until antPropertyNodes.size) {
//      var j = 0;
//      antPropertySums(i) = 0.0;
//      while (j < receivedAntPropertyMessages(i).size) {
//        antPropertySums(i) += receivedAntPropertyMessages(i)(j);
//        j += 1;
//      }
//      productOfAntPropertySums *= antPropertySums(i);
//    }
//    require(productOfAntPropertySums > 0);
//    
//    myPropertySum = 0.0;
//    for (i <- 0 until receivedMyPropertyMessage.size) {
//      myPropertySum += receivedMyPropertyMessage(i)
//    }
//  }
//  
//  private def totalSumForAntIdx(antIdx: Int): Double = {
//    var sum = 0.0;
//    for (pCurr <- 0 until myPropertyNode.domain.size) {
//      for (pAnt <- 0 until antPropertyNodes(antIdx).domain.size) {
//        sum += factorValue(antIdx, pCurr, pAnt) * receivedAntecedentMessage(antIdx) * receivedMyPropertyMessage(pCurr) * receivedAntPropertyMessages(antIdx)(pAnt);
//      }
//    }
//    sum *= productOfAntPropertySums / antPropertySums(antIdx)
//    sum;
//  }
//  
//  def sendMessages() {
////    Logger.logss("Dimensions of feature matrix: " + indexedFeatureMatrix.map(_.map(_.size).toSeq).toSeq)
//    for (i <- 0 until antecedentNode.domain.size) {
//      sentAntecedentMessage(i) = 0;
//    }
//    for (i <- 0 until myPropertyNode.domain.size) {
//      sentMyPropertyMessage(i) = 0;
//    }
//    for (i <- 0 until sentAntPropertyMessages.size; j <- 0 until sentAntPropertyMessages(i).size) {
//      sentAntPropertyMessages(i)(j) = 0;
//    }
//    
//    // VERY SLOW METHOD
////    val sentAntecedentMessage2 = new Array[Double](antecedentNode.domain.size);
////    val sentMyPropertyMessage2 = new Array[Double](myPropertyNode.domain.size);
////    val sentAntPropertyMessages2 = Array.tabulate(sentAntPropertyMessages.size)(i => new Array[Double](sentAntPropertyMessages(i).size));
////    Logger.logss("Sending message: " + antecedentNode.domain.size + " " + myPropertyNode.domain.size + " " + antPropertyNodes.map(_.domain.size).toSeq);
////    for (antIdx <- 0 until antecedentNode.domain.size; pCurr <- 0 until myPropertyNode.domain.size) {
////      val perm = new CombinatorialIterator(antPropertyNodes.map(_.domain.size));
////      while (perm.hasNext) {
////        val pAnts = perm.next;
////        var prod = factorValue(antIdx, pCurr, pAnts) * receivedAntecedentMessage(antIdx) * receivedMyPropertyMessage(pCurr);
////        for (i <- 0 until pAnts.size) {
////          prod *= receivedAntPropertyMessages(i)(pAnts(i));
////        }
////        sentAntecedentMessage2(antIdx) += prod / receivedAntecedentMessage(antIdx);
////        sentMyPropertyMessage2(pCurr) += prod / receivedMyPropertyMessage(pCurr);
////        for (i <- 0 until pAnts.size) {
////          sentAntPropertyMessages2(i)(pAnts(i)) += prod / receivedAntPropertyMessages(i)(pAnts(i));
////        }
////      }
////    }
//    
//    // FAST METHOD, SHOULD BE THE SAME
//    precomputeReceivedMessagesProductOfSums;
//    sentAntecedentMessage(antecedentNode.domain.size - 1) = productOfAntPropertySums * myPropertySum;
//    for (antIdx <- 0 until antecedentNode.domain.size - 1) {
//      for (pCurr <- 0 until myPropertyNode.domain.size) {
//        var pAnt = 0;
//        while (pAnt < antPropertyNodes(antIdx).domain.size) {
//          sentAntecedentMessage(antIdx) += factorValue(antIdx, pCurr, pAnt) * receivedMyPropertyMessage(pCurr) * receivedAntPropertyMessages(antIdx)(pAnt) *
//                  productOfAntPropertySums / antPropertySums(antIdx);
//          pAnt += 1;
//        }
//      }
//    }
//    for (pCurr <- 0 until myPropertyNode.domain.size) {
//      for (antIdx <- 0 until antecedentNode.domain.size - 1) {
//        var pAnt = 0;
//        while (pAnt < antPropertyNodes(antIdx).domain.size) {
//          sentMyPropertyMessage(pCurr) += factorValue(antIdx, pCurr, pAnt) * receivedAntecedentMessage(antIdx) * receivedAntPropertyMessages(antIdx)(pAnt) *
//                  productOfAntPropertySums / antPropertySums(antIdx);
//          pAnt += 1;
//        }
//      }
//      sentMyPropertyMessage(pCurr) += productOfAntPropertySums * receivedAntecedentMessage(antecedentNode.domain.size - 1);
//    }
//    
//    var allIndicesTotalSum = 0.0;
//    for (antIdx <- 0 until antecedentNode.domain.size - 1) {
//      allIndicesTotalSum += totalSumForAntIdx(antIdx);
//    }
//    for (antIdx <- 0 until antecedentNode.domain.size - 1) {
//      for (pAnt <- 0 until antPropertyNodes(antIdx).domain.size) {
//        sentAntPropertyMessages(antIdx)(pAnt) += (allIndicesTotalSum - totalSumForAntIdx(antIdx)) / antPropertySums(antIdx);
//        for (pCurr <- 0 until myPropertyNode.domain.size) {
//          sentAntPropertyMessages(antIdx)(pAnt) += factorValue(antIdx, pCurr, pAnt) * receivedAntecedentMessage(antIdx) * receivedMyPropertyMessage(pCurr) *
//                  productOfAntPropertySums / antPropertySums(antIdx);
//        }
//        sentAntPropertyMessages(antIdx)(pAnt) += receivedAntecedentMessage(antecedentNode.domain.size - 1) * productOfAntPropertySums / antPropertySums(antIdx) * myPropertySum;
//      }
//    }
//    
////    for (i <- 0 until sentAntecedentMessage2.size) {
////      Logger.logss(i + " ANT: " + sentAntecedentMessage(i) + "    " + sentAntecedentMessage2(i))
////    }
////    for (i <- 0 until sentMyPropertyMessage2.size) {
////      Logger.logss(i + " PROP: " + sentMyPropertyMessage(i) + "    " + sentMyPropertyMessage2(i))
////    }
////    for (i <- 0 until sentAntPropertyMessages2.size) {
////      for (j <- 0 until sentAntPropertyMessages2(i).size) {
////        Logger.logss(i + ", " + j + " ANTPROP: " + sentAntPropertyMessages(i)(j) + "    " + sentAntPropertyMessages2(i)(j))
////      }
////    }
//    
//    
//    // END STUFF
//    
//    GUtil.normalizeiHard(sentAntecedentMessage);
//    require(!sentAntecedentMessage.contains(0.0));
//    antecedentNode.receiveMessage(this, sentAntecedentMessage);
//    
//    GUtil.normalizeiHard(sentMyPropertyMessage);
//    require(!sentMyPropertyMessage.contains(0.0));
//    myPropertyNode.receiveMessage(this, sentMyPropertyMessage);
//    
//    for (i <- 0 until antPropertyNodes.size) {
//      GUtil.normalizeiHard(sentAntPropertyMessages(i));
//      require(!sentAntPropertyMessages(i).contains(0.0));
//      antPropertyNodes(i).receiveMessage(this, sentAntPropertyMessages(i));
//    }
//  }
//  
//  def computeFactorNormalizer = {
//    var normalizer = 0.0;
//    for (antIdx <- 0 until antecedentNode.domain.size - 1; pCurr <- 0 until myPropertyNode.domain.size; pAnt <- 0 until antPropertyNodes(antIdx).domain.size) {
//      normalizer += factorValue(antIdx, pCurr, pAnt) * receivedAntecedentMessage(antIdx) * receivedMyPropertyMessage(pCurr) * receivedAntPropertyMessages(antIdx)(pAnt) *
//              productOfAntPropertySums / antPropertySums(antIdx);
//    }
//    normalizer += receivedAntecedentMessage(antecedentNode.domain.size - 1) * myPropertySum * productOfAntPropertySums;
//    normalizer;
//  }
//  
//  def addExpectedFeatureCounts(scale: Float, gradient: Array[Float]) {
//    precomputeReceivedMessagesProductOfSums;
//    val normalizer = computeFactorNormalizer;
//    val multiplier = scale/normalizer;
//    for (antIdx <- 0 until antecedentNode.domain.size - 1; pCurr <- 0 until myPropertyNode.domain.size; pAnt <- 0 until antPropertyNodes(antIdx).domain.size) {
//      val value = factorValue(antIdx, pCurr, pAnt) * receivedAntecedentMessage(antIdx) * receivedMyPropertyMessage(pCurr) * receivedAntPropertyMessages(antIdx)(pAnt) *
//              productOfAntPropertySums / antPropertySums(antIdx);
//      var featIdx = 0;
//      while (featIdx < indexedFeatureMatrix(antIdx)(pCurr)(pAnt).size) {
//        gradient(indexedFeatureMatrix(antIdx)(pCurr)(pAnt)(featIdx)) += (multiplier * value).toFloat;
//        featIdx += 1;
//      }
//    }
//  }
//}
