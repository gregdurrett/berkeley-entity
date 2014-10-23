package edu.berkeley.nlp.entity.bp

import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.futile.util.Logger

trait Factor {
  
  def setWeights(newWeights: Array[Float]);
  
  def clearAllMessages();
  
  def receiveMessage(node: Node[_], message: Array[Double]);
  
  def sendMessages();
  
  def getAllAssociatedFeatures(): Array[String];
  
  def computeFactorNormalizer: Double;
  
  def addExpectedFeatureCounts(scale: Float, gradient: Array[Float]);
}

class AgreementFactor(val nodeOne: Node[String],
                      val nodeTwo: Node[String],
                      val featureMatrix: Array[Array[Seq[String]]],
                      val indexedFeatureMatrix: Array[Array[Seq[Int]]],
                      val defaultValMatrix: Array[Array[Double]]) extends Factor {
  var cachedWeights: Array[Float] = null;
  nodeOne.registerFactor(this);
  nodeTwo.registerFactor(this);
  
  var receivedNodeOneMessage: Array[Double] = null;
  var receivedNodeTwoMessage: Array[Double] = null;
  var sentNodeOneMessage: Array[Double] = new Array[Double](nodeOne.domain.size);
  var sentNodeTwoMessage: Array[Double] = new Array[Double](nodeTwo.domain.size);
  
  def setWeights(newWeights: Array[Float]) {
    this.cachedWeights = newWeights;
    for (i <- 0 until sentNodeOneMessage.length) {
      sentNodeOneMessage(i) = 0;
    }
    for (i <- 0 until sentNodeTwoMessage.length) {
      sentNodeTwoMessage(i) = 0;
    }
  }
  
  def clearAllMessages() {
    throw new RuntimeException("Unimplemented");
  }
  
  def receiveMessage(node: Node[_], message: Array[Double]) {
    require(!GUtil.containsNaN(message));
    require(message.size == node.domain.size);
    if (node == nodeOne) {
      receivedNodeOneMessage = message;
    } else if (node == nodeTwo) {
      receivedNodeTwoMessage = message;
    } else {
      throw new RuntimeException("Bad node in graph");
    }
  }
  
  def factorValue(nodeOneValueIdx: Int, nodeTwoValueIdx: Int): Double = {
    var featValue = 1.0;
    featValue *= Math.exp(defaultValMatrix(nodeOneValueIdx)(nodeTwoValueIdx));
    var featIdx = 0;
    while (featIdx < indexedFeatureMatrix(nodeOneValueIdx)(nodeTwoValueIdx).size) {
      featValue *= Math.exp(cachedWeights(indexedFeatureMatrix(nodeOneValueIdx)(nodeTwoValueIdx)(featIdx)));
      featIdx += 1;
    }
    featValue;
    throw new RuntimeException("OPTIMIZE ME!");
  }
  
  def sendMessages() {
    throw new RuntimeException("Need to initialize these from null in case they're cleared");
    for (i <- 0 until sentNodeOneMessage.length) {
      sentNodeOneMessage(i) = 0;
    }
    for (i <- 0 until sentNodeTwoMessage.length) {
      sentNodeTwoMessage(i) = 0;
    }
    
    for (i <- 0 until nodeOne.domain.size) {
      // While loop for the inner loop here
      var j = 0;
      while (j < nodeTwo.domain.size) {
        val currFactorValue = factorValue(i, j);
        sentNodeOneMessage(i) += currFactorValue * receivedNodeTwoMessage(j);
        sentNodeTwoMessage(j) += currFactorValue * receivedNodeOneMessage(i);
        j += 1;
      }
    }
    GUtil.normalizeiHard(sentNodeOneMessage);
    GUtil.normalizeiHard(sentNodeTwoMessage);
    
    require(!sentNodeOneMessage.contains(0.0));
    require(!sentNodeTwoMessage.contains(0.0));
    nodeOne.receiveMessage(this, sentNodeOneMessage);
    nodeTwo.receiveMessage(this, sentNodeTwoMessage);
  }
  
  def getAllAssociatedFeatures(): Array[String] = {
    // Flatten matrix of lists of features
    featureMatrix.flatMap(_.flatMap((currFeats: Seq[String]) => currFeats)).toSet.toArray;
  }
  
  def computeFactorNormalizer: Double = {
    var normalizer = 0.0;
    for (i <- 0 until nodeOne.domain.size) {
      for (j <- 0 until nodeTwo.domain.size) {
        val value = factorValue(i, j) * receivedNodeOneMessage(i) * receivedNodeTwoMessage(j);
        normalizer += value;
      }
    }
    normalizer;
  }
  
  def addExpectedFeatureCounts(scale: Float, gradient: Array[Float]) {
    val normalizer = computeFactorNormalizer;
    val multiplier = scale/normalizer;
    for (i <- 0 until nodeOne.domain.size) {
      for (j <- 0 until nodeTwo.domain.size) {
        val value = factorValue(i, j) * receivedNodeOneMessage(i) * receivedNodeTwoMessage(j);
        var featIdx = 0;
        while (featIdx < indexedFeatureMatrix(i)(j).size) {
          gradient(indexedFeatureMatrix(i)(j)(featIdx)) += (multiplier * value).toFloat;
          featIdx += 1;
        }
      }
    }
  }
}

// N.B. The semantics are that a null feature sequence is prohibited
class BinaryFactorGeneral(val nodeOne: Node[_],
                          val nodeTwo: Node[_],
                          val indexedFeatureMatrix: Array[Array[Array[Int]]]) extends Factor {
  var cachedWeights: Array[Float] = null;
  nodeOne.registerFactor(this);
  nodeTwo.registerFactor(this);
  
  var receivedNodeOneMessage: Array[Double] = null;
  var receivedNodeTwoMessage: Array[Double] = null;
  var sentNodeOneMessage: Array[Double] = null;
  var sentNodeTwoMessage: Array[Double] = null;
  
  def setWeights(newWeights: Array[Float]) {
    this.cachedWeights = newWeights;
//    for (i <- 0 until sentNodeOneMessage.length) {
//      sentNodeOneMessage(i) = 0;
//    }
//    for (i <- 0 until sentNodeTwoMessage.length) {
//      sentNodeTwoMessage(i) = 0;
//    }
  }
  
  def clearAllMessages() {
    receivedNodeOneMessage = null;
    receivedNodeTwoMessage = null;
    sentNodeOneMessage = null;
    sentNodeTwoMessage = null;
  }
  
  def receiveMessage(node: Node[_], message: Array[Double]) {
    require(!GUtil.containsNaN(message));
    require(message.size == node.domain.size);
    if (node == nodeOne) {
      receivedNodeOneMessage = message;
    } else if (node == nodeTwo) {
      receivedNodeTwoMessage = message;
    } else {
      throw new RuntimeException("Bad node in graph");
    }
  }
  
  def factorValue(nodeOneValueIdx: Int, nodeTwoValueIdx: Int): Double = {
    if (indexedFeatureMatrix(nodeOneValueIdx)(nodeTwoValueIdx) == null) {
      // N.B. -10 instead of infinity for numerical reasons
      Math.exp(-10);
    } else {
      Math.exp(GUtil.scoreIndexedFeats(indexedFeatureMatrix(nodeOneValueIdx)(nodeTwoValueIdx), cachedWeights));
    }
  }
  
  def sendMessages() {
    if (sentNodeOneMessage == null) {
      sentNodeOneMessage = Array.tabulate(nodeOne.domain.size)(i => 0);
    } else {
      for (i <- 0 until sentNodeOneMessage.length) {
        sentNodeOneMessage(i) = 0;
      }
    }
    if (sentNodeTwoMessage == null) {
      sentNodeTwoMessage = Array.tabulate(nodeTwo.domain.size)(i => 0);
    } else {
      for (i <- 0 until sentNodeTwoMessage.length) {
        sentNodeTwoMessage(i) = 0;
      }
    }
    
    for (i <- 0 until nodeOne.domain.size) {
      // While loop for the inner loop here
      var j = 0;
      while (j < nodeTwo.domain.size) {
        val currFactorValue = factorValue(i, j);
        sentNodeOneMessage(i) += currFactorValue * receivedNodeTwoMessage(j);
        sentNodeTwoMessage(j) += currFactorValue * receivedNodeOneMessage(i);
        j += 1;
      }
    }
    GUtil.normalizeiHard(sentNodeOneMessage);
    GUtil.normalizeiHard(sentNodeTwoMessage);
//    require(!sentNodeOneMessage.contains(0.0), nodeOne.domain.entries.toSeq + " " + nodeTwo.domain.entries.toSeq);
//    require(!sentNodeTwoMessage.contains(0.0), nodeOne.domain.entries.toSeq + " " + nodeTwo.domain.entries.toSeq);
    nodeOne.receiveMessage(this, sentNodeOneMessage);
    nodeTwo.receiveMessage(this, sentNodeTwoMessage);
  }
  
  def getAllAssociatedFeatures(): Array[String] = {
    throw new RuntimeException("Deprecated");
  }
  
  def computeFactorNormalizer: Double = {
    var normalizer = 0.0;
    for (i <- 0 until nodeOne.domain.size) {
      for (j <- 0 until nodeTwo.domain.size) {
        val value = factorValue(i, j) * receivedNodeOneMessage(i) * receivedNodeTwoMessage(j);
        normalizer += value;
      }
    }
    normalizer;
  }
  
  def addExpectedFeatureCounts(scale: Float, gradient: Array[Float]) {
    val normalizer = computeFactorNormalizer;
    val multiplier = scale/normalizer;
    for (i <- 0 until nodeOne.domain.size) {
      for (j <- 0 until nodeTwo.domain.size) {
        if (indexedFeatureMatrix(i)(j) != null) {
          val value = factorValue(i, j) * receivedNodeOneMessage(i) * receivedNodeTwoMessage(j);
          val delta = (value * multiplier).toFloat;
          var featIdx = 0;
          while (featIdx < indexedFeatureMatrix(i)(j).size) {
            gradient(indexedFeatureMatrix(i)(j)(featIdx)) += delta
            featIdx += 1;
          }
        }
      }
    }
  }
}
                      

class PropertyFactor(val selectedAntecedentMentionIdx: Int,
                     val propertyNode: Node[String],
                     val antecedentNode: Node[Int],
                     val antecedentPropertyNode: Node[String],
                     val featureMatrix: Array[Array[Seq[String]]],
                     val indexedFeatureMatrix: Array[Array[Seq[Int]]],
                     val defaultValMatrix: Array[Array[Double]]) extends Factor {
  var cachedWeights: Array[Float] = null;
  require(antecedentPropertyNode.domain == propertyNode.domain);
  propertyNode.registerFactor(this);
  antecedentNode.registerFactor(this);
  antecedentPropertyNode.registerFactor(this);
  var selectedAntecedentValueIdx = -1;
  for (i <- 0 until antecedentNode.domain.size) {
    if (antecedentNode.domain.value(i) == selectedAntecedentMentionIdx) {
      selectedAntecedentValueIdx = i;
    }
  }
  
  var receivedPropertyMessage: Array[Double] = null;
  var receivedAntecedentMessage: Array[Double] = null;
  var receivedAntecedentPropertyMessage: Array[Double] = null;
  var sentPropertyMessage: Array[Double] = new Array[Double](propertyNode.domain.size);
  var sentAntecedentMessage: Array[Double] = new Array[Double](antecedentNode.domain.size);
  var sentAntecedentPropertyMessage: Array[Double] = new Array[Double](antecedentPropertyNode.domain.size);
  
  def setWeights(newWeights: Array[Float]) {
    this.cachedWeights = newWeights;
    for (i <- 0 until sentPropertyMessage.length) {
      sentPropertyMessage(i) = 0;
    }
    for (i <- 0 until sentAntecedentMessage.length) {
      sentAntecedentMessage(i) = 0;
    }
    for (i <- 0 until sentAntecedentPropertyMessage.length) {
      sentAntecedentPropertyMessage(i) = 0;
    }
  }
  
  def clearAllMessages() {
    throw new RuntimeException("Unimplemented");
  }
  
  def receiveMessage(node: Node[_], message: Array[Double]) {
    require(!GUtil.containsNaN(message));
    require(message.size == node.domain.size);
    if (node == propertyNode) {
      receivedPropertyMessage = message;
    } else if (node == antecedentNode) {
      receivedAntecedentMessage = message;
    } else if (node == antecedentPropertyNode) {
      receivedAntecedentPropertyMessage = message;
    } else {
      throw new RuntimeException("Bad node in graph");
    }
  }
  
  def factorValue(propertyValueIdx: Int, antecedentValueIdx: Int, antecedentPropertyValueIdx: Int): Double = {
    if (antecedentValueIdx == selectedAntecedentValueIdx) {
      var featValue = 1.0;
      featValue *= Math.exp(defaultValMatrix(propertyValueIdx)(antecedentPropertyValueIdx));
      var featIdx = 0;
      while (featIdx < indexedFeatureMatrix(propertyValueIdx)(antecedentPropertyValueIdx).size) {
        featValue *= Math.exp(cachedWeights(indexedFeatureMatrix(propertyValueIdx)(antecedentPropertyValueIdx)(featIdx)));
        featIdx += 1;
      }
      featValue;
    } else 1.0;
  }
  
  // TODO: Optimize this if it's slow
  def sendMessages() {
    for (i <- 0 until propertyNode.domain.size) {
      sentPropertyMessage(i) = 0;
    }
    for (i <- 0 until antecedentNode.domain.size) {
      sentAntecedentMessage(i) = 0;
    }
    for (i <- 0 until antecedentPropertyNode.domain.size) {
      sentAntecedentPropertyMessage(i) = 0;
    }
    
//    // Antecedent message
//    var propertyMessageSumForIrrelevantAntecedents = 0.0;
//    for (i <- 0 until propertyNode.domain.size) {
//      for (j <- 0 until antecedentPropertyNode.domain.size) {
//        propertyMessageSumForIrrelevantAntecedents += receivedPropertyMessage(i) * receivedAntecedentPropertyMessage(j);
//      }
//    }
//    for (k <- 0 until antecedentNode.domain.size) {
//      if (k != selectedAntecedentValueIdx) {
//        sentAntecedentMessage(k) = propertyMessageSumForIrrelevantAntecedents;
//      } else {
//        var propertyMessageSumForRelevantAntecedent = 0.0;
//        for (i <- 0 until propertyNode.domain.size) {
//          for (j <- 0 until antecedentPropertyNode.domain.size) {
//            propertyMessageSumForRelevantAntecedent += receivedPropertyMessage(i) * receivedAntecedentPropertyMessage(j) * factorValue(i, k, j);
//          }
//        }
//        sentAntecedentMessage(k) = propertyMessageSumForRelevantAntecedent;
//      }
//    }
//    // Property messages
//    var constantPropertyComponent = 0.0;
//    for (j <- 0 until antecedentPropertyNode.domain.size) {
//      for (k <- 0 until antecedentNode.domain.size) {
//        if (k != selectedAntecedentValueIdx) {
//          constantPropertyComponent += receivedAntecedentPropertyMessage(j) * receivedAntecedentMessage(k);
//        }
//      }
//    }
//    for (i <- 0 until propertyNode.domain.size) {
//      var messageVal = constantPropertyComponent;
//      for (j <- 0 until antecedentPropertyNode.domain.size) {
//        messageVal += receivedAntecedentPropertyMessage(j) * receivedAntecedentMessage(selectedAntecedentValueIdx) * factorValue(i, selectedAntecedentValueIdx, j);
//      }
//      sentPropertyMessage(i) = messageVal;
//    }
//    // Analogous for the other property message
//    
//    var constantAntecedentPropertyComponent = 0.0;
//    for (i <- 0 until propertyNode.domain.size) {
//      for (k <- 0 until antecedentNode.domain.size) {
//        if (k != selectedAntecedentValueIdx) {
//          constantAntecedentPropertyComponent += receivedPropertyMessage(i) * receivedAntecedentMessage(k);
//        }
//      }
//    }
//    for (j <- 0 until antecedentPropertyNode.domain.size) {
//      var messageVal = constantAntecedentPropertyComponent;
//      for (i <- 0 until propertyNode.domain.size) {
//        messageVal += receivedPropertyMessage(i) * receivedAntecedentMessage(selectedAntecedentValueIdx) * factorValue(i, selectedAntecedentValueIdx, j);
//      }
//      sentAntecedentPropertyMessage(j) = messageVal;
//    }
    
    
    // OLD METHOD
    for (i <- 0 until antecedentNode.domain.size) {
      for (j <- 0 until propertyNode.domain.size) {
        // While loop for the inner loop here
        var k = 0;
        while (k < antecedentPropertyNode.domain.size) {
          val currFactorValue = factorValue(j, i, k);
          sentPropertyMessage(j) += currFactorValue * receivedAntecedentMessage(i) * receivedAntecedentPropertyMessage(k);
          sentAntecedentMessage(i) += currFactorValue * receivedPropertyMessage(j) * receivedAntecedentPropertyMessage(k);
          sentAntecedentPropertyMessage(k) += currFactorValue * receivedPropertyMessage(j) * receivedAntecedentMessage(i);
          k += 1;
        }
      }
    }
    GUtil.normalizeiHard(sentPropertyMessage);
    GUtil.normalizeiHard(sentAntecedentMessage);
    GUtil.normalizeiHard(sentAntecedentPropertyMessage);
    
    if (sentPropertyMessage.contains(0.0)) {
      Logger.logss("Received prop message: " + receivedPropertyMessage.toSeq);
      Logger.logss("Received antecedent prop message: " + receivedAntecedentPropertyMessage.toSeq);
      Logger.logss("Received antecedent message: " + receivedAntecedentMessage.toSeq);
      for (i <- 0 until antecedentNode.domain.size) {
        for (j <- 0 until propertyNode.domain.size) {
          // While loop for the inner loop here
          var k = 0;
          while (k < antecedentPropertyNode.domain.size) {
            Logger.logss("Factor value for " + j + " " + i + " " + k + ": " + factorValue(j, i, k));
            k += 1;
          }
        }
      }
      require(false);
    }
    require(!sentAntecedentMessage.contains(0.0));
    require(!sentAntecedentPropertyMessage.contains(0.0));
    propertyNode.receiveMessage(this, sentPropertyMessage);
    antecedentNode.receiveMessage(this, sentAntecedentMessage);
    antecedentPropertyNode.receiveMessage(this, sentAntecedentPropertyMessage);
  }
  
  def getAllAssociatedFeatures(): Array[String] = {
    // Flatten matrix of lists of features
    featureMatrix.flatMap(_.flatMap((currFeats: Seq[String]) => currFeats)).toSet.toArray;
  }
  
  def computeFactorNormalizer: Double = {
    var normalizer = 0.0;
    for (i <- 0 until antecedentNode.domain.size) {
      for (j <- 0 until propertyNode.domain.size) {
        for (k <- 0 until antecedentPropertyNode.domain.size) {
          val value = factorValue(j, i, k) * receivedPropertyMessage(j) * receivedAntecedentMessage(i) * receivedAntecedentPropertyMessage(k);
          normalizer += value;
        }
      }
    }
    normalizer;
  }
  
  def addExpectedFeatureCounts(scale: Float, gradient: Array[Float]) {
    val normalizer = computeFactorNormalizer;
    val multiplier = scale/normalizer;
    for (i <- 0 until antecedentNode.domain.size) {
      if (antecedentNode.domain.entries(i) == selectedAntecedentMentionIdx) {
        for (j <- 0 until propertyNode.domain.size) {
          for (k <- 0 until antecedentPropertyNode.domain.size) {
            val value = factorValue(j, i, k) * receivedPropertyMessage(j) * receivedAntecedentMessage(i) * receivedAntecedentPropertyMessage(k);
            var featIdx = 0;
            while (featIdx < indexedFeatureMatrix(j)(k).size) {
              gradient(indexedFeatureMatrix(j)(k)(featIdx)) += (multiplier * value).toFloat;
              featIdx += 1;
            }
          }
        }
      }
    }
  }
}

class HardPropertyFactor(val selectedAntecedentMentionIdx: Int,
                         val propertyNode: Node[String],
                         val antecedentNode: Node[Int],
                         val antecedentPropertyNode: Node[String]) extends Factor {
  require(antecedentPropertyNode.domain == propertyNode.domain);
  propertyNode.registerFactor(this);
  antecedentNode.registerFactor(this);
  antecedentPropertyNode.registerFactor(this);
  var selectedAntecedentValueIdx = -1;
  for (i <- 0 until antecedentNode.domain.size) {
    if (antecedentNode.domain.value(i) == selectedAntecedentMentionIdx) {
      selectedAntecedentValueIdx = i;
    }
  }
  
  var receivedPropertyMessage: Array[Double] = null;
  var receivedAntecedentMessage: Array[Double] = null;
  var receivedAntecedentPropertyMessage: Array[Double] = null;
  var sentPropertyMessage: Array[Double] = new Array[Double](propertyNode.domain.size);
  var sentAntecedentMessage: Array[Double] = new Array[Double](antecedentNode.domain.size);
  var sentAntecedentPropertyMessage: Array[Double] = new Array[Double](antecedentPropertyNode.domain.size);
  
  def setWeights(newWeights: Array[Float]) {
    clearMessages();
  }
  
  def clearAllMessages() {
    throw new RuntimeException("Unimplemented");
  }
  
  def clearMessages() {
    var i = 0;
    while (i < propertyNode.domain.size) {
      sentPropertyMessage(i) = 0;
      i += 1;
    }    
    i = 0;
    while (i < propertyNode.domain.size) {
      sentAntecedentPropertyMessage(i) = 0;
      i += 1;
    }
    i = 0;
    while (i < sentAntecedentMessage.length) {
      sentAntecedentMessage(i) = 0;
      i += 1;
    }
  }
  
  def receiveMessage(node: Node[_], message: Array[Double]) {
    require(!GUtil.containsNaN(message));
    require(message.size == node.domain.size);
    if (node == propertyNode) {
      receivedPropertyMessage = message;
    } else if (node == antecedentNode) {
      receivedAntecedentMessage = message;
    } else if (node == antecedentPropertyNode) {
      receivedAntecedentPropertyMessage = message;
    } else {
      throw new RuntimeException("Bad node in graph");
    }
  }
  
  def factorValue(propertyValueIdx: Int, antecedentValueIdx: Int, antecedentPropertyValueIdx: Int): Double = {
    if (antecedentValueIdx == selectedAntecedentValueIdx && propertyValueIdx != antecedentPropertyValueIdx) 0.0 else 1.0;
  }
  
  // TODO: Optimize this if it's slow
  def sendMessages() {
    clearMessages();
    
    // NEW COMPUTATION METHOD
    var i = 0;
    var j = 0;
    var k = 0;
    var propNodeSum = 0.0;
    var antPropNodeSum = 0.0;
    var innerProduct = 0.0;
    i = 0;
    while (i < propertyNode.domain.size) {
      propNodeSum += receivedPropertyMessage(i);
      antPropNodeSum += receivedAntecedentPropertyMessage(i);
      innerProduct += receivedPropertyMessage(i) * receivedAntecedentPropertyMessage(i);
      i += 1;
    }
    var nonselectedAntecedentSum = 0.0;
    var selectedAntecedentVal = 0.0;
    k = 0;
    while (k < antecedentNode.domain.size) {
      if (k != selectedAntecedentValueIdx) {
        nonselectedAntecedentSum += receivedAntecedentMessage(k);
      } else {
        selectedAntecedentVal = receivedAntecedentMessage(k);
      }
      k += 1;
    }
    
    i = 0;
    while (i < propertyNode.domain.size) {
      sentPropertyMessage(i) = antPropNodeSum * nonselectedAntecedentSum + receivedAntecedentPropertyMessage(i) * selectedAntecedentVal;
      i += 1;
    }
    j = 0;
    while (j < antecedentPropertyNode.domain.size) {
      sentAntecedentPropertyMessage(j) = propNodeSum * nonselectedAntecedentSum + receivedPropertyMessage(j) * selectedAntecedentVal;
      j += 1;
    }
    k = 0;
    while (k < antecedentNode.domain.size) {
      if (k == selectedAntecedentValueIdx) {
        sentAntecedentMessage(k) = innerProduct;
      } else {
        sentAntecedentMessage(k) = propNodeSum * antPropNodeSum;
      }
      k += 1;
    }
    
    // OLD METHOD
//    for (i <- 0 until antecedentNode.domain.size) {
//      for (j <- 0 until propertyNode.domain.size) {
//        var k = 0;
//        while (k < antecedentPropertyNode.domain.size) {
//          val currFactorValue = factorValue(j, i, k);
//          sentPropertyMessage(j) += currFactorValue * receivedAntecedentMessage(i) * receivedAntecedentPropertyMessage(k);
//          sentAntecedentMessage(i) += currFactorValue * receivedPropertyMessage(j) * receivedAntecedentPropertyMessage(k);
//          sentAntecedentPropertyMessage(k) += currFactorValue * receivedPropertyMessage(j) * receivedAntecedentMessage(i);
//          k += 1;
//        }
//      }
//    }
    
    GUtil.normalizeiHard(sentPropertyMessage);
    GUtil.normalizeiHard(sentAntecedentMessage);
    GUtil.normalizeiHard(sentAntecedentPropertyMessage);
    
    if (sentPropertyMessage.contains(0.0)) {
      Logger.logss("Received prop message: " + receivedPropertyMessage.toSeq);
      Logger.logss("Received antecedent prop message: " + receivedAntecedentPropertyMessage.toSeq);
      Logger.logss("Received antecedent message: " + receivedAntecedentMessage.toSeq);
      for (i <- 0 until antecedentNode.domain.size) {
        for (j <- 0 until propertyNode.domain.size) {
          // While loop for the inner loop here
          var k = 0;
          while (k < antecedentPropertyNode.domain.size) {
            Logger.logss("Factor value for " + j + " " + i + " " + k + ": " + factorValue(j, i, k));
            k += 1;
          }
        }
      }
      require(false);
    }
    require(!sentAntecedentMessage.contains(0.0));
    require(!sentAntecedentPropertyMessage.contains(0.0));
    propertyNode.receiveMessage(this, sentPropertyMessage);
    antecedentNode.receiveMessage(this, sentAntecedentMessage);
    antecedentPropertyNode.receiveMessage(this, sentAntecedentPropertyMessage);
  }
  
  def computeFactorNormalizer: Double = {
    throw new RuntimeException("Unimplemented for hard factors; need to sum over all legal configurations");
  }
  
  def getAllAssociatedFeatures(): Array[String] = new Array[String](0);
  
  def addExpectedFeatureCounts(scale: Float, gradient: Array[Float]) {}
}




class BetterPropertyFactor[T](val selectedAntecedentMentionIdx: Int,
                              val propertyNode: Node[T],
                              val antecedentNode: Node[Int],
                              val antecedentPropertyNode: Node[T],
                              val indexedFeatureMatrix: Array[Array[Array[Int]]]) extends Factor {
  var cachedWeights: Array[Float] = null;
  propertyNode.registerFactor(this);
  antecedentNode.registerFactor(this);
  antecedentPropertyNode.registerFactor(this);
  var selectedAntecedentValueIdx = -1;
  for (i <- 0 until antecedentNode.domain.size) {
    if (antecedentNode.domain.value(i) == selectedAntecedentMentionIdx) {
      selectedAntecedentValueIdx = i;
    }
  }
  
  var receivedPropertyMessage: Array[Double] = null;
  var receivedAntecedentMessage: Array[Double] = null;
  var receivedAntecedentPropertyMessage: Array[Double] = null;
  var sentPropertyMessage: Array[Double] = null;
  var sentAntecedentMessage: Array[Double] = null;
  var sentAntecedentPropertyMessage: Array[Double] = null;
  
  def setWeights(newWeights: Array[Float]) {
    this.cachedWeights = newWeights;
//    for (i <- 0 until sentPropertyMessage.length) {
//      sentPropertyMessage(i) = 0;
//    }
//    for (i <- 0 until sentAntecedentMessage.length) {
//      sentAntecedentMessage(i) = 0;
//    }
//    for (i <- 0 until sentAntecedentPropertyMessage.length) {
//      sentAntecedentPropertyMessage(i) = 0;
//    }
  }
  
  def clearAllMessages() {
    receivedPropertyMessage = null;
    receivedAntecedentMessage = null;
    receivedAntecedentPropertyMessage = null;
    sentPropertyMessage = null;
    sentAntecedentMessage = null;
    sentAntecedentPropertyMessage = null;
  }
  
  def receiveMessage(node: Node[_], message: Array[Double]) {
    require(!GUtil.containsNaN(message));
    require(message.size == node.domain.size);
    if (node == propertyNode) {
      receivedPropertyMessage = message;
    } else if (node == antecedentNode) {
      receivedAntecedentMessage = message;
    } else if (node == antecedentPropertyNode) {
      receivedAntecedentPropertyMessage = message;
    } else {
      throw new RuntimeException("Bad node in graph");
    }
  }
  
  def factorValue(propertyValueIdx: Int, antecedentValueIdx: Int, antecedentPropertyValueIdx: Int): Double = {
    if (antecedentValueIdx == selectedAntecedentValueIdx) {
      var logFeatValue = GUtil.scoreIndexedFeats(indexedFeatureMatrix(propertyValueIdx)(antecedentPropertyValueIdx), cachedWeights);
      Math.exp(logFeatValue)
    } else {
      1.0;
    }
  }
  
  def sendMessages() {
    if (sentPropertyMessage == null) {
      sentPropertyMessage = Array.tabulate(propertyNode.domain.size)(i => 0);
    }
    if (sentAntecedentMessage == null) {
      sentAntecedentMessage = Array.tabulate(antecedentNode.domain.size)(i => 0);
    }
    if (sentAntecedentPropertyMessage == null) {
      sentAntecedentPropertyMessage = Array.tabulate(antecedentPropertyNode.domain.size)(i => 0);
    }
    // OLD METHOD
//      for (i <- 0 until propertyNode.domain.size) {
//        sentPropertyMessage(i) = 0;
//      }
//      for (i <- 0 until antecedentNode.domain.size) {
//        sentAntecedentMessage(i) = 0;
//      }
//      for (i <- 0 until antecedentPropertyNode.domain.size) {
//        sentAntecedentPropertyMessage(i) = 0;
//      }
//    for (i <- 0 until antecedentNode.domain.size) {
//      for (j <- 0 until propertyNode.domain.size) {
//        // While loop for the inner loop here
//        var k = 0;
//        while (k < antecedentPropertyNode.domain.size) {
//          val currFactorValue = factorValue(j, i, k);
//          sentPropertyMessage(j) += currFactorValue * receivedAntecedentMessage(i) * receivedAntecedentPropertyMessage(k);
//          sentAntecedentMessage(i) += currFactorValue * receivedPropertyMessage(j) * receivedAntecedentPropertyMessage(k);
//          sentAntecedentPropertyMessage(k) += currFactorValue * receivedPropertyMessage(j) * receivedAntecedentMessage(i);
//          k += 1;
//        }
//      }
//    }
    // NEW METHOD
    // Antecedent message
    var propertyMessageSumForIrrelevantAntecedents = 0.0;
    for (i <- 0 until propertyNode.domain.size) {
      var j = 0;
      var sum = 0.0;
      while (j < antecedentPropertyNode.domain.size) {
        sum += receivedAntecedentPropertyMessage(j);
        j += 1;
      }
      propertyMessageSumForIrrelevantAntecedents += receivedPropertyMessage(i) * sum;
    }
    for (k <- 0 until antecedentNode.domain.size) {
      if (k != selectedAntecedentValueIdx) {
//        maybeDisplay("AN IRR", sentAntecedentMessage(k), propertyMessageSumForIrrelevantAntecedents);
        sentAntecedentMessage(k) = propertyMessageSumForIrrelevantAntecedents;
      } else {
        var propertyMessageSumForRelevantAntecedent = 0.0;
        for (i <- 0 until propertyNode.domain.size) {
          var j = 0;
          while (j < antecedentPropertyNode.domain.size) {
            propertyMessageSumForRelevantAntecedent += receivedPropertyMessage(i) * receivedAntecedentPropertyMessage(j) * factorValue(i, k, j);
            j += 1;
          }
        }
//        maybeDisplay("AN REL", sentAntecedentMessage(k), propertyMessageSumForRelevantAntecedent);
        sentAntecedentMessage(k) = propertyMessageSumForRelevantAntecedent;
      }
    }
    // Property messages
    var irrelevantAntecedentSum = 0.0;
    var k = 0;
    while (k < antecedentNode.domain.size) {
      if (k != selectedAntecedentValueIdx) {
        irrelevantAntecedentSum += receivedAntecedentMessage(k);
      }
      k += 1;
    }
    var constantPropertyComponent = 0.0;
    for (j <- 0 until antecedentPropertyNode.domain.size) {
      constantPropertyComponent += receivedAntecedentPropertyMessage(j) * irrelevantAntecedentSum;
    }
    for (i <- 0 until propertyNode.domain.size) {
      var messageVal = constantPropertyComponent;
      var j = 0;
      while (j < antecedentPropertyNode.domain.size) {
        messageVal += receivedAntecedentPropertyMessage(j) * receivedAntecedentMessage(selectedAntecedentValueIdx) * factorValue(i, selectedAntecedentValueIdx, j);
        j += 1;
      }
//      maybeDisplay("PROP", sentPropertyMessage(i), messageVal);
      sentPropertyMessage(i) = messageVal;
    }
    // Analogous for the other property message
    
    var constantAntecedentPropertyComponent = 0.0;
    for (i <- 0 until propertyNode.domain.size) {
      constantAntecedentPropertyComponent += receivedPropertyMessage(i) * irrelevantAntecedentSum;
    }
    for (j <- 0 until antecedentPropertyNode.domain.size) {
      var messageVal = constantAntecedentPropertyComponent;
      var i = 0;
      while (i < propertyNode.domain.size) {
        messageVal += receivedPropertyMessage(i) * receivedAntecedentMessage(selectedAntecedentValueIdx) * factorValue(i, selectedAntecedentValueIdx, j);
        i += 1;
      }
      sentAntecedentPropertyMessage(j) = messageVal;
    }
    GUtil.normalizeiHard(sentPropertyMessage);
    GUtil.normalizeiHard(sentAntecedentMessage);
    GUtil.normalizeiHard(sentAntecedentPropertyMessage);
    if (sentPropertyMessage.contains(0.0)) {
      Logger.logss("Received prop message: " + receivedPropertyMessage.toSeq);
      Logger.logss("Received antecedent prop message: " + receivedAntecedentPropertyMessage.toSeq);
      Logger.logss("Received antecedent message: " + receivedAntecedentMessage.toSeq);
      for (i <- 0 until antecedentNode.domain.size) {
        for (j <- 0 until propertyNode.domain.size) {
          // While loop for the inner loop here
          var k = 0;
          while (k < antecedentPropertyNode.domain.size) {
            Logger.logss("Factor value for " + j + " " + i + " " + k + ": " + factorValue(j, i, k));
            k += 1;
          }
        }
      }
      require(false);
    }
    require(!sentAntecedentMessage.contains(0.0));
    require(!sentAntecedentPropertyMessage.contains(0.0));
    propertyNode.receiveMessage(this, sentPropertyMessage);
    antecedentNode.receiveMessage(this, sentAntecedentMessage);
    antecedentPropertyNode.receiveMessage(this, sentAntecedentPropertyMessage);
  }
  
  def getAllAssociatedFeatures(): Array[String] = Array[String]();
  
  def computeFactorNormalizer: Double = {
    var normalizer = 0.0;
    for (i <- 0 until antecedentNode.domain.size) {
      for (j <- 0 until propertyNode.domain.size) {
        for (k <- 0 until antecedentPropertyNode.domain.size) {
          val value = factorValue(j, i, k) * receivedPropertyMessage(j) * receivedAntecedentMessage(i) * receivedAntecedentPropertyMessage(k);
          normalizer += value;
        }
      }
    }
    normalizer;
  }
  
  def addExpectedFeatureCounts(scale: Float, gradient: Array[Float]) {
    val normalizer = computeFactorNormalizer;
    val multiplier = scale/normalizer;
    for (i <- 0 until antecedentNode.domain.size) {
      if (antecedentNode.domain.entries(i) == selectedAntecedentMentionIdx) {
        for (j <- 0 until propertyNode.domain.size) {
          for (k <- 0 until antecedentPropertyNode.domain.size) {
            val value = factorValue(j, i, k) * receivedPropertyMessage(j) * receivedAntecedentMessage(i) * receivedAntecedentPropertyMessage(k);
            var featIdx = 0;
            while (featIdx < indexedFeatureMatrix(j)(k).size) {
              gradient(indexedFeatureMatrix(j)(k)(featIdx)) += (multiplier * value).toFloat;
              featIdx += 1;
            }
          }
        }
      }
    }
  }
}


// Old unary factor implementation, only used for antecedent nodes anymore
class UnaryFactorOld(val propertyNode: Node[_]) extends Factor {
//                     val unaryFactor: Array[Double]) extends Factor {
  val unaryFactor = Array.fill(propertyNode.domain.size)(0.0);
  propertyNode.registerFactor(this);
  var receivedPropertyMessage: Array[Double] = null;
  
  def setWeights(newWeights: Array[Float]) {
    // Do nothing
  }
  
  def clearAllMessages() {
    throw new RuntimeException("Unimplemented");
  }
  
  def setUnaryFactor(newFactor: Array[Double]) {
    require(unaryFactor.length == newFactor.length);
    for (i <- 0 until unaryFactor.length) {
      unaryFactor(i) = newFactor(i);
    }
  }
  
  def setUnaryFactor(newFactor: Array[Float]) {
    require(unaryFactor.length == newFactor.length);
    for (i <- 0 until unaryFactor.length) {
      unaryFactor(i) = newFactor(i);
    }
  }
  
  def receiveMessage(node: Node[_], message: Array[Double]) {
    if (node == propertyNode) {
      receivedPropertyMessage = message;
    } else {
      throw new RuntimeException("Bad node in graph");
    }
  }
  
  def sendMessages() {
    propertyNode.receiveMessage(this, unaryFactor);
  }
  
  def computeFactorNormalizer = {
    var normalizer = 0.0;
    for (i <- 0 until propertyNode.domain.size) {
      val value = unaryFactor(i) * receivedPropertyMessage(i);
      normalizer += value;
    }
    normalizer;
  }
  
  def getAllAssociatedFeatures(): Array[String] = new Array[String](0);
  
  def addExpectedFeatureCounts(scale: Float, gradient: Array[Float]) {
    // No features so nothing to do
  }
}

class UnaryFactorGeneral(val propertyNode: Node[_],
                         val indexedFeatures: Array[Array[Int]]) extends Factor {
  var cachedWeights: Array[Float] = null;
  var constantOffset: Array[Float] = null;
  propertyNode.registerFactor(this);
  require(propertyNode.domain.size == indexedFeatures.size);
  var receivedMessage: Array[Double] = null;
  var sentMessage: Array[Double] = null;
  var cacheDirty = true;
  
//  val indexedFeaturesForEachValue = featuresForEachValue.map(_.map(featurizer.getIndex(_, false)));
  
  def setWeights(newWeights: Array[Float]) {
    this.cachedWeights = newWeights;
    this.cacheDirty = true;
  }
  
  def setConstantOffset(newOffset: Array[Float]) {
    this.constantOffset = newOffset;
  }
  
  def clearAllMessages() {
    receivedMessage = null;
    sentMessage = null;
    cacheDirty = true;
  }
    
  def receiveMessage(node: Node[_], message: Array[Double]) {
    if (node == propertyNode) {
      receivedMessage = message;
    } else {
      throw new RuntimeException("Bad node in graph");
    }
  }
  
  // TODO: OPTIMIZE ME
  def factorValue(propertyValueIdx: Int): Double = {
//    Math.exp(GUtil.scoreIndexedFeats(indexedFeatures(propertyValueIdx) + constantOffset(propertyValueIdx), cachedWeights));
    var logFeatValue = GUtil.scoreIndexedFeats(indexedFeatures(propertyValueIdx), cachedWeights);
//    var featIdx = 0;
//    while (featIdx < indexedFeatures(propertyValueIdx).size) {
//      logFeatValue += cachedWeights(indexedFeatures(propertyValueIdx)(featIdx));
//      featIdx += 1;
//    }
    if (constantOffset != null) {
      logFeatValue += constantOffset(propertyValueIdx);
    }
    val featValue = Math.exp(logFeatValue);
    require(!featValue.isInfinite(), indexedFeatures(propertyValueIdx).toSeq.map(cachedWeights(_)));
    featValue;
  }
  
  def sendMessages() {
    if (cacheDirty) {
      if (sentMessage == null) {
        sentMessage = Array.tabulate(propertyNode.domain.size)(i => factorValue(i));
      } else {
        for (i <- 0 until sentMessage.size) {
          sentMessage(i) = factorValue(i);
        }
      }
      cacheDirty = false;
    }
    propertyNode.receiveMessage(this, sentMessage);
  }
  
  def getAllAssociatedFeatures(): Array[String] = {
    throw new RuntimeException("Deprecated");
  }
  
  def computeFactorNormalizer = {
    var normalizer = 0.0;
    for (i <- 0 until propertyNode.domain.size) {
      val value = factorValue(i) * receivedMessage(i);
      normalizer += value;
    }
    normalizer;
  }
  
  def addExpectedFeatureCounts(scale: Float, gradient: Array[Float]) {
    val normalizer = computeFactorNormalizer;
    val multiplier = scale/normalizer;
    for (i <- 0 until propertyNode.domain.size) {
      val delta = (factorValue(i) * receivedMessage(i) * multiplier).toFloat;
      var featIdx = 0;
      while (featIdx < indexedFeatures(i).size) {
        gradient(indexedFeatures(i)(featIdx)) += delta;
        featIdx += 1;
      }
    }
  }
}
