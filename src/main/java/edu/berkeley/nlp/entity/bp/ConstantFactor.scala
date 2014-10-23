package edu.berkeley.nlp.entity.bp

import edu.berkeley.nlp.entity.GUtil
import edu.berkeley.nlp.math.SloppyMath

trait ConstantFactor[T] extends Factor {
  
  val normalize = true;
//  val normalize = false;
  
  def getNodes: Seq[Node[T]];
  def factorValue(variableSetting: Array[Int]): Double;
  
  // Also receiveMessage, sendMessages, clearAllMessages, computeFactorNormalizer
  
  // Don't need any of these methods since it's constant
  
  def setWeights(newWeights: Array[Float]) {}
  def getAllAssociatedFeatures(): Array[String] = new Array[String](0);
  def addExpectedFeatureCounts(scale: Float, gradient: Array[Float]) {}
}

class ConstantUnaryFactor[T](val node: Node[T],
                             val factorValues: Array[Double]) extends ConstantFactor[T] {
  node.registerFactor(this);
  var receivedNodeMessage: Array[Double] = null;
  var sentNodeMessage: Array[Double] = new Array[Double](node.domain.size);
  
  def getNodes = Seq(node);
  
  def factorValue(variableSetting: Array[Int]): Double = factorValues(variableSetting(0));
  
  def computeFactorNormalizer = {
    var normalizer = 0.0;
    for (i <- 0 until node.domain.size) {
      normalizer += factorValues(i) * receivedNodeMessage(i);
    }
    normalizer;
  }
  
  def clearAllMessages() {
    receivedNodeMessage = null;
    sentNodeMessage = null;
  }
  
  def receiveMessage(incNode: Node[_], message: Array[Double]) {
    require(!GUtil.containsNaN(message));
    require(message.size == incNode.domain.size);
    if (incNode == node) {
      receivedNodeMessage = message;
    } else {
      throw new RuntimeException("Bad node in graph");
    }
  }
  
  def sendMessages() {
    if (sentNodeMessage == null) {
      sentNodeMessage = new Array[Double](node.domain.size);
    }
    for (i <- 0 until node.domain.size) {
      sentNodeMessage(i) = factorValues(i);
    }
    if (normalize) {
      GUtil.normalizeiHard(sentNodeMessage);
    }
    node.receiveMessage(this, sentNodeMessage);
  }
}

class ConstantBinaryFactor[T](val nodeOne: Node[T],
                              val nodeTwo: Node[T],
                              val factorValues: Array[Array[Double]]) extends ConstantFactor[T] {
  
  nodeOne.registerFactor(this);
  nodeTwo.registerFactor(this);
  
  var receivedNodeOneMessage: Array[Double] = null;
  var receivedNodeTwoMessage: Array[Double] = null;
  var sentNodeOneMessage: Array[Double] = null;
  var sentNodeTwoMessage: Array[Double] = null;
  
  def getNodes = Seq(nodeOne, nodeTwo);
  
  def factorValue(variableSetting: Array[Int]): Double = factorValues(variableSetting(0))(variableSetting(1));
  
  def computeFactorNormalizer = {
    var normalizer = 0.0;
    for (i <- 0 until nodeOne.domain.size) {
      // While loop for the inner loop here
      var j = 0;
      while (j < nodeTwo.domain.size) {
        normalizer += factorValues(i)(j) * receivedNodeOneMessage(i) * receivedNodeTwoMessage(j);
        j += 1;
      }
    }
    normalizer;
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
  
  def sendMessages() {
    if (sentNodeOneMessage == null) {
      sentNodeOneMessage = new Array[Double](nodeOne.domain.size);
    }
    if (sentNodeTwoMessage == null) {
      sentNodeTwoMessage = new Array[Double](nodeTwo.domain.size);
    }
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
        val currFactorValue = factorValues(i)(j);
        sentNodeOneMessage(i) += currFactorValue * receivedNodeTwoMessage(j);
        sentNodeTwoMessage(j) += currFactorValue * receivedNodeOneMessage(i);
        j += 1;
      }
    }
    if (normalize) {
      GUtil.normalizeiHard(sentNodeOneMessage);
      GUtil.normalizeiHard(sentNodeTwoMessage);
    }
    nodeOne.receiveMessage(this, sentNodeOneMessage);
    nodeTwo.receiveMessage(this, sentNodeTwoMessage);
  }
}


class ConstantTernaryFactor[T](val nodes: Seq[Node[T]],
                               val factorValues: Array[Array[Array[Double]]]) extends ConstantFactor[T] {
  
  val receivedNodeMessages = new Array[Array[Double]](nodes.size);
  val sentNodeMessages = new Array[Array[Double]](nodes.size);
  for (i <- 0 until nodes.size) {
    nodes(i).registerFactor(this);
    receivedNodeMessages(i) = null;
    sentNodeMessages(i) = new Array[Double](nodes(i).domain.size);
  }
  
  def getNodes = nodes;
  
  def factorValue(variableSetting: Array[Int]): Double = factorValues(variableSetting(0))(variableSetting(1))(variableSetting(2));
  
  def computeFactorNormalizer = {
    var normalizer = 0.0;
    val combIterator = new CombinatorialIterator(nodes.map(_.domain.size).toArray);
    while (combIterator.hasNext) {
      val combination = combIterator.next;
      var currVal = factorValue(combination);
      for (i <- 0 until combination.size) {
        currVal *= receivedNodeMessages(i)(combination(i));
      }
      normalizer += currVal;
    }
    normalizer;
  }
  
  def clearAllMessages() {
    throw new RuntimeException("Unimplemented");
  }
  
  def receiveMessage(node: Node[_], message: Array[Double]) {
    require(!GUtil.containsNaN(message))
    require(!message.contains(0.0));
    require(message.size == node.domain.size);
    if (nodes.contains(node)) {
      receivedNodeMessages(nodes.indexOf(node)) = message;
    } else {
      throw new RuntimeException("Bad node in graph");
    }
  }
  
  def sendMessages() {
    sendMessagesLogSpace();
  }
  
  def sendMessagesLogSpace() {
    for (i <- 0 until sentNodeMessages.size) {
      for (j <- 0 until sentNodeMessages(i).size) {
        sentNodeMessages(i)(j) = Double.NegativeInfinity;
      }
    }
    for (i <- 0 until nodes(0).domain.size) {
      for (j <- 0 until nodes(1).domain.size) {
        // While loop for the inner loop here
        var k = 0;
        while (k < nodes(2).domain.size) {
          val currFactorValue = factorValues(i)(j)(k);
          require(currFactorValue != 0.0, factorValues.map(_.map(_.toSeq).toSeq).toSeq);
          sentNodeMessages(0)(i) = SloppyMath.logAdd(sentNodeMessages(0)(i), Math.log(currFactorValue) + Math.log(receivedNodeMessages(1)(j)) + Math.log(receivedNodeMessages(2)(k)));
          sentNodeMessages(1)(j) = SloppyMath.logAdd(sentNodeMessages(1)(j), Math.log(currFactorValue) + Math.log(receivedNodeMessages(0)(i)) + Math.log(receivedNodeMessages(2)(k)));
          sentNodeMessages(2)(k) = SloppyMath.logAdd(sentNodeMessages(2)(k), Math.log(currFactorValue) + Math.log(receivedNodeMessages(0)(i)) + Math.log(receivedNodeMessages(1)(j)));
          k += 1;
        }
      }
    }
    for (i <- 0 until sentNodeMessages.size) {
      if (normalize) {
        GUtil.logNormalizei(sentNodeMessages(i));
      }
      sentNodeMessages(i) = sentNodeMessages(i).map(Math.exp(_));
      nodes(i).receiveMessage(this, sentNodeMessages(i));
    }
  }
  
  def sendMessagesRealSpace() {
    for (i <- 0 until sentNodeMessages.size) {
      for (j <- 0 until sentNodeMessages(i).size) {
        sentNodeMessages(i)(j) = 0;
      }
    }
    for (i <- 0 until nodes(0).domain.size) {
      for (j <- 0 until nodes(1).domain.size) {
        // While loop for the inner loop here
        var k = 0;
        while (k < nodes(2).domain.size) {
          val currFactorValue = factorValues(i)(j)(k);
          sentNodeMessages(0)(i) += currFactorValue * receivedNodeMessages(1)(j) * receivedNodeMessages(2)(k);
          sentNodeMessages(1)(j) += currFactorValue * receivedNodeMessages(0)(i) * receivedNodeMessages(2)(k);
          sentNodeMessages(2)(k) += currFactorValue * receivedNodeMessages(0)(i) * receivedNodeMessages(1)(j);
          k += 1;
        }
      }
    }
    for (i <- 0 until sentNodeMessages.size) {
      if (normalize) {
        GUtil.normalizeiHard(sentNodeMessages(i));
      }
      require(!sentNodeMessages(i).contains(0.0));
      nodes(i).receiveMessage(this, sentNodeMessages(i));
    }
  }
}
