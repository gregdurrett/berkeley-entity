package edu.berkeley.nlp.entity.bp
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.GUtil

class Node[T](val domain: Domain[T]) {
  var factors = new ArrayBuffer[Factor]();
  var receivedMessages: Array[Array[Double]] = null;
  var sentMessages: Array[Array[Double]] = null;
  var cachedBeliefsOrMarginals: Array[Double] = Array.fill(domain.size)(0.0);
  
  def registerFactor(factor: Factor) {
    factors += factor;
  }
  
  // TODO: Do I need this null thing?
  def initializeReceivedMessagesUniform() {
    if (receivedMessages == null) {
      receivedMessages = new Array[Array[Double]](factors.size);
    } else {
      for (i <- 0 until receivedMessages.size) {
        receivedMessages(i) = null;
      }
    }
  }
  
  def clearSentMessages() {
    this.sentMessages = null;
  }
  
  // This is just here so we can let things be null...At some point, it was a problem because
  // the received messages remember which factors sent them, so clearing them for some reason
  // caused problems (maybe writing the value 1.0 was problematic when we weren't clearing the
  // received messages on the other end?). Can probably get rid of this somehow and just do the
  // obvious thing of initializing messages to 1.0.
  def receivedMessageValue(i: Int, j: Int): Double = {
    if (receivedMessages(i) == null) {
      1.0;
    } else {
      receivedMessages(i)(j);
    }
  }
  
  def receiveMessage(factor: Factor, message: Array[Double]) {
    // Lots of checks on well-formedness
    require(receivedMessages != null);
    require(message.size == domain.size); 
    var messageIdx = 0;
    var total = 0.0
    // Message can contain some zeroes but can't be all zeroes
    while (messageIdx < message.size) {
      if (message(messageIdx).isNaN() || message(messageIdx).isInfinite) {
        Logger.logss("For domain: " + domain + ", bad received message: " + message.toSeq + " from " + factor.getClass());
        Logger.logss("Previous message: " + receivedMessages(factors.indexOf(factor)).toSeq);
        require(false);
      }
      total += message(messageIdx);
      messageIdx += 1;
    }
    if (total == 0) {
      Logger.logss("For domain: " + domain + ", bad received message: " + message.toSeq + " from " + factor.getClass());
      Logger.logss("Previous message: " + receivedMessages(factors.indexOf(factor)).toSeq);
      require(false)
    }
    // This is what the method actually does
    val idx = factors.indexOf(factor);
    require(idx != -1 && idx < receivedMessages.size);
    receivedMessages(idx) = message;
  }
  
  def sendMessages() {
    sendMessages(1.0);
  }
  
  def sendMessages(messageMultiplier: Double) {
    sendMessagesUseLogSpace(messageMultiplier);
  }
  
  // Received messages get exponentiated
  def sendMessagesUseLogSpace(messageMultiplier: Double) {
    for (i <- 0 until cachedBeliefsOrMarginals.size) {
      cachedBeliefsOrMarginals(i) = 0.0;
    }
    require(receivedMessages.size == factors.size);
    for (i <- 0 until receivedMessages.size) {
      var j = 0;
      while (j < cachedBeliefsOrMarginals.size) {
        cachedBeliefsOrMarginals(j) += Math.log(receivedMessageValue(i, j)) * messageMultiplier;
        j += 1;
      }
    }
    GUtil.logNormalizei(cachedBeliefsOrMarginals);
    require(!GUtil.containsNaN(cachedBeliefsOrMarginals), cachedBeliefsOrMarginals.toSeq)
    for (i <- 0 until cachedBeliefsOrMarginals.size) {
      cachedBeliefsOrMarginals(i) = Math.exp(cachedBeliefsOrMarginals(i));
    }
    require(!GUtil.containsNaN(cachedBeliefsOrMarginals), cachedBeliefsOrMarginals.toSeq)
    if (sentMessages == null) {
      sentMessages = new Array[Array[Double]](factors.size);
    }
    for (i <- 0 until factors.length) {
      if (sentMessages(i) == null) {
        sentMessages(i) = new Array[Double](domain.size);
      }
      var j = 0;
      var normalizer = 0.0;
      while (j < domain.size) {
        val rmVal = receivedMessageValue(i, j);
        if (rmVal == 0) {
          sentMessages(i)(j) = 0;
        } else {
          val msgVal = cachedBeliefsOrMarginals(j)/rmVal;
          normalizer += msgVal;
          sentMessages(i)(j) = msgVal;
        }
        j += 1;
      }
      require(normalizer > 0, domain.entries.toSeq);
      j = 0;
      while (j < domain.size) {
        sentMessages(i)(j) /= normalizer;
        j += 1;
      }
      factors(i).receiveMessage(this, sentMessages(i));
    }
  }
  
  def getMarginals(): Array[Double] = {
    getMarginalsUseLogSpace(1.0);
  }
  
  def getMarginals(messageMultiplier: Double): Array[Double] = {
    getMarginalsUseLogSpace(messageMultiplier);
  }
  
  def getMarginalsUseLogSpace(messageMultiplier: Double): Array[Double] = {
    for (i <- 0 until cachedBeliefsOrMarginals.size) {
      cachedBeliefsOrMarginals(i) = 0.0;
    }
    for (i <- 0 until cachedBeliefsOrMarginals.size) {
      for (j <- 0 until receivedMessages.size) {
        cachedBeliefsOrMarginals(i) += Math.log(receivedMessageValue(j, i)) * messageMultiplier;
      }
    }
//    if (domain.size < 20) {
//      Logger.logss("Node with domain " + domain.entries.toSeq + " receiving messages from " + factors.size + " factors");
//      receivedMessages.foreach(msg => Logger.logss(if (msg != null) msg.toSeq else "null"));
//    }
    GUtil.logNormalizei(cachedBeliefsOrMarginals);
    for (i <- 0 until cachedBeliefsOrMarginals.size) {
      cachedBeliefsOrMarginals(i) = Math.exp(cachedBeliefsOrMarginals(i));
    }
    cachedBeliefsOrMarginals
  }
}
