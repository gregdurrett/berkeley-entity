package edu.berkeley.nlp.entity.bp

class CombinatorialIterator(domainSizes: Array[Int]) {
  val currComb = domainSizes.map(i => 0);
  var isStarting = true;
  
  def next = {
    if (!isStarting) {
      // Need to advance
      var idx = 0;
      var isAdvanced = false;
      while (!isAdvanced && idx < domainSizes.size) {
        if (currComb(idx) < domainSizes(idx) - 1) {
          currComb(idx) += 1;
          isAdvanced = true;
        } else {
          currComb(idx) = 0;
          idx += 1;
        }
      }
    }
    isStarting = false;
    currComb;
  }
  
  def isDone = !isStarting && (0 until currComb.size).map(i => currComb(i) == domainSizes(i) - 1).reduce(_ && _);
  
  def hasNext = !isDone;
}

object CombinatorialIterator {
  def main(args: Array[String]) {
    val combTest = new CombinatorialIterator(Array(3, 2, 1, 4));
    while (combTest.hasNext) {
      println(combTest.next.toSeq);
    }
  }
}
