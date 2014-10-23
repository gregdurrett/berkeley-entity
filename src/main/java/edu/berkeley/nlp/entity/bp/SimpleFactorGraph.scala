package edu.berkeley.nlp.entity.bp

import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.math.SloppyMath

class SimpleFactorGraph[T](val nodes: Seq[Node[T]],
                           val factors: Seq[ConstantFactor[T]]) {
  
  def runBP(numItrs: Int) {
    nodes.foreach(_.initializeReceivedMessagesUniform());
    for (i <- 0 until numItrs) {
      for (node <- nodes) {
        node.sendMessages;
      }
      for (factor <- factors) {
        factor.sendMessages;
      }
    }
  }
  
  def computeLogNormalizerApprox = SimpleFactorGraph.computeLogNormalizerApprox(nodes, factors);
  
  def computeLogNormalizerExplicit = {
    var normalizer = Double.NegativeInfinity;
    val combs = new CombinatorialIterator(nodes.map(_.domain.size).toArray);
    while (combs.hasNext) {
      val config = combs.next;
      val score = logScoreConfiguration(config);
//      Logger.logss((0 until config.size).map(i => nodes(i).domain.entries(config(i))).toSeq + ": " + score)
      normalizer = SloppyMath.logAdd(normalizer, score);
    }
    normalizer;
  }
  
  def logScoreConfiguration(values: Array[Int]) = {
    var score = 0.0;
    for (factor <- factors) {
      val nodeValues = factor.getNodes.map(node => values(nodes.indexOf(node))).toArray;
      val factorValue = Math.log(factor.factorValue(nodeValues));
//      val nodeDomainValues = factor.getNodes.map(node => node.domain.entries(values(nodes.indexOf(node)))).toSeq;
//      Logger.logss(factor.getClass.toString.substring(factor.getClass.toString.lastIndexOf(".") + 1) +
//                   ", node values: " + nodeDomainValues + " => value: " + factorValue)
      score += factorValue;
    }
    score;
  }
}

object SimpleFactorGraph {
  
//  def computeNormalizerApprox(nodes: Seq[Node[_]],
//                              factors: Seq[Factor]) = {
//    var approxLikelihood = 1.0;
//    for (i <- 0 until factors.size) {
//      val factor = factors(i);
//      val thisFactorNormalizer = factor.computeFactorNormalizer
////      println("Factor " + i + ": " + thisFactorNormalizer)
//      approxLikelihood *= thisFactorNormalizer;
//    }
//    var correctionFactor1 = 1.0;
//    for (i <- 0 until nodes.size) {
//      val node = nodes(i);
//      var sumOfProds = 0.0
//      for (valueIdx <- 0 until node.domain.size) {
//        var prod = 1.0;
//        for (msg <- node.receivedMessages) {
//          prod *= msg(valueIdx);
//        }
//        sumOfProds += prod;
//      }
////      println("Node " + i + ": " + sumOfProds);
//      correctionFactor1 *= Math.pow(sumOfProds, 1 - node.factors.size);
//    }
//    var correctionFactor2 = 1.0;
//    for (nodeIdx <- 0 until nodes.size) {
//      val node = nodes(nodeIdx)
//      for (factorIdx <- 0 until node.factors.size) {
//        var sumOfProds = 0.0
//        for (valueIdx <- 0 until node.domain.size) {
//          var prod = 1.0;
//          for (otherFactorIdx <- 0 until node.factors.size) {
//            if (otherFactorIdx != factorIdx) {
//              prod *= node.receivedMessages(otherFactorIdx)(valueIdx);
//            }
//          }
//          sumOfProds += prod;
//        }
////        println("Node " + nodeIdx + " leaving out factor " + factorIdx + ": " + sumOfProds);
////        approxLikelihood *= Math.pow(sumOfProds, 1 - node.factors.size);
//        correctionFactor2 *= sumOfProds;
//      }
//    }
////    println("Approx likelihood, raw: " + approxLikelihood);
////    println("Approx likelihood, correction 1: " + approxLikelihood * correctionFactor1);
////    println("Approx likelihood, correction 2: " + approxLikelihood * correctionFactor2);
////    println("Approx likelihood, correction 1+2: " + approxLikelihood * correctionFactor1 * correctionFactor2);
//    approxLikelihood * correctionFactor1 * correctionFactor2;
//  }
  
  def computeLogNormalizerApprox(nodes: Seq[Node[_]],
                                 factors: Seq[Factor]) = {
    var approxLogLikelihood = 0.0;
    for (i <- 0 until factors.size) {
      val factor = factors(i);
      val thisFactorNormalizer = factor.computeFactorNormalizer
      if (thisFactorNormalizer <= 0.0) {
        Logger.logss("Bad normalizer from factor of type " + factor.getClass.toString);
      }
//      Logger.logss("Factor " + i + " " + factor.getClass.toString + ": " + thisFactorNormalizer);
//      if (factor.isInstanceOf[UnaryFactorOld]) {
//        Logger.logss("  " + factor.asInstanceOf[UnaryFactorOld].unaryFactor.toSeq);
//      }
      approxLogLikelihood += Math.log(thisFactorNormalizer);
    }
    for (i <- 0 until nodes.size) {
      val node = nodes(i);
      var logSumOfProds = Double.NegativeInfinity;
      for (valueIdx <- 0 until node.domain.size) {
        var logProd = 0.0;
        for (msg <- node.receivedMessages) {
          logProd += Math.log(msg(valueIdx));
        }
        logSumOfProds = SloppyMath.logAdd(logSumOfProds, logProd);
      }
      if (logSumOfProds.isInfinite || logSumOfProds.isNaN) {
        Logger.logss("Bad correction factor #1 from node with arity " + node.factors.size + " and domain " + node.domain.entries.toSeq);
        Logger.logss("  logSumOfProds: " + logSumOfProds);
      }
      approxLogLikelihood += (1 - node.factors.size) * logSumOfProds;
    }
    // XXX: Can optimize these by precomputing logProd over all factors and then subtracting otherFactorIdx
    for (nodeIdx <- 0 until nodes.size) {
      val node = nodes(nodeIdx)
      for (factorIdx <- 0 until node.factors.size) {
        var logSumOfProds = Double.NegativeInfinity
        for (valueIdx <- 0 until node.domain.size) {
          var logProd = 0.0;
          for (otherFactorIdx <- 0 until node.factors.size) {
            if (otherFactorIdx != factorIdx) {
              logProd += Math.log(node.receivedMessages(otherFactorIdx)(valueIdx));
            }
          }
          logSumOfProds = SloppyMath.logAdd(logSumOfProds, logProd);
        }
        if (logSumOfProds.isInfinite || logSumOfProds.isNaN) {
          Logger.logss("Bad correction factor #2 from node with arity " + node.factors.size + " and domain " + node.domain.entries.toSeq);
          Logger.logss("  logSumOfProds: " + logSumOfProds);
        }
        approxLogLikelihood += logSumOfProds;
      }
    }
    if (approxLogLikelihood.isNaN) {
      Logger.logss("NaN likelihood from graph with " + nodes.size + " nodes, " + factors.size + " factors");
      System.exit(0);
    }
    approxLogLikelihood;
  }

  def main(args: Array[String]) {
    println("LOOPINESS EFFECTS TEST");
    {
      val node1 = new Node[Int](new Domain(Array(0, 1)));
      val node2 = new Node[Int](new Domain(Array(0, 1)));
      val f12 = new ConstantBinaryFactor(node1, node2, Array(Array(2.0, 1.0),
                                                             Array(0.1, 3.0)));
      val f12Other = new ConstantBinaryFactor(node1, node2, Array(Array(1.0, 2.0),
                                                                  Array(2.0, 1.0)));
      val sfg = new SimpleFactorGraph(Seq(node1, node2), Seq(f12, f12Other));
      sfg.runBP(10);
      println(Math.exp(sfg.computeLogNormalizerExplicit));
      println(Math.exp(sfg.computeLogNormalizerApprox));
    }
    {
      val node1 = new Node[Int](new Domain(Array(0, 1)));
      val node2 = new Node[Int](new Domain(Array(0, 1)));
      val f12Prod = new ConstantBinaryFactor(node1, node2, Array(Array(2.0, 2.0),
                                                                 Array(0.2, 3.0)));
      val sfg = new SimpleFactorGraph(Seq(node1, node2), Seq(f12Prod));
      sfg.runBP(10);
      println(Math.exp(sfg.computeLogNormalizerExplicit));
      println(Math.exp(sfg.computeLogNormalizerApprox));
    }
    println("SMALL TEST CASES");
    {
      val node1 = new Node[Int](new Domain(Array(0, 1)));
      val node2 = new Node[Int](new Domain(Array(0, 1)));
      val f1 = new ConstantUnaryFactor(node1, Array(1.0, 2.0));
      val f12 = new ConstantBinaryFactor(node1, node2, Array(Array(2.0, 1.0),
                                                             Array(0.1, 3.0)));
      val f2 = new ConstantUnaryFactor(node2, Array(1.0, 1.0));
      val sfg = new SimpleFactorGraph(Seq(node1, node2), Seq(f1, f12, f2));
      sfg.runBP(10);
      println(Math.exp(sfg.computeLogNormalizerExplicit));
      println(Math.exp(sfg.computeLogNormalizerApprox));
    }
    println("==================");
    {
      val node1 = new Node[Int](new Domain(Array(0, 1)));
      val f1 = new ConstantUnaryFactor(node1, Array(1.0, 2.0));
      val f2 = new ConstantUnaryFactor(node1, Array(3.0, 1.0));
      val f3 = new ConstantUnaryFactor(node1, Array(5.0, 2.0));
      val sfg = new SimpleFactorGraph(Seq(node1), Seq(f1, f2, f3));
      sfg.runBP(10);
      println(Math.exp(sfg.computeLogNormalizerExplicit));
      println(Math.exp(sfg.computeLogNormalizerApprox));
    }
    println("==================");
    // Triangle with one more node hanging off
    {
      val node1 = new Node[Int](new Domain(Array(0, 1)));
      val node2 = new Node[Int](new Domain(Array(0, 1)));
      val node3 = new Node[Int](new Domain(Array(0, 1)));
      val node4 = new Node[Int](new Domain(Array(0, 1)));
      val f1 = new ConstantUnaryFactor(node1, Array(1.0, 2.0));
      val f2 = new ConstantUnaryFactor(node2, Array(2.0, 1.0));
      val f3 = new ConstantUnaryFactor(node3, Array(1.0, 1.0));
      val f4 = new ConstantUnaryFactor(node4, Array(1.0, 1.0));
      
      val f12 = new ConstantBinaryFactor(node1, node2, Array(Array(2.0, 1.0),
                                                             Array(0.1, 3.0)));
      val f31 = new ConstantBinaryFactor(node3, node1, Array(Array(1.1, 0.9),
                                                             Array(0.9, 1.1)));
      val f234 = new ConstantTernaryFactor(Seq(node2, node3, node4),
                                           Array(Array(Array(2.0, 0.5),
                                                       Array(0.4, 1.2)),
                                                 Array(Array(2.3, 1.8),
                                                       Array(3.0, 1.2))));
      
      val sfg = new SimpleFactorGraph(Seq(node1, node2, node3, node4), Seq(f1, f2, f3, f4, f12, f31, f234));
      sfg.runBP(20);
      println(Math.exp(sfg.computeLogNormalizerExplicit));
      println(Math.exp(sfg.computeLogNormalizerApprox));
    }
  }
}
