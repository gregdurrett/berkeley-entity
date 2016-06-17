package edu.berkeley.nlp.entity

import java.util.IdentityHashMap
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.collectionAsScalaIterableConverter
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.futile.fig.basic.Pair
import edu.berkeley.nlp.futile.ling.HeadFinder
import edu.berkeley.nlp.futile.syntax.Trees.PennTreeRenderer
import edu.berkeley.nlp.futile.syntax.Tree
import edu.berkeley.nlp.futile.util.Logger
import scala.collection.mutable.HashSet
import scala.collection.JavaConverters._
import java.util.Collections
import edu.berkeley.nlp.entity.lang.ModCollinsHeadFinder
import edu.berkeley.nlp.futile.ling.CollinsHeadFinder

@SerialVersionUID(1L)
class DepConstTree(val constTree: Tree[String],
                   val pos: Seq[String],
                   val words: Seq[String],
                   val childParentDepMap: HashMap[Int,Int]) extends Serializable {
  require(childParentDepMap.keys.toSeq.sorted.sameElements((0 until words.size)), PennTreeRenderer.render(constTree));
  
  def size = words.size;
  
  override def toString() = {
    var strRep = "";
    strRep += PennTreeRenderer.render(constTree) + "\n";
    for (i <- 0 until words.size) {
      val headIdx = childParentDepMap(i);
      strRep += words(i) + "(" + i + "), head = " + (if (headIdx == -1) "ROOT" else words(headIdx) + "(" + headIdx + ")") + "\n";
    }
    strRep;
  }
  
  /**
   * Fetches the head for an arbitrary span; this is needed for head-finding of mentions
   * that might not be constituents.
   */
  def getSpanHead(startIdx: Int, endIdx: Int) = DepConstTree.getSpanHead(childParentDepMap, startIdx, endIdx);
  
  def getSpanHeadACECustom(startIdx: Int, endIdx: Int) = DepConstTree.getSpanHeadACECustom(childParentDepMap, pos, startIdx, endIdx);
  
  /**
   * Usually returns the head of the span, unless it's an NP or NML that dominates
   * N* CC N* or [N* ,]+ N* , CC N* (coordination cases). Then we return the head
   * of each coordinated thing.
   * 
   * Note: English-specific
   */
  def getSpanHeadOrNPCoordinatedHeads(startIdx: Int, endIdx: Int): Set[Int] = DepConstTree.getSpanHeadOrNPCoordinatedHeads(constTree, childParentDepMap, startIdx, endIdx);
  
  def isConstituent(start: Int, end: Int) = {
    val spans = constTree.getSpanMap();
    spans.containsKey(Pair.makePair(new Integer(start), new Integer(end)));
  }
  
  def doesCrossBrackets(start: Int, end: Int) = {
    val spans = constTree.getSpanMap();
    var crossesBrackets = false;
    for (span <- spans.keySet().asScala) {
      val spanStart = span.getFirst.intValue();
      val spanEnd = span.getSecond.intValue();
      crossesBrackets = crossesBrackets || ((start < spanStart && end > spanStart && end < spanEnd) || (start > spanStart && start < spanEnd && end > spanEnd));
    }
    crossesBrackets;
  }
  
  def getConstituentType(start: Int, end: Int) = {
    val spans = constTree.getSpanMap();
    val pair = Pair.makePair(new Integer(start), new Integer(end));
    if (spans.containsKey(pair) && !spans.get(pair).isEmpty()) {
      spans.get(pair).get(0).getLabel();
    } else {
      "";
    }
  }
  
  def getAllConstituentTypes(start: Int, end: Int): Seq[String] = {
    val spans = constTree.getSpanMap();
    val pair = Pair.makePair(new Integer(start), new Integer(end));
    if (spans.containsKey(pair) && !spans.get(pair).isEmpty()) {
      spans.get(pair).asScala.map(_.getLabel());
    } else {
      Seq[String]();
    }
  }
  
  // XXX: This is broken in some subtle way
//  def cCommand(commanderStart: Int, commanderEnd: Int, commandeeStart: Int, commandeeEnd: Int): String = {
//    val spans = constTree.getSpanMap();
//    if (spans.containsKey(fig.basic.Pair.makePair(new Integer(commanderStart), new Integer(commanderEnd - 1)))) {
//      "UNKNOWN"
//    } else {
//      // Find the smallest span properly containing this one
//      var parentStart = -1;
//      var parentEnd = constTree.size() + 1;
//      for (span <- spans.keySet.asScala) {
//        val thisStart = span.getFirst.intValue;
//        val thisEnd = span.getSecond.intValue + 1;
//        val containsProperly = thisStart <= commanderStart && commanderEnd <= thisEnd && (thisStart != commanderStart || commanderEnd != thisEnd);
//        if (containsProperly && thisStart >= parentStart && thisEnd <= parentEnd) {
//          parentStart = thisStart;
//          parentEnd = thisEnd;
//        }
//      }
//      require(parentStart != -1 && parentEnd != constTree.size() + 1);
//      if (parentStart <= commandeeStart && commandeeEnd <= parentEnd) {
//        "TRUE";
//      } else {
//        "FALSE";
//      }
//    }
//  }
  
  def getSpansAndHeadsOfType(constituentType: String): Seq[(Int, Int, Int)] = {
    val results = new ArrayBuffer[(Int, Int, Int)];
    for (constituent <- constTree.getConstituentCollection().asScala) {
      if (constituent.getLabel() == constituentType) {
        results += new Tuple3(constituent.getStart(), constituent.getEnd() + 1, getSpanHead(constituent.getStart(), constituent.getEnd() + 1));
      }
    }
    results;
  }
  
  def getSpansAndCoordinatedHeadsOfType(constituentType: String): Seq[(Int, Int, Set[Int])] = {
    val results = new ArrayBuffer[(Int, Int, Set[Int])];
    for (constituent <- constTree.getConstituentCollection().asScala) {
      if (constituent.getLabel() == constituentType) {
        results += new Tuple3(constituent.getStart(), constituent.getEnd() + 1, getSpanHeadOrNPCoordinatedHeads(constituent.getStart(), constituent.getEnd() + 1));
      }
    }
    results;
  }
  
  def computeSyntacticUnigram(headIdx: Int): String = {
    val parentIdx: Int = if (childParentDepMap.contains(headIdx)) childParentDepMap(headIdx) else -1;
    val parentStr = if (parentIdx == -1) {
      "NULL";
    } else {
      pos(parentIdx) + "(" + (if (headIdx > parentIdx) "L" else "R") + ")"; 
    }
    parentStr;
  }
  
  def computeSyntacticBigram(headIdx: Int): String = {
    val parentIdx: Int = if (childParentDepMap.contains(headIdx)) childParentDepMap(headIdx) else -1;
    val grandparentIdx: Int = if (parentIdx != -1 && childParentDepMap.contains(parentIdx)) childParentDepMap(parentIdx) else -1;
    val parentStr = if (parentIdx == -1) {
      "NULL";
    } else {
      pos(parentIdx) + "(" + (if (headIdx > parentIdx) "L" else "R") + ")"; 
    }
    val grandparentStr = if (grandparentIdx == -1) {
      "NULL";
    } else {
      pos(grandparentIdx) + "(" + (if (parentIdx > grandparentIdx) "L" else "R") + ")"; 
    }
    parentStr + "-" + grandparentStr;
  }
  
  def computeSyntacticPositionSimple(headIdx: Int): String = {
    val parentIdx: Int = if (childParentDepMap.contains(headIdx)) childParentDepMap(headIdx) else -1;
    val grandparentIdx: Int = if (parentIdx != -1 && childParentDepMap.contains(parentIdx)) childParentDepMap(parentIdx) else -1;
    if (parentIdx != -1 && pos(parentIdx).startsWith("V") && headIdx < parentIdx) {
      "SUBJECT";
    } else if (parentIdx != -1 && pos(parentIdx).startsWith("V") && headIdx > parentIdx) {
      "DIROBJ";
    } else if (parentIdx != -1 && grandparentIdx != -1 && (pos(parentIdx) == "IN" || pos(parentIdx) == "TO") && pos(grandparentIdx).startsWith("V")) {
      "INDIROBJ";
    } else {
      "OTHER";
    }
  }
  
  def computeDependencyPath(startIdx: Int, endIdx: Int): (Seq[String], Seq[String]) = {
    val startParents = getAllParentIndices(startIdx);
    val endParents = getAllParentIndices(endIdx);
    var spIndex = -1;
    for (i <- 0 until startParents.size) {
      if (spIndex == -1 && endParents.contains(startParents(i))) {
        spIndex = i;
      }
    }
    val epIndex = endParents.indexOf(startParents(spIndex));
    (startParents.slice(0, spIndex+1).map(pos(_)), endParents.slice(0, epIndex+1).reverse.map(pos(_)));
  }
  
  def isChild(child: Int, parent: Int) = childParentDepMap(child) == parent;
  
  def getAllParentIndices(idx: Int): Seq[Int] = {
    var parents = new ArrayBuffer[Int];
    var currentParent = idx;
    while (currentParent != -1) {
      parents += currentParent;
      currentParent = childParentDepMap(currentParent);
    }
    parents;
  }
  
  def getAllChildrenIndices(idx: Int): IndexedSeq[Int] = {
    val children = new ArrayBuffer[Int];
    var i = 0;
    while (i < size) {
      if (childParentDepMap(i) == idx) {
        children += i;
      }
      i += 1;
    }
    children;
  }
}

object DepConstTree {
  
  val hfEnglish = new ModCollinsHeadFinder();
  
  /**
   * Fetches the head for an arbitrary span; this is needed for head-finding of mentions
   * that might not be constituents.
   */
  def getSpanHead(childParentDepMap: HashMap[Int,Int], startIdx: Int, endIdx: Int) = {
    // If it's a constituent, only one should have a head outside
    val outsidePointing = new ArrayBuffer[Int];
    for (i <- startIdx until endIdx) {
      val ptr = childParentDepMap(i);
      if (ptr < startIdx || ptr >= endIdx) {
        outsidePointing += i;
      }
    }
    // If our heuristic failed to identify anything, assume head final
    if (outsidePointing.isEmpty) {
      Logger.logss("WARNING: Empty outside pointing " + startIdx + ", " + endIdx + ": " + childParentDepMap);
      endIdx - 1;
    } else {
      outsidePointing.last;
    }
  }
  
  /**
   * Fetches the head for a span where you know it's a nominal but the parser might be wrong; basically
   * corrects for the problem of the span actually being (NP ...) (PP ...) and the head of the PP being
   * taken as the head of the whole span
   */
  def getSpanHeadACECustom(childParentDepMap: HashMap[Int,Int], pos: Seq[String], startIdx: Int, endIdx: Int) = {
    // If it's a constituent, only one should have a head outside
    val outsidePointing = new ArrayBuffer[Int];
    for (i <- startIdx until endIdx) {
      val ptr = childParentDepMap(i);
      if (ptr < startIdx || ptr >= endIdx) {
        outsidePointing += i;
      }
    }
    // If our heuristic failed to identify anything, assume head final
    if (outsidePointing.isEmpty) {
      Logger.logss("WARNING: Empty outside pointing " + startIdx + ", " + endIdx + ": " + childParentDepMap);
      endIdx - 1;
    } else {
      // Take the last nominal head
      val headsAndTags = outsidePointing.map(idx => idx -> pos(idx));
      val nominalHeadsAndTags = headsAndTags.filter(_._2.startsWith("N"));
      if (!nominalHeadsAndTags.isEmpty) {
        nominalHeadsAndTags.last._1;
      } else {
        outsidePointing.last;
      }
    }
  }
  
  /**
   * Usually returns the head of the span, unless it's an NP or NML that dominates
   * N* CC N* or [N* ,]+ N* , CC N* (coordination cases). Then we return the head
   * of each coordinated thing.
   */
  def getSpanHeadOrNPCoordinatedHeads(constTree: Tree[String], childParentDepMap: HashMap[Int,Int], startIdx: Int, endIdx: Int): Set[Int] = {
    val spanMap = constTree.getSpanMap();
    val pair = Pair.makePair(new Integer(startIdx), new Integer(endIdx));
    val coordinatedHeads = new HashSet[Int];
    if (spanMap.containsKey(pair)) {
      val nonUnaryConstituents = spanMap.get(pair).asScala.filter(_.getChildren().size() > 1);
      if (nonUnaryConstituents.size == 1 && nonUnaryConstituents.head.getLabel.startsWith("N")) {
        val children = nonUnaryConstituents.head.getChildren;
        if (children.size > 1) {
          val childLabels = children.asScala.map(_.getLabel);
          if (childLabels.contains("CC")) {
            val childSizes = children.asScala.map(_.getYield.size);
            if (childLabels.size == 3 && childLabels(0).startsWith("N") && childLabels(2).startsWith("N")) {
              // Case 1: N* CC N*
              coordinatedHeads += getSpanHead(childParentDepMap, startIdx, startIdx + childSizes(0));
              coordinatedHeads += getSpanHead(childParentDepMap, endIdx - childSizes(2), endIdx);
            } else if (childLabels.size >= 6 && childLabels.size % 2 == 0) {
              // Case 2: [N* ,]+ N* , CC N*
              var isNsAndCommas = true;
              for (i <- 0 until childLabels.size - 2 by 2) {
                isNsAndCommas = isNsAndCommas && childLabels(i).startsWith("N") && childLabels(i+1) == ",";
              }
              isNsAndCommas = isNsAndCommas && childLabels(childLabels.size - 2) == "CC" && childLabels(childLabels.size - 1).startsWith("N");
              if (isNsAndCommas) {
                var currStart = startIdx;
                for (i <- 0 until childLabels.size - 2 by 2) {
                  coordinatedHeads += getSpanHead(childParentDepMap, currStart, currStart + childSizes(i));
                  currStart += childSizes(i) + childSizes(i + 1);
                }
                // Add the last child
                coordinatedHeads += getSpanHead(childParentDepMap, endIdx - childSizes(childSizes.size - 1), endIdx);
              }
            }
          }
        }
      }
    }
    if (coordinatedHeads.size >= 1) {
      coordinatedHeads.toSet;
    } else {
      Set(getSpanHead(childParentDepMap, startIdx, endIdx));
    }
  }
  
  def extractDependencyStructure(constTree: Tree[String], headFinder: HeadFinder): HashMap[Int, Int] = {
    // Type created by this method is an IdentityHashMap, which is correct
    // N.B. The constituent end index is the last word of the mention, it's not on fenceposts
    val constituents = constTree.getConstituents()
    val subtreeHeads = new IdentityHashMap[Tree[String],Int];
    val trees = constTree.getPostOrderTraversal().asScala;
    require(trees.last eq constTree);
    val heads = new HashMap[Int,Int]();
    for (tree <- trees) {
      if (tree.isLeaf) {
        // Do nothing
      } else if (tree.isPreTerminal) {
        val constituent = constituents.get(tree);
        require(!subtreeHeads.containsKey(tree));
        subtreeHeads.put(tree, constituent.getStart());
      } else {
        val children = tree.getChildren();
        val head = headFinder.determineHead(tree);
        if (head == null) {
          Logger.logss("WARNING: null head: " + PennTreeRenderer.render(constTree) + "\n" + PennTreeRenderer.render(tree));
        }
        val headIdx = subtreeHeads.get(head);
        for (child <- children.asScala) {
          if (child eq head) {
            subtreeHeads.put(tree, headIdx);
          } else {
            require(!heads.contains(subtreeHeads.get(child)), "\n" + PennTreeRenderer.render(constTree) +
                      "\n" + PennTreeRenderer.render(tree) +
                      "\n" + PennTreeRenderer.render(child) +
                      "\n" + heads);
            heads(subtreeHeads.get(child)) = headIdx;
          }
        }
      }
    }
    // Set the root head
    heads(subtreeHeads.get(constTree)) = -1;
    val numLeaves = constTree.getYield.size();
    for (i <- 0 until numLeaves) {
      require(heads.contains(i), heads + "\n" + PennTreeRenderer.render(constTree));
    }
    heads;
  }
  
  def apply(tree: Tree[String]) = {
    new DepConstTree(tree, tree.getPreTerminalYield().asScala, tree.getYield().asScala, extractDependencyStructure(tree, hfEnglish));
  }
  
  
  def main(args: Array[String]) {
    // Can't have duplicate substructures
    val childComma1 = new Tree[String](",", Collections.singletonList(new Tree[String](",")));
    val childComma2 = new Tree[String](",", Collections.singletonList(new Tree[String](",")));
    val childComma3 = new Tree[String](",", Collections.singletonList(new Tree[String](",")));
    val childAnd = new Tree[String]("CC", Collections.singletonList(new Tree[String]("CC")));
    val child1 = new Tree[String]("NN", Collections.singletonList(new Tree[String]("bacon")));
    val child2 = new Tree[String]("NN", Collections.singletonList(new Tree[String]("sausage")));
    val child3 = new Tree[String]("NNS", Collections.singletonList(new Tree[String]("eggs")));
    val child4 = new Tree[String]("NN", Collections.singletonList(new Tree[String]("cheese")));
    
    val goodExampleType1 = new Tree[String]("NP", IndexedSeq(child1, childAnd, child2).asJava);
    val goodExampleType2 = new Tree[String]("NP", IndexedSeq(child1, childComma1, child2, childComma2, childAnd, child3).asJava);
    val goodExampleType2a = new Tree[String]("NP", IndexedSeq(child1, childComma1, child2, childComma2, child3, childComma3, childAnd, child4).asJava);
    val badExample = new Tree[String]("NP", IndexedSeq(child1, childComma1, child2, childComma2).asJava);
    
    test(goodExampleType1);
    test(goodExampleType2);
    test(goodExampleType2a);
    test(badExample);
  }
  
  private def test(tree: Tree[String]) {
    val hf = new ModCollinsHeadFinder();
    val ds = extractDependencyStructure(tree, hf);
    println(getSpanHeadOrNPCoordinatedHeads(tree, ds, 0, tree.getYield().size));
  }
}
