package edu.berkeley.nlp.entity.coref
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.futile.util.Logger
import scala.collection.JavaConverters._
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.entity.sem.QueryCountsBundle
import edu.berkeley.nlp.entity.sem.MentionFilter
import edu.mit.jwi.item.Pointer
import edu.berkeley.nlp.entity.sem.FancyHeadMatcher
import edu.berkeley.nlp.entity.sem.AbbreviationHandler
import edu.berkeley.nlp.entity.Driver
import edu.berkeley.nlp.entity.WordNetInterfacer
import edu.berkeley.nlp.entity.GUtil
import scala.collection.mutable.HashMap

object CorefEvaluator {
  
  def evaluateAndRenderShort(docGraphs: Seq[DocumentGraph],
                             allPredBackptrs: Array[Array[Int]],
                             allPredClusterings: Seq[OrderedClustering],
                             strPrefix: String): String = {
    // Fraction of guys whose backpointers point to something in the correct cluster
    // Needed for the final eval
    val (accuracy, accuracyStr) = computeRenderAccuracy(docGraphs, allPredBackptrs);
    var rendered = accuracyStr;
    val (pairwiseF1, pairwiseF1Str) = computeRenderPairwisePRF1(docGraphs, allPredBackptrs, allPredClusterings);
    rendered += "\n" + pairwiseF1Str;
    rendered += "\n" + computeLinkageStats(docGraphs, allPredBackptrs);
    val renderedGreppable = strPrefix + rendered.replaceAll("\n", "\n" + strPrefix);
    renderedGreppable;
  }

  def evaluateAndRender(docGraphs: Seq[DocumentGraph],
                        allPredBackptrs: Seq[Array[Int]],
                        allPredClusterings: Seq[OrderedClustering],
                        conllEvalScriptPath: String,
                        strPrefix: String,
                        analysesToPrint: String): String = {
    // Fraction of guys whose backpointers point to something in the correct cluster
    // Needed for the final eval
    val (accuracy, accuracyStr) = computeRenderAccuracy(docGraphs, allPredBackptrs);
    var rendered = accuracyStr;
    rendered += "\n" + computeRenderSubdivisionAnalysis(docGraphs, allPredBackptrs);
    rendered += "\n" + computeRenderAccuracyUnaryProperty(docGraphs, allPredBackptrs, MentionFilter.isPronominal, "PRONOUN")
    rendered += "\n" + computeRenderAccuracyUnaryProperty(docGraphs, allPredBackptrs, MentionFilter.isCanonicalHardReferringCase, "REFERRING")
    rendered += "\n" + computeRenderAccuracyUnaryProperty(docGraphs, allPredBackptrs, MentionFilter.isCanonicalProperNominalCase, "PROPER->NOMINAL")
    rendered += "\n" + computeRenderAccuracyUnaryProperty(docGraphs, allPredBackptrs, MentionFilter.isCanonicalStrictHeadContainedCase, "HEAD CONTAINED")
    rendered += "\n" + computeRenderAccuracyUnaryProperty(docGraphs, allPredBackptrs, MentionFilter.unaryIsCanonicalHardestReferringCase, "HARDEST")
    rendered += "\n" + computeRenderAccuracyUnaryProperty(docGraphs, allPredBackptrs, MentionFilter.unaryIsNoHarderThanHeadMatch, "HEAD MATCH/SING")
    rendered += "\n" + computeRenderAccuracyBinaryProperty(docGraphs, allPredBackptrs, MentionFilter.binaryIsReferringHeadContainedCase, "HEAD CONTAINED")
    val (pairwiseF1, pairwiseF1Str) = computeRenderPairwisePRF1(docGraphs, allPredBackptrs, allPredClusterings);
    rendered += "\n" + pairwiseF1Str;
    rendered += "\n" + computeLinkageStats(docGraphs, allPredBackptrs);
    // PRUNING ANALYSIS
    if (analysesToPrint.contains("+oracle")) {
      rendered += "\nPRUNING/ORACLE ANALYSIS:\n" + evaluatePruning(docGraphs, allPredBackptrs, allPredClusterings, conllEvalScriptPath)
    }
    if (analysesToPrint.contains("+limitedoracle")) {
      {
        rendered += "\nLIMITED ORACLE ANALYSIS SOLVED BY SIGNAL";
        val filter = (docGraph: DocumentGraph, idx: Int) => MentionFilter.unaryIsHardReferringAndSomeSatisfiesSignal(docGraph, idx, WordNetInterfacer.satisfiesWordNetSignal);
        val allFixedBackptrs = applyLimitedOracleToBackptrs(docGraphs, allPredBackptrs, filter);
        val allFixedClusterings = allFixedBackptrs.map(OrderedClustering.createFromBackpointers(_));
        rendered += "\n" + computeRenderCoNLL(docGraphs, allFixedClusterings, conllEvalScriptPath)
      }
    }
    // FINAL SCORING
    rendered += "\nFINAL SCORE:";
    if (pairwiseF1 > 0.20) {
      rendered += "\n" + computeRenderCoNLL(docGraphs, allPredClusterings, conllEvalScriptPath)
      if (Driver.printSigSuffStats) {
        rendered += "\nBootstrap stats";
        rendered += "\n" + computeRenderCoNLLIndividual(docGraphs, allPredClusterings, conllEvalScriptPath);
      }
    } else {
      rendered += "\n" + "Too crappy to run CoNLL scorer: " + pairwiseF1;
    }
    val renderedGreppable = strPrefix + rendered.replaceAll("\n", "\n" + strPrefix);
    renderedGreppable;
  }
  
  def evaluatePruning(docGraphs: Seq[DocumentGraph],
                      allPredBackptrs: Seq[Array[Int]],
                      allPredClusterings: Seq[OrderedClustering],
                      conllEvalScriptPath: String): String = {
    var rendered = "";
    var trivialCorrect = new Array[Array[Int]](2);
    (0 until trivialCorrect.size).foreach(i => trivialCorrect(i) = Array.fill(3)(0));
    var trivialIncorrect = new Array[Array[Int]](2);
    (0 until trivialIncorrect.size).foreach(i => trivialIncorrect(i) = Array.fill(3)(0));
    var nontrivial = new Array[Array[Int]](2);
    (0 until nontrivial.size).foreach(i => nontrivial(i) = Array.fill(3)(0));
    // Evaluate oracle
    val allOracleBackptrs = new Array[Array[Int]](docGraphs.size);
    val allOracleClusterings = new Array[OrderedClustering](docGraphs.size);
    for (i <- 0 until docGraphs.size) {
      val docGraph = docGraphs(i);
      // Fix up to make oracle, count number of times there's a nontrivial decision
      val doc = docGraphs(i);
      val predBackptrs = allPredBackptrs(i)
      val oracleBackptrs = new Array[Int](doc.size)
      for (j <- 0 until oracleBackptrs.size) {
        val allAntecedents = doc.getAllAntecedentsCurrentPruning(j);
        val goldAntecedents = doc.getGoldAntecedentsUnderCurrentPruningOrEmptySet(j);
        val goldAntecedentsUnpruned = doc.getGoldAntecedentsNoPruning(j);
        require(goldAntecedentsUnpruned.size >= 1);
        val anaphoricIdx = if (goldAntecedentsUnpruned(0) != j) 0 else 1;
        val mentType = docGraph.getMention(j).mentionType;
        val mentTypeIdx = if (mentType == MentionType.PRONOMINAL) 0 else if (mentType == MentionType.NOMINAL) 1 else 2
        // If the gold has been ruled out, use the guess
        if (goldAntecedents.size == 0) {
          trivialIncorrect(anaphoricIdx)(mentTypeIdx) += 1;
          oracleBackptrs(j) = predBackptrs(j)
        } else { // Otherwise oracle is the gold
          if (allAntecedents.size == goldAntecedents.size) {
            trivialCorrect(anaphoricIdx)(mentTypeIdx) += 1;
          } else {
            nontrivial(anaphoricIdx)(mentTypeIdx) += 1;
          }
          oracleBackptrs(j) = goldAntecedents.last;
        }
      }
      allOracleBackptrs(i) = oracleBackptrs;
      allOracleClusterings(i) = OrderedClustering.createFromBackpointers(oracleBackptrs);
    }
    for (i <- 0 until 2) {
      val anaphoric = if (i == 0) "Anaphoric" else "Nonanaphoric";
      for (j <- 0 until 3) {
        val mentType = if (j == 0) "pronominal" else if (j == 1) "nominal" else "proper";
        rendered += "\n" + anaphoric + " " + mentType + " trivial correct / trivial incorrect / nontrivial: " + trivialCorrect(i)(j) + " / " + trivialIncorrect(i)(j) + " / " + nontrivial(i)(j)
      }
    }
    val (accuracy, accuracyStr) = computeRenderAccuracy(docGraphs, allOracleBackptrs);
    rendered += "\nOracle " + accuracyStr;
    val (pairwiseF1, pairwiseF1Str) = computeRenderPairwisePRF1(docGraphs, allOracleBackptrs, allOracleClusterings);
    rendered += "\nOracle " + pairwiseF1Str;
    if (pairwiseF1 > 0.20) {
      rendered += "\nOracle " + computeRenderCoNLL(docGraphs, allOracleClusterings, conllEvalScriptPath)
    } else {
      rendered += "\nToo crappy to run CoNLL scorer: " + pairwiseF1;
    }
    rendered;
  }
  
  
  def computeRenderAccuracyUnaryProperty(docGraphs: Seq[DocumentGraph],
                                         allPredBackptrs: Seq[Array[Int]],
                                         criterion: (DocumentGraph, Int) => Boolean,
                                         descriptor: String): String = {
    var numConsidered = 0;
    var numConsideredCorrect = 0;
    for (i <- 0 until docGraphs.size) {
      val docGraph = docGraphs(i);
      for (mentIdx <- 0 until docGraph.size) {
        val matchesCriteria = criterion(docGraph, mentIdx);
        val incorrect = MentionFilter.isIncorrect(docGraph, mentIdx, allPredBackptrs(i)(mentIdx));
        if (matchesCriteria) {
          numConsidered += 1;
          if (!incorrect) {
            numConsideredCorrect += 1;
          }
        }
      }
    }
    "Accuracy of " + descriptor + ": " + GUtil.renderNumerDenom(numConsideredCorrect.toDouble, numConsidered.toDouble);
  }
  
  def computeRenderAccuracyBinaryProperty(docGraphs: Seq[DocumentGraph],
                                          allPredBackptrs: Seq[Array[Int]],
                                          criterion: (DocumentGraph, Int, Int) => Boolean,
                                          descriptor: String): String = {
    var numConsidered = 0;
    var numConsideredCorrect = 0;
    var numArcsCorrect = 0;
    var numTotalArcs = 0;
    for (i <- 0 until docGraphs.size) {
      val docGraph = docGraphs(i);
      for (mentIdx <- 0 until docGraph.size) {
        val goldAntecedents = docGraphs(i).getGoldAntecedentsUnderCurrentPruning(mentIdx);
        var oneMatchesCriteria = false;
        for (antIdx <- 0 until mentIdx) {
          val matchesCriteria = criterion(docGraph, mentIdx, antIdx);
          oneMatchesCriteria = oneMatchesCriteria || matchesCriteria;
          val incorrect = MentionFilter.isIncorrect(docGraph, mentIdx, antIdx);
          if (matchesCriteria) {
            numTotalArcs += 1;
            if (!incorrect) {
              numArcsCorrect += 1;
            }
          }
        }
        if (oneMatchesCriteria) {
          numConsidered += 1;
          if (goldAntecedents.contains(allPredBackptrs(i)(mentIdx))) {
            numConsideredCorrect += 1;
          }
        }
      }
    }
    descriptor + " precision: " + GUtil.renderNumerDenom(numArcsCorrect.toDouble, numTotalArcs.toDouble) + "; accuracy: " + GUtil.renderNumerDenom(numConsideredCorrect.toDouble, numConsidered.toDouble);
  }
  
  // Returns accuracy and string rep
  def computeRenderAccuracy(docGraphs: Seq[DocumentGraph],
                            allPredBackptrs: Seq[Array[Int]]): (Double, String) = {
    var numCorrect = 0;
    var numTotal = 0;
    var correct = new Array[Array[Int]](2);
    (0 until correct.size).foreach(i => correct(i) = Array.fill(3)(0));
    var total = new Array[Array[Int]](2);
    (0 until total.size).foreach(i => total(i) = Array.fill(3)(0));
    var numCorrectInformedProns = 0;
    var numTotalInformedProns = 0;
    var numCorrectBridging = 0;
    var numTotalBridging = 0;
    var numCorrectNomMisleadingHM = 0;
    var numTotalNomMisleadingHM = 0;
    val topNomMisleadingHM = new Counter[String]();
    var numCorrectPropMisleadingHM = 0;
    var numTotalPropMisleadingHM = 0;
    val topPropMisleadingHM = new Counter[String]();
    for (docIdx <- 0 until docGraphs.size) {
      val docGraph = docGraphs(docIdx);
      for (i <- 0 until docGraph.size) {
        // Accuracy
        val predBackptr = allPredBackptrs(docIdx)(i);
        val goldBackptrs = docGraph.getGoldAntecedentsNoPruning(i);
        numTotal += 1;
        val anaphoricIdx = if (goldBackptrs(0) != i) 0 else 1;
        val mentType = docGraph.getMention(i).mentionType;
        val mentTypeIdx = if (mentType == MentionType.PRONOMINAL) 0 else if (mentType == MentionType.NOMINAL) 1 else 2
        val isCorrect = goldBackptrs.contains(predBackptr);
        if (isCorrect) {
          numCorrect += 1;
          correct(anaphoricIdx)(mentTypeIdx) += 1;
        }
        // Bridging anaphora
        if (mentType == MentionType.NOMINAL || mentType == MentionType.PROPER) {
          val goldAntecedents = docGraph.getGoldAntecedentsNoPruning(i);
          val previousMents = goldAntecedents.map(docGraph.getMention(_));
          val hasPreviousNominalProper = previousMents.foldLeft(false)((has, ment) => has || ment.mentionType != MentionType.PRONOMINAL);
          val hasHeadMatch = previousMents.foldLeft(false)((has, ment) => has || ment.headStringLc.equals(docGraph.getMention(i).headStringLc));
          if (hasPreviousNominalProper && !hasHeadMatch) {
            if (isCorrect) {
              numCorrectBridging += 1;
            }
            numTotalBridging += 1;
          }
        }
        // Misleading head matches, all
        if (mentType == MentionType.NOMINAL || mentType == MentionType.PROPER) {
          val goldAntecedents = docGraph.getGoldAntecedentsNoPruning(i);
          val mentHead = docGraph.getMention(i).headStringLc;
          var misleadingHeadMatch = false;
          for (j <- 0 until i) {
            if (!goldAntecedents.contains(j) && docGraph.getMention(j).headStringLc == mentHead) {
              misleadingHeadMatch = true;
              if (mentType == MentionType.NOMINAL) topNomMisleadingHM.incrementCount(mentHead, 1.0) else topPropMisleadingHM.incrementCount(mentHead, 1.0);
            }
          }
          if (misleadingHeadMatch) {
            if (isCorrect) {
              if (mentType == MentionType.NOMINAL) numCorrectNomMisleadingHM += 1 else numCorrectPropMisleadingHM += 1;
            }
            if (mentType == MentionType.NOMINAL) numTotalNomMisleadingHM += 1 else numTotalPropMisleadingHM += 1;
          }
        }
        total(anaphoricIdx)(mentTypeIdx) += 1;
      }
    }
    topNomMisleadingHM.keepTopNKeys(20);
    topPropMisleadingHM.keepTopNKeys(20);
    val accuracy = numCorrect.toDouble / numTotal.toDouble;
    val accStr: (Int, Int) => String = (i, j) => correct(i)(j) + "/" + total(i)(j) + " = " + correct(i)(j).toDouble/total(i)(j).toDouble;
    (accuracy, "Accuracy: " + numCorrect + "/" + numTotal + " = " + accuracy +
        "\nAnaphoric pronominals " + accStr(0, 0) + ", Anaphoric nominals " + accStr(0, 1) + ", Anaphoric propers " + accStr(0, 2) +
        "\nNonanaphoric pronominals " + accStr(1, 0) + ", Nonanaphoric nominals " + accStr(1, 1) + ", Nonanaphoric propers " + accStr(1, 2) +
        "\nInformed pronouns " + numCorrectInformedProns + "/" + numTotalInformedProns + " = " + numCorrectInformedProns.toDouble/numTotalInformedProns.toDouble +
        "\nMisleading headmatch: nominal: " + numCorrectNomMisleadingHM + "/" + numTotalNomMisleadingHM + " = " + numCorrectNomMisleadingHM.toDouble/numTotalNomMisleadingHM.toDouble + ", top misleading: " + topNomMisleadingHM +
        "\n                      proper: " + numCorrectPropMisleadingHM + "/" + numTotalPropMisleadingHM + " = " + numCorrectPropMisleadingHM.toDouble/numTotalPropMisleadingHM.toDouble + ", top misleading: " + topPropMisleadingHM +
        "\nQuasi-bridging anaphora " + numCorrectBridging + "/" + numTotalBridging + " = " + numCorrectBridging.toDouble/numTotalBridging.toDouble);
  }
  
  def computeRenderSubdivisionAnalysis(docGraphs: Seq[DocumentGraph],
                                       allPredBackptrs: Seq[Array[Int]]): String = {
    val correctCounter = new Counter[Seq[String]];
    val totalCounter = new Counter[Seq[String]];
    for (docIdx <- 0 until docGraphs.size) {
      val docGraph = docGraphs(docIdx);
      for (i <- 0 until docGraph.size) {
        // Is it correct?
        val predBackptr = allPredBackptrs(docIdx)(i);
        val goldBackptrs = docGraph.getGoldAntecedentsNoPruning(i);
        val isCorrect = goldBackptrs.contains(predBackptr);
        val properties = new ArrayBuffer[String]();
        // Identify anaphoric vs. starting new vs. singleton
        val goldClustering = docGraph.getOraclePredClustering();
        val antecedents = goldClustering.getAllAntecedents(i);
        val consequents = goldClustering.getAllConsequents(i);
        val mentType = (if (docGraph.getMention(i).mentionType == MentionType.PRONOMINAL) "pron" else "nomprop");
        val structuralType = if (antecedents.size > 0) "anaphoric" else if (consequents.size > 0) "new" else "singleton";
        properties += mentType;
        properties += structuralType;
        // Nominal/proper splits
        if (mentType == "nomprop") {
          var goodHeadMatch = false;
          var misleadingHeadMatch = false;
          for (j <- 0 until i) {
            val antMent = docGraph.getMention(j);
            if (antMent.headStringLc == docGraph.getMention(i).headStringLc) {
              if (antecedents.contains(j)) {
                goodHeadMatch = true;
              } else {
                misleadingHeadMatch = true;
              }
            }
          }
          if (structuralType == "anaphoric") {
            properties += (if (goodHeadMatch || misleadingHeadMatch) "not-1st-occ" else "1st-occ");
            properties += (if (goodHeadMatch) "head-match" else "no-head-match");
            properties += (if (misleadingHeadMatch) "misleading" else "no-misleading");
          } else {
            properties += (if (goodHeadMatch || misleadingHeadMatch) "not-1st-occ" else "1st-occ");
            properties += (if (misleadingHeadMatch) "misleading" else "no-misleading");
          }
        } else { // Pronominal splits
          if (structuralType == "anaphoric") {
            val immediateAntecedentType = docGraph.getMention(antecedents.last).mentionType;
            properties += (if (immediateAntecedentType == MentionType.PRONOMINAL) "pron-ant" else "nomprop-ant");
          }
        }
        // Increment all subsequences of the properties
        for (i <- 1 to properties.size) {
          if (isCorrect) {
            correctCounter.incrementCount(properties.slice(0, i), 1.0);
          }
          totalCounter.incrementCount(properties.slice(0, i), 1.0);
        }
      }
    }
    var renderedStr = "Analysis:\n"
    // Sort the keys...this seems way harder than it has to be...
    val sortedKeys = totalCounter.keySet.asScala.toSeq.sortWith((a, b) => {
      var firstDiffIdx = -1;
      for (i <- 0 until Math.min(a.size, b.size)) {
        if (firstDiffIdx == -1 && a(i) != b(i)) {
          firstDiffIdx = i;
        }
      }
      // Never different
      if (firstDiffIdx == -1) {
        a.size < b.size; // return the shorter;
      } else {
        a(firstDiffIdx) < b(firstDiffIdx);
      }
    });
    for (key <- sortedKeys) yield {
      renderedStr += "   " + key.toString + ": " + correctCounter.getCount(key) + " / " + totalCounter.getCount(key) + " = " + (correctCounter.getCount(key)/totalCounter.getCount(key)) + "\n";
    }
    renderedStr;
  }
  
  // Returns F1 and string rep
  def computeRenderPairwisePRF1(docGraphs: Seq[DocumentGraph],
                                allPredBackptrs: Seq[Array[Int]],
                                allPredClusterings: Seq[OrderedClustering]): (Double, String) = {
    var numPairsCorrect = 0;
    var predNumPairs = 0;
    var goldNumPairs = 0;
    for (docIdx <- 0 until docGraphs.size) {
      val oraclePredClustering = docGraphs(docIdx).getOraclePredClustering;
      for (i <- 0 until allPredBackptrs(docIdx).size) {
        // Pairwise F1
        for (j <- 0 until i) {
          val predEdge = allPredClusterings(docIdx).areInSameCluster(i, j);
          val goldEdge = oraclePredClustering.areInSameCluster(i, j);
          if (predEdge && goldEdge) {
            numPairsCorrect += 1;
          }
          if (predEdge) {
            predNumPairs += 1;
          }
          if (goldEdge) {
            goldNumPairs += 1;
          }
        }
      }
    }
    val pairwisePrec = numPairsCorrect.toDouble / predNumPairs.toDouble;
    val pairwiseRec = numPairsCorrect.toDouble / goldNumPairs.toDouble;
    val pairwiseF1 = (2 * pairwisePrec * pairwiseRec / (pairwisePrec + pairwiseRec));
    (pairwiseF1, "Pairwise P/R/F1: " + numPairsCorrect + "/" + predNumPairs + " = " + pairwisePrec + ", " +
          numPairsCorrect + "/" + goldNumPairs + " = " + pairwiseRec + ", " + pairwiseF1);
  }
  
  def computeLinkageStats(docGraphs: Seq[DocumentGraph],
                          allPredBackptrs: Seq[Array[Int]]): String = {
    var nomPropToNomProp = 0;
    var nomPropToDiffNomProp = 0;
    var nomPropToPron = 0;
    var pronToNomProp = 0;
    var pronToPron = 0;
    var pronToDiffPron = 0;
    val linkageCounts = Array.tabulate(3, 4)((i, j) => 0);
    for (i <- 0 until docGraphs.size) {
      val docGraph = docGraphs(i);
      val predBackptrs = allPredBackptrs(i);
      for (j <- 0 until docGraph.size) {
        val backptr = predBackptrs(j);
        if (backptr != j) {
          val currIsPron = docGraph.getMention(j).mentionType == MentionType.PRONOMINAL;
          val prevIsPron = docGraph.getMention(backptr).mentionType == MentionType.PRONOMINAL;
          if (currIsPron && prevIsPron) {
            pronToPron += 1;
            if (!docGraph.getMention(j).headStringLc.equals(docGraph.getMention(backptr).headStringLc)) {
              pronToDiffPron += 1;
            }
          } else if (currIsPron && !prevIsPron) {
            pronToNomProp += 1;
          } else if (!currIsPron && prevIsPron) {
            nomPropToPron += 1;
          } else if (!currIsPron && !prevIsPron) {
            nomPropToNomProp += 1;
            if (!docGraph.getMention(j).headStringLc.equals(docGraph.getMention(backptr).headStringLc)) {
              nomPropToDiffNomProp += 1;
            }
          }
        }
      }
    }
    "Linkage counts: NP->NP: " + nomPropToNomProp + " (" + nomPropToDiffNomProp + " different), P->NP: " + pronToNomProp + ", NP->P: " + nomPropToPron + ", P->P: " + pronToPron + " (" + pronToDiffPron + " different)";
  }
  
  def renderLimitedOracle(docGraphs: Seq[DocumentGraph],
                          allPredBackptrs: Seq[Array[Int]],
                          criterion: (DocumentGraph, Int) => Boolean,
                          conllEvalScriptPath: String) = {
     val allFixedBackptrs = applyLimitedOracleToBackptrs(docGraphs, allPredBackptrs, criterion);
     val allFixedClusterings = allFixedBackptrs.map(OrderedClustering.createFromBackpointers(_));
     computeRenderCoNLL(docGraphs, allFixedClusterings, conllEvalScriptPath)
  }
  
  def applyLimitedOracleToBackptrs(docGraphs: Seq[DocumentGraph],
                                   allPredBackptrs: Seq[Array[Int]],
                                   criterion: (DocumentGraph, Int) => Boolean) = {
    for (docIdx <- 0 until docGraphs.size) yield {
      val docGraph = docGraphs(docIdx);
      val predBackptrs = allPredBackptrs(docIdx);
      (0 until predBackptrs.size).map(mentIdx => {
        if (MentionFilter.isIncorrect(docGraph, mentIdx, predBackptrs(mentIdx)) && criterion(docGraph, mentIdx)) {
          val allGoldAntecedents = docGraph.getGoldAntecedentsUnderCurrentPruning(mentIdx);
          // Shouldn't happen with these conditions, since isCanonicalProperNominalCase requires gold antecedents
          if (allGoldAntecedents.isEmpty) mentIdx else allGoldAntecedents.head;
        } else {
          predBackptrs(mentIdx)
        }
      });
    }
  }
  
  
  def computeRenderCoNLLIndividual(docGraphs: Seq[DocumentGraph],
                                   allPredClusterings: Seq[OrderedClustering],
                                   conllEvalScriptPath: String): String = {
    var str = "";
    for (i <- 0 until docGraphs.size) {
//      val conllStr = new CorefConllScorer(conllEvalScriptPath).renderSuffStats(docGraphs(i).corefDoc.rawDoc, allPredClusterings(i), docGraphs(i).corefDoc.goldClustering);
      val pc = new OrderedClusteringBound(docGraphs(i).getMentions(), allPredClusterings(i));
      val gc = new OrderedClusteringBound(docGraphs(i).corefDoc.goldMentions, docGraphs(i).corefDoc.goldClustering);
      val conllStr = new CorefConllScorer(conllEvalScriptPath).renderSuffStats(docGraphs(i).corefDoc.rawDoc, pc, gc);
      str += i + ": " + conllStr + "\n";
    }
    str;
  }
  
  def computeRenderCoNLL(docGraphs: Seq[DocumentGraph],
                         allPredClusterings: Seq[OrderedClustering],
                         conllEvalScriptPath: String): String = {
//    val conllStr = new CorefConllScorer(conllEvalScriptPath).renderFinalScore(docGraphs.map(_.corefDoc.rawDoc), allPredClusterings, docGraphs.map(_.corefDoc.goldClustering));
    val pcs = (0 until docGraphs.size).map(i => new OrderedClusteringBound(docGraphs(i).getMentions, allPredClusterings(i)));
    val gcs = docGraphs.map(graph => new OrderedClusteringBound(graph.corefDoc.goldMentions, graph.corefDoc.goldClustering))
    val conllStr = new CorefConllScorer(conllEvalScriptPath).renderFinalScore(docGraphs.map(_.corefDoc.rawDoc), pcs, gcs);
    "CoNLL score: " + conllStr;
  }
  
  //////////////////////////////
  // METRIC REIMPLEMENTATIONS //
  //////////////////////////////
  
  def computeMUC(doc: CorefDoc, predClustering: OrderedClustering, verbose: Boolean = false): Double = {
    val goldClusteringBound = doc.goldClustering.bind(doc.goldMentions, true).toSimple
    val predClusteringBound = predClustering.bind(doc.predMentions, true).toSimple
    val mucComputation = new MucComputation(goldClusteringBound, predClusteringBound, computeMapping(predClusteringBound, goldClusteringBound), computeMapping(goldClusteringBound, predClusteringBound))
    mucComputation.f1
  }
  
  def computeBcub(doc: CorefDoc, predClustering: OrderedClustering): Double = {
    val goldClusteringBound = doc.goldClustering.bind(doc.goldMentions, true).toSimple
    val predClusteringBound = predClustering.bind(doc.predMentions, true).toSimple
    val bcubComputation = new BcubComputation(goldClusteringBound, predClusteringBound, computeMapping(predClusteringBound, goldClusteringBound), computeMapping(goldClusteringBound, predClusteringBound), true)
    bcubComputation.f1
  }
  
  
  def getLosses(doc: CorefDoc, predBackpointers: Array[Int], mucPrecWeight: Double, mucRecWeight: Double, mucF1Weight: Double, bcubPrecWeight: Double, bcubRecWeight: Double, bcubF1Weight: Double, prunedEdges: Option[Array[Array[Boolean]]]): Array[Array[Double]] = {
    getLossesFast(doc, predBackpointers, mucPrecWeight, mucRecWeight, mucF1Weight, bcubPrecWeight, bcubRecWeight, bcubF1Weight, prunedEdges)
  }
  
//  def getLosses(doc: CorefDoc, predBackpointers: Array[Int], mucWeight: Double, bcubWeight: Double, prunedEdges: Option[Array[Array[Boolean]]]): Array[Array[Double]] = {
//    getLossesFast(doc, predBackpointers, 0, 0, mucWeight, 0, 0, bcubWeight, prunedEdges)
//  }
  
  def getLossesFast(doc: CorefDoc, predBackpointers: Array[Int], mucPrecWeight: Double, mucRecWeight: Double, mucF1Weight: Double, bcubPrecWeight: Double, bcubRecWeight: Double, bcubF1Weight: Double, prunedEdges: Option[Array[Array[Boolean]]]): Array[Array[Double]] = {
    // N.B. must be done sequentially because predBackpointers gets mutated
    val goldClustering = doc.goldClustering.bind(doc.goldMentions, false).toSimple
    val origPredClustering = OrderedClustering.createFromBackpointers(predBackpointers).bind(doc.predMentions, false).toSimple
    // Caches some computation about the backpointers structure to make computing cluster
    // changes easier
    val origPredClusteringWithBackpointers = new OrderedClusteringFromBackpointers(predBackpointers, origPredClustering.clustering)
    
    val goldToPredAlignment = computeMapping(origPredClustering, goldClustering)
    val predToGoldAlignment = computeMapping(goldClustering, origPredClustering)
      
    val mucComputation = new MucComputation(goldClustering, origPredClustering, goldToPredAlignment, predToGoldAlignment)
    val bcubComputation = new BcubComputation(goldClustering, origPredClustering, goldToPredAlignment, predToGoldAlignment, true)
    // Our MUC and BCUB agree with those from the CoNLL scorer; uncomment the following lines to see
//    val scorer = new CorefConllScorer(Driver.conllEvalScriptPath)
//    Logger.logss("MUC: " + mucComputation.prf1 + "; BCUB: " + bcubComputation.prf1)
//    Logger.logss(scorer.renderFinalScore(Seq(doc.rawDoc),
//                                         Seq(OrderedClustering.createFromBackpointers(predBackpointers).bind(doc.predMentions, true)),
//                                         Seq(doc.goldClustering.bind(doc.goldMentions, true))))
      
    val time = System.nanoTime() 
    val losses = Array.tabulate(doc.predMentions.size)(i => {
      val oldValue = predBackpointers(i)
      val results = Array.tabulate(i+1)(j => {
        if (prunedEdges.isDefined && prunedEdges.get(i)(j)) {
          Double.NegativeInfinity
        } else if (j == oldValue || (j != i && origPredClustering.clustering.areInSameCluster(j, oldValue))) {
          // The computation doesn't change if you're linking to something different in the same cluster.
          // Note that oldValue == i (e.g. we're only adding a link) never takes this code path because if i is non-anaphoric
          // it can't be in the same cluster as j (would need something to have multiple parents).
          // You can check that this doesn't matter by uncommenting the following lines
//          predBackpointers(i) = j
//          val oldMucComputation = new MucComputation(goldClustering, OrderedClustering.createFromBackpointers(predBackpointers).bind(doc.predMentions, false).toSimple, goldToPredAlignment, predToGoldAlignment)
//          val oldBcubComputation = new BcubComputation(goldClustering, OrderedClustering.createFromBackpointers(predBackpointers).bind(doc.predMentions, false).toSimple, goldToPredAlignment, predToGoldAlignment, true)
//          predBackpointers(i) = oldValue
//          require(Math.abs(mucComputation.f1 - oldMucComputation.f1) < 1e-5, i + " " + j + " " + oldValue)
//          require(Math.abs(bcubComputation.f1 - oldBcubComputation.f1) < 1e-5, i + " " + j + " " + oldValue)
          mucComputation.prec * mucPrecWeight + mucComputation.recall * mucRecWeight + mucComputation.f1 * mucF1Weight +
              bcubComputation.prec * bcubPrecWeight + bcubComputation.recall * bcubRecWeight + bcubComputation.f1 * bcubF1Weight
        } else {
          // Previous i was in the same cluster as oldValue, but j was in a distinct cluster
          // (due to the "else if" clause above)
          val oldClusterCurrMentIdx = origPredClustering.clustering.getClusterIdx(i)
          val oldClusterTargetMentIdx = origPredClustering.clustering.getClusterIdx(j)
//          require(oldClusterCurrMentIdx != oldClusterTargetMentIdx)
          // Returns a new cluster for oldValue and a distinct cluster for i and j (which now share a cluster)
          val (newClusterOldAnt, newClusterCurrMent) = origPredClusteringWithBackpointers.changeBackpointerGetClusters(i, j)
//          Logger.logss("i: " + i + " -- cluster: " + origPredClustering.clustering.clusters(oldClusterCurrMent))
//          Logger.logss("j: " + j + " -- cluster: " + origPredClustering.clustering.clusters(oldClusterTargetMent))
//          Logger.logss("oldValue = " + oldValue)
//          Logger.logss("New cluster old ant: " + newClusterOldAnt)
//          Logger.logss("New cluster curr ment: " + newClusterCurrMent)
          
          
          ////////////// MUC //////////////
          // NUMERATORS
          // Recall: Any partition containing one of the changed clusters has changed.
          // Need to recompute the mapping from predicted mention to predicted cluster index
          var mucRecNewNumer = mucComputation.recallSuffStats._1
          // OLD METHOD: This is slow but more obvious that it works.
//          val newClusters = new ArrayBuffer[Seq[Int]]
//          for (clusterIdx <- 0 until origPredClustering.clustering.clusters.size) {
//            if (clusterIdx != oldClusterCurrMentIdx && clusterIdx != oldClusterTargetMentIdx) {
//              newClusters += origPredClustering.clustering.clusters(clusterIdx)
//            }
//          }
//          if (newClusterOldAnt.size > 0) {
//            newClusters += newClusterOldAnt
//          }
//          newClusters += newClusterCurrMent
//          val newPredClustering = new OrderedClustering(newClusters)
//          val newPredClusteringIdxMap = newPredClustering.getClusterIdxMap
          // Much faster!
          val newPredClusteringIdxMap: HashMap[Int,Int] = new HashMap[Int,Int] ++ origPredClustering.clustering.getClusterIdxMap
          for (mentIdx <- origPredClustering.clustering.clusters(oldClusterCurrMentIdx)) {
            newPredClusteringIdxMap.remove(mentIdx)
          }
          if (oldClusterCurrMentIdx != oldClusterTargetMentIdx) {
            for (mentIdx <- origPredClustering.clustering.clusters(oldClusterTargetMentIdx)) {
              newPredClusteringIdxMap.remove(mentIdx)
            }
          }
          for (mentIdx <- newClusterOldAnt) {
            newPredClusteringIdxMap += mentIdx -> (Integer.MAX_VALUE - 1)
          }
          
          for (mentIdx <- newClusterCurrMent) {
            newPredClusteringIdxMap += mentIdx -> (Integer.MAX_VALUE - 2)
          }
          // Now get rid of old partition counts and add new ones
          for (goldClusterIdx <- 0 until mucComputation.keyClusterPartitions.size) {
            val partition = mucComputation.keyClusterPartitions(goldClusterIdx)
            if (partition.contains(oldClusterCurrMentIdx) || partition.contains(oldClusterCurrMentIdx)) {
              // Add in the old partition (to cancel out the negative term) and subtract the current one
              val goldCluster = goldClustering.clustering.clusters(goldClusterIdx)
              mucRecNewNumer = mucRecNewNumer + partition.size - MucComputation.computeMUCPartition(goldCluster, newPredClusteringIdxMap, goldToPredAlignment).size
            }
          } 
          // Precision: only a few terms have changed. Get rid of old numerator terms and
          // add new ones.
          var mucPrecNewNumer = mucComputation.precSuffStats._1
          mucPrecNewNumer -= (origPredClustering.clustering.clusters(oldClusterCurrMentIdx).size - mucComputation.responseClusterPartitions(oldClusterCurrMentIdx).size)
          // True when the target == curr mention
          if (oldClusterCurrMentIdx != oldClusterTargetMentIdx) {
            mucPrecNewNumer -= (origPredClustering.clustering.clusters(oldClusterTargetMentIdx).size - mucComputation.responseClusterPartitions(oldClusterTargetMentIdx).size)
          }
          // If old antecedent was self (non-anaphoric), a zero-sized cluster is returned. Otherwise we need to handle the term.
          if (newClusterOldAnt.size > 0) {
            mucPrecNewNumer += newClusterOldAnt.size - MucComputation.computeMUCPartition(newClusterOldAnt, goldClustering.clustering.getClusterIdxMap, predToGoldAlignment).size
          }
          mucPrecNewNumer += newClusterCurrMent.size - MucComputation.computeMUCPartition(newClusterCurrMent, goldClustering.clustering.getClusterIdxMap, predToGoldAlignment).size
          
          // DENOMINATOR: Precision is simply number of guess links, so we just need to add 1 if
          // we're adding a link and subtract 1 if we're cutting a link.
          val mucPrecNewDenom = mucComputation.precSuffStats._2 + (if (oldValue == i) 1 else if (j == i) -1 else 0)
          
          ////////////// B^3 //////////////
          
          var bcubRecNewNumer = bcubComputation.recallSuffStats._1.toDouble
          // NUMERATORS: Subtract off terms corresponding to old clusters.
          // Subtract off terms from the recall numerator.
          for (goldClusterIdx <- 0 until goldClustering.clustering.clusters.size) {
            if (bcubComputation.recallSumTerms(goldClusterIdx).contains(oldClusterCurrMentIdx)) {
              bcubRecNewNumer -= bcubComputation.recallSumTerms(goldClusterIdx)(oldClusterCurrMentIdx)
            }
//            if (bcubComputation.recallSumTerms(goldClusterIdx).contains(oldClusterTargetMentIdx)) {
            if (oldClusterCurrMentIdx != oldClusterTargetMentIdx && bcubComputation.recallSumTerms(goldClusterIdx).contains(oldClusterTargetMentIdx)) {
              bcubRecNewNumer -= bcubComputation.recallSumTerms(goldClusterIdx)(oldClusterTargetMentIdx)
            }
          }
          // Subtract off terms from the precision numerator
          var bcubPrecNewNumer = bcubComputation.precSuffStats._1.toDouble
          bcubPrecNewNumer -= bcubComputation.precSumTerms(oldClusterCurrMentIdx).values.foldLeft(0.0)(_ + _)
          if (oldClusterCurrMentIdx != oldClusterTargetMentIdx) {
            bcubPrecNewNumer -= bcubComputation.precSumTerms(oldClusterTargetMentIdx).values.foldLeft(0.0)(_ + _)
          }
          // Now go through and compute new terms for the two clusters
          for (goldCluster <- goldClustering.clustering.clusters) {
            val projectedGoldCluster = goldCluster.map(mentIdx => goldToPredAlignment(mentIdx)).filter(_ != -1)
            if (projectedGoldCluster.size != 0) {
              // Intersect with the two new response clusters *if* they're not singletons
              if (newClusterOldAnt.size > 1) {
                val intersectionSize1 = (newClusterOldAnt.toSet & projectedGoldCluster.toSet).size
                if (intersectionSize1 > 0) {
                  bcubRecNewNumer += intersectionSize1 * intersectionSize1.toDouble / goldCluster.size.toDouble
                  bcubPrecNewNumer += intersectionSize1 * intersectionSize1.toDouble / newClusterOldAnt.size.toDouble
                }
              }
              if (newClusterCurrMent.size > 1) {
                val intersectionSize2 = (newClusterCurrMent.toSet & projectedGoldCluster.toSet).size
                if (intersectionSize2 > 0) {
                  bcubRecNewNumer += intersectionSize2 * intersectionSize2.toDouble / goldCluster.size.toDouble
                  bcubPrecNewNumer += intersectionSize2 * intersectionSize2.toDouble / newClusterCurrMent.size.toDouble
                }
              }
            }
          }
          // DENOMINATORS: Recall is unchanged. But we may have created/destroyed singleton clusters
          // so the precision denominator might change.
          // Is oldValue a singleton and it wasn't before? Is i a singleton and it wasn't before? Is j no longer a singleton?
          var bcubPrecNewDenom = bcubComputation.precSuffStats._2.toDouble
          if (oldValue != i) {
            // We cut a link; oldValue may now be a singleton
            if (newClusterOldAnt.size == 1) {
              bcubPrecNewDenom -= 1
            }
            if (j == i) {
              // We're not creating a new link. i may now be a singleton.
              if (newClusterCurrMent.size == 1) {
                bcubPrecNewDenom -= 1
              }
            } else {
              // We're adding a new link; we may have killed a singleton when we added the link.
              if (origPredClustering.clustering.clusters(oldClusterTargetMentIdx).size == 1) {
                bcubPrecNewDenom += 1
              }
            }
          } else {
            // oldValue == i and j != i: we're adding a link
            // If the curr mention was a singleton, it no longer is, so add it
            if (origPredClustering.clustering.clusters(oldClusterCurrMentIdx).size == 1) {
              bcubPrecNewDenom += 1
            }
            // If the target mention was a singleton, it no longer is, so add it
            if (origPredClustering.clustering.clusters(oldClusterTargetMentIdx).size == 1) {
              bcubPrecNewDenom += 1
            }
          }
          ////////////// END B^3 //////////////
          
          val mucRecall = mucRecNewNumer.toDouble / mucComputation.recallSuffStats._2.toDouble
          val mucPrec = mucPrecNewNumer.toDouble / mucPrecNewDenom
          val mucF1 = 2.0 * mucPrec * mucRecall / (mucPrec + mucRecall)
          val bcubRecall = bcubRecNewNumer.toDouble / bcubComputation.recallSuffStats._2.toDouble
          val bcubPrec = bcubPrecNewNumer.toDouble / bcubPrecNewDenom
          val bcubF1 = 2.0 * bcubPrec * bcubRecall / (bcubPrec + bcubRecall)
//          predBackpointers(i) = j
//          val oldMucComputation = new MucComputation(goldClustering, OrderedClustering.createFromBackpointers(predBackpointers).bind(doc.predMentions, false).toSimple, goldToPredAlignment, predToGoldAlignment)
//          val oldBcubComputation = new BcubComputation(goldClustering, OrderedClustering.createFromBackpointers(predBackpointers).bind(doc.predMentions, false).toSimple, goldToPredAlignment, predToGoldAlignment, true)
//          predBackpointers(i) = oldValue
//          if (Math.abs(mucF1 - oldMucComputation.f1) > 1e-5) {
//            Logger.logss("Divergent MUC: " + i + " -> " + j + " instead of " + oldValue + ": recall = " + mucRecall + " vs. " +
//                         oldMucComputation.recall + "; prec = " + mucPrec + " vs. " + oldMucComputation.prec)
//            Logger.logss("NEW: " + mucRecNewNumer + " " + mucComputation.recallSuffStats._2.toDouble + " " + mucPrecNewNumer + " " + mucPrecNewDenom)
//            Logger.logss("OLD: " + oldMucComputation.recallSuffStats + " " + oldMucComputation.precSuffStats)
//            Logger.logss("i: " + i + " -- cluster: " + origPredClustering.clustering.clusters(oldClusterCurrMentIdx))
//            Logger.logss("j: " + j + " -- cluster: " + origPredClustering.clustering.clusters(oldClusterTargetMentIdx))
//            Logger.logss("oldValue = " + oldValue)
//            Logger.logss("New cluster old ant: " + newClusterOldAnt)
//            Logger.logss("New cluster curr ment: " + newClusterCurrMent)
//            Logger.logss("---------------------------------------------------")
//          }
//          if (Math.abs(bcubF1 - oldBcubComputation.f1) > 1e-5) {
//            Logger.logss("Divergent B^3: " + i + " -> " + j + " instead of " + oldValue + ": recall = " + bcubRecall + " vs. " +
//                         oldBcubComputation.recall + "; prec = " + bcubPrec + " vs. " + oldBcubComputation.prec)
//            Logger.logss("NEW: " + bcubRecNewNumer + " " + bcubComputation.recallSuffStats._2.toDouble + " " + bcubPrecNewNumer + " " + bcubPrecNewDenom)
//            Logger.logss("OLD: " + oldBcubComputation.recallSuffStats + " " + oldBcubComputation.precSuffStats)
//            Logger.logss("i: " + i + " -- cluster: " + origPredClustering.clustering.clusters(oldClusterCurrMentIdx))
//            Logger.logss("j: " + j + " -- cluster: " + origPredClustering.clustering.clusters(oldClusterTargetMentIdx))
//            Logger.logss("oldValue = " + oldValue)
//            Logger.logss("New cluster old ant: " + newClusterOldAnt)
//            Logger.logss("New cluster curr ment: " + newClusterCurrMent)
//            Logger.logss("---------------------------------------------------")
//          }
          mucPrec * mucPrecWeight + mucRecall * mucRecWeight + mucF1 * mucF1Weight +
              bcubPrec * bcubPrecWeight + bcubRecall * bcubRecWeight + bcubF1 * bcubF1Weight
        }
      })
      val bestScore = results(GUtil.argMaxIdx(results))
      for (j <- 0 until results.size) {
        // Scale by doc size so it's around the right scale
        results(j) = (bestScore - results(j)) * doc.predMentions.size
      }
//      for (j <- 0 until results.size) {
//        if ((j == i && doc.oraclePredOrderedClustering.startsCluster(i)) || (j != i && doc.oraclePredOrderedClustering.areInSameCluster(i, j))) {
//          if (!results(j).isInfinite() && results(j) > 1e-5) {
//            Logger.logss("Nonzero loss for gold mention: " + results(j) + " " + i + " " + j + " " + results.toSeq.filter(!_.isInfinite()) + "; last = " + results.last)
//          }
//        }
//      }
//      if (i < 10) {
//        Logger.logss("Results for " + i + ": " + results.toSeq)
//      }
      results
    })
//    Logger.logss((System.nanoTime - time)/1000000 + " millis")
    losses
  }
  
  // Align target to source
  def computeMapping(source: OrderedClusteringBoundSimple, target: OrderedClusteringBoundSimple) = {
    Array.tabulate(target.ments.size)(i => {
      val targetMent = target.ments(i)
      val matchingIndices = (0 until source.ments.size).filter(j => {
        val sourceMent = source.ments(j)
//        targetMent.sentIdx == sourceMent.sentIdx && targetMent.startIdx == sourceMent.startIdx && targetMent.endIdx == sourceMent.endIdx
        targetMent == sourceMent
      })
      if (matchingIndices.size != 0) {
        matchingIndices.head
      } else {
        -1
      }
    });
  }
  
  def main(args: Array[String]) {
    val key = new OrderedClusteringBoundSimple((0 until 7).map(i => (i, 0, 0)),
                                               OrderedClustering.createFromBackpointers(Array(0, 0, 1, 3, 3, 4, 5)))
    val response = new OrderedClusteringBoundSimple(Seq(0, 1, 2, 3, 5, 6, 7, 8).map(i => (i, 0, 0)),
                                                    OrderedClustering.createFromBackpointers(Array(0, 0, 2, 2, 4, 4, 5, 6)))
    val goldToPredAlignment = computeMapping(response, key)
    val predToGoldAlignment = computeMapping(key, response)
    val muc = new MucComputation(key, response, goldToPredAlignment, predToGoldAlignment)
    val bcub = new BcubComputation(key, response, goldToPredAlignment, predToGoldAlignment, true)
    val bcubWithSings = new BcubComputation(key, response, goldToPredAlignment, predToGoldAlignment, false)
    // Should be MUC = 0.4 0.4 0.4000000000000001
    //           B^3 = 0.5 0.41666666666666663 0.45454545454545453
    println(muc.prec + " " + muc.recall + " " + muc.f1)
    println(bcub.prec + " " + bcub.recall + " " + bcub.f1)
    println(bcubWithSings.prec + " " + bcubWithSings.recall + " " + bcubWithSings.f1)
//    println(computeMUC(key, response, goldToPredAlignment, predToGoldAlignment, true))
//    computeBcub(key, response, goldToPredAlignment, predToGoldAlignment)
//    computeBcub(key, response, goldToPredAlignment, predToGoldAlignment)
  }
}

class MucComputation(val goldClustering: OrderedClusteringBoundSimple,
                     val predClustering: OrderedClusteringBoundSimple,
                     val goldToPredAlignment: Array[Int],
                     val predToGoldAlignment: Array[Int]) {
  val keyClusterPartitions = MucComputation.computeMUCPartitions(goldClustering, predClustering, goldToPredAlignment)
  val responseClusterPartitions = MucComputation.computeMUCPartitions(predClustering, goldClustering, predToGoldAlignment)
  val recallSuffStats = MucComputation.computeMUCRecallSuffStats(goldClustering, goldToPredAlignment, keyClusterPartitions)
  val precSuffStats = MucComputation.computeMUCRecallSuffStats(predClustering, predToGoldAlignment, responseClusterPartitions)
  val recall = recallSuffStats._1.toDouble / recallSuffStats._2.toDouble
  val prec = precSuffStats._1.toDouble / precSuffStats._2.toDouble
  val f1 = 2 * prec * recall / (prec + recall)
  
  def prf1 = prec + " " + recall + " " + f1
}

object MucComputation {
  
  def computeMUCRecallSuffStats(goldClustering: OrderedClusteringBoundSimple,
                                goldToPredAlignment: Array[Int],
                                partitions: Array[HashSet[Int]]): (Int, Int) = {
    val key = goldClustering.clustering
    var recallNumer = 0
    for (i <- 0 until key.clusters.size) {
      recallNumer += key.clusters(i).size - partitions(i).size
    }
    val recallDenom = key.clusters.map(_.size - 1).foldLeft(0)(_ + _)
    recallNumer -> recallDenom
  }
  
  // Computes partitions of the key with respect to the response (the calculation for
  // recall; precision is just this flipped)
  def computeMUCPartitions(goldClustering: OrderedClusteringBoundSimple, predClustering: OrderedClusteringBoundSimple, goldToPredAlignment: Array[Int]): Array[HashSet[Int]] = {
    val key = goldClustering.clustering
    val response = predClustering.clustering
    Array.tabulate(key.clusters.size)(clusterIdx => {
      computeMUCPartition(key.clusters(clusterIdx), predClustering.clustering.getClusterIdxMap, goldToPredAlignment)
    })
  }
  
  // Note that for computing partitions of response clusters, just switch gold and pred
  def computeMUCPartition(goldCluster: Seq[Int], predMentionToClusterIdxMap: HashMap[Int,Int], goldToPredAlignment: Array[Int]): HashSet[Int] = {
    val responseClustersRepresented = new HashSet[Int]
    for (goldMentIdx <- goldCluster) {
      val alignment = goldToPredAlignment(goldMentIdx)
      if (alignment == -1) {
        // Make a unique cluster index for this mention. Use the number of pred mentions
        // (which is clearly larger than the number of pred clusters) and then add the gold
        // mention index to make it unique
        responseClustersRepresented += predMentionToClusterIdxMap.size + goldMentIdx
      } else {
        responseClustersRepresented += predMentionToClusterIdxMap(alignment)
      }
    }
    responseClustersRepresented
  }
}

class BcubComputation(val goldClustering: OrderedClusteringBoundSimple,
                      val predClustering: OrderedClusteringBoundSimple,
                      val goldToPredAlignment: Array[Int],
                      val predToGoldAlignment: Array[Int],
                      val filterSingletons: Boolean = true) {
  val recallSumTerms = BcubComputation.computeBcubRecallSumTerms(goldClustering, predClustering, goldToPredAlignment, filterSingletons)
  val precSumTerms = BcubComputation.computeBcubRecallSumTerms(predClustering, goldClustering, predToGoldAlignment, filterSingletons)
  val recallSuffStats = BcubComputation.computeBcubRecallSuffStats(goldClustering, recallSumTerms, filterSingletons)
  val precSuffStats = BcubComputation.computeBcubRecallSuffStats(predClustering, precSumTerms, filterSingletons)
  val recall = recallSuffStats._1.toDouble / recallSuffStats._2.toDouble
  val prec = precSuffStats._1.toDouble / precSuffStats._2.toDouble
  val f1 = 2 * prec * recall / (prec + recall)
  
  def prf1 = prec + " " + recall + " " + f1
}

object BcubComputation {
  
  def computeBcubRecallSuffStats(goldClustering: OrderedClusteringBoundSimple,
                                 sumTerms: Array[HashMap[Int,Double]],
                                 filterSingletons: Boolean = true): (Double, Int) = {
    val key = goldClustering.clustering
    val recallNumer = sumTerms.map(_.values.foldLeft(0.0)(_ + _)).foldLeft(0.0)(_ + _)
    val recallDenom = if (filterSingletons) {
      key.clusters.map(_.size).filter(_ != 1).foldLeft(0)(_ + _)
    } else {
      key.clusters.map(_.size).foldLeft(0)(_ + _)
    }
    recallNumer -> recallDenom
  }
  
  def computeBcubRecallSumTerms(goldClustering: OrderedClusteringBoundSimple, predClustering: OrderedClusteringBoundSimple, goldToPredAlignment: Array[Int], filterSingletons: Boolean = true): Array[HashMap[Int,Double]] = {
    val key = goldClustering.clustering
    val response = predClustering.clustering
    Array.tabulate(key.clusters.size)(keyClusterIdx => {
      val keyCluster = key.clusters(keyClusterIdx)
      val projectedToResponse = keyCluster.map(mentIdx => goldToPredAlignment(mentIdx)).filter(_ != -1)
      val oneOverKeyClusterSize = 1.0/ keyCluster.size.toDouble
      val sparseResponseMatches = new HashMap[Int,Double]
      if ((!filterSingletons || keyCluster.size > 1) && projectedToResponse.size != 0) {
        for (responseClusterIdx <- 0 until response.clusters.size) {
          val responseCluster = response.clusters(responseClusterIdx)
          if (!filterSingletons || responseCluster.size > 1) {
            val intersectionSize = (responseCluster.toSet & projectedToResponse.toSet).size
            if (intersectionSize > 0) {
              sparseResponseMatches += responseClusterIdx -> intersectionSize * intersectionSize.toDouble * oneOverKeyClusterSize
            }
          }
        }
      }
      sparseResponseMatches
    })
  }
}
