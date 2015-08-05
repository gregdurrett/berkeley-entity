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
  
  def computeMUC(docGraph: DocumentGraph, predClustering: OrderedClustering) {
    
  }
  
  def computeBCubed {
    
  }
}
