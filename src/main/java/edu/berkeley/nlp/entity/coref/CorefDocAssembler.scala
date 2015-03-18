package edu.berkeley.nlp.entity.coref
import scala.collection.JavaConverters.asScalaBufferConverter
import edu.berkeley.nlp.entity.Chunk
import edu.berkeley.nlp.entity.Driver;
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.lang.CorefLanguagePack
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.lang.EnglishCorefLanguagePack
import edu.berkeley.nlp.entity.lang.ChineseCorefLanguagePack
import edu.berkeley.nlp.entity.lang.ArabicCorefLanguagePack
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.futile.syntax.Trees.PennTreeRenderer
import edu.berkeley.nlp.entity.Document

case class ProtoMention(val sentIdx: Int, val startIdx: Int, val endIdx: Int, val headIdx: Int);
case class ProtoMentionFancy(val sentIdx: Int, val startIdx: Int, val endIdx: Int, val headIndices: Seq[Int]);

case class ProtoCorefDoc(val doc: Document, val goldMentions: Seq[Mention], val predProtoMentions: Seq[ProtoMention]);

class CorefDocAssembler(val langPack: CorefLanguagePack,
                        val useGoldMentions: Boolean) {
  
  def createCorefDoc(rawDoc: Document, propertyComputer: MentionPropertyComputer): CorefDoc = {
    val (goldMentions, goldClustering) = extractGoldMentions(rawDoc, propertyComputer);
    if (goldMentions.size == 0) {
      Logger.logss("WARNING: no gold mentions on document " + rawDoc.printableDocName);
    }
    val predMentions = if (useGoldMentions) goldMentions else extractPredMentions(rawDoc, propertyComputer, goldMentions);
    new CorefDoc(rawDoc, goldMentions, goldClustering, predMentions)
  }
  
  def createCorefDocFancy(rawDoc: Document, propertyComputer: MentionPropertyComputer, possibleChunks: Seq[Seq[Chunk[Boolean]]]): CorefDoc = {
    val (goldMentions, goldClustering) = extractGoldMentions(rawDoc, propertyComputer);
    if (goldMentions.size == 0) {
      Logger.logss("WARNING: no gold mentions on document " + rawDoc.printableDocName);
    }
    val predMentions = if (useGoldMentions) goldMentions else extractPredMentionsFancy(rawDoc, propertyComputer, goldMentions, possibleChunks);
//    MentionLatticeHandler.findConflicts(predMentions);
    new CorefDoc(rawDoc, goldMentions, goldClustering, predMentions)
  }
  
  def extractGoldMentions(rawDoc: Document, propertyComputer: MentionPropertyComputer): (Seq[Mention], OrderedClustering) = {
    CorefDocAssembler.extractGoldMentions(rawDoc, propertyComputer, langPack);
  }
  
  def extractPredMentions(rawDoc: Document, propertyComputer: MentionPropertyComputer, gms: Seq[Mention]): Seq[Mention] = {
    val protoMentionsSorted = getProtoMentionsSorted(rawDoc, gms);
    val finalMentions = new ArrayBuffer[Mention]();
    for (sentProtoMents <- protoMentionsSorted; protoMent <- sentProtoMents) {
      finalMentions += Mention.createMentionComputeProperties(rawDoc, finalMentions.size, protoMent.sentIdx, protoMent.startIdx, protoMent.endIdx, protoMent.headIdx, Seq(protoMent.headIdx), false, propertyComputer, langPack)
    }
    finalMentions;
  }
  
  def extractPredMentionsFancy(rawDoc: Document, propertyComputer: MentionPropertyComputer, gms: Seq[Mention], possibleChunks: Seq[Seq[Chunk[Boolean]]]): Seq[Mention] = {
    val protoMentionsSorted = getProtoMentionsSortedFancy(rawDoc, gms, possibleChunks);
    val finalMentions = new ArrayBuffer[Mention]();
    for (sentProtoMents <- protoMentionsSorted; protoMent <- sentProtoMents) {
      finalMentions += Mention.createMentionComputeProperties(rawDoc, finalMentions.size, protoMent.sentIdx, protoMent.startIdx, protoMent.endIdx, protoMent.headIdx, Seq(protoMent.headIdx), false, propertyComputer, langPack)
    }
    finalMentions;
  }
  
  private def getProtoMentionsSorted(rawDoc: Document, gms: Seq[Mention]): Seq[Seq[ProtoMention]] = {
    val mentionExtents = (0 until rawDoc.numSents).map(i => new HashSet[ProtoMention]);
    for (sentIdx <- 0 until rawDoc.numSents) {
      // Extract NE spans: filter out O, QUANTITY, CARDINAL, CHUNK
      // Throw out NE types which aren't mentions
      val filterNEsByType: Chunk[String] => Boolean = chunk => !(chunk.label == "O" || chunk.label == "QUANTITY" || chunk.label == "CARDINAL" || chunk.label == "PERCENT");
      // Extract NPs and PRPs *except* for those contained in NE chunks (the NE tagger seems more reliable than the parser)
      val posAndConstituentsOfInterest = langPack.getMentionConstituentTypes ++ langPack.getPronominalTags;
      for (label <- posAndConstituentsOfInterest) {
        mentionExtents(sentIdx) ++= rawDoc.trees(sentIdx).getSpansAndHeadsOfType(label).map(span => new ProtoMention(sentIdx, span._1, span._2, span._3));
      }
      // Add NEs if we want
      val neMentType = Driver.neMentType
      if (neMentType == "all") {
        val neProtoMents = rawDoc.nerChunks(sentIdx).filter(filterNEsByType).
            map(chunk => new ProtoMention(sentIdx, chunk.start, chunk.end, rawDoc.trees(sentIdx).getSpanHead(chunk.start, chunk.end)));
        mentionExtents(sentIdx) ++= neProtoMents
      } else if (neMentType == "nnp") {
        val spans = getMaximalNNPSpans(rawDoc.pos(sentIdx));
        val neProtoMents = spans.map(span => new ProtoMention(sentIdx, span._1, span._2, rawDoc.trees(sentIdx).getSpanHead(span._1, span._2)));
        mentionExtents(sentIdx) ++= neProtoMents
      } else {
        // Do nothing
      }
    }
    // Now take maximal mentions with the same heads
    if (Driver.filterNonMaximalNPs) {
      filterNonMaximalNPs(rawDoc, mentionExtents).map(CorefDocAssembler.sortProtoMentionsLinear(_));
    } else {
      mentionExtents.map(protoMents => CorefDocAssembler.sortProtoMentionsLinear(new ArrayBuffer[ProtoMention] ++ protoMents));
    }
//    if (Driver.filterNonMaximalNPs) {
//      val filteredProtoMentionsSorted = (0 until rawDoc.numSents).map(i => new ArrayBuffer[ProtoMention]);
//      for (sentIdx <- 0 until mentionExtents.size) {
//        val protoMentionsByHead = mentionExtents(sentIdx).groupBy(_.headIdx);
//        // Look from smallest head first
//        for (head <- protoMentionsByHead.keys.toSeq.sorted) {
//          // Find the biggest span containing this head
//          var currentBiggest: ProtoMention = null;
//          for (ment <- protoMentionsByHead(head)) {
//            // Overlapping but neither is contained in the other
//            if (currentBiggest != null && ((ment.startIdx < currentBiggest.startIdx && ment.endIdx < currentBiggest.endIdx) || (ment.startIdx > currentBiggest.startIdx && ment.endIdx > currentBiggest.endIdx))) {
//              Logger.logss("WARNING: mentions with the same head but neither contains the other");
//              Logger.logss("  " + rawDoc.words(sentIdx).slice(ment.startIdx, ment.endIdx) + ", head = " + rawDoc.words(sentIdx)(head));
//              Logger.logss("  " + rawDoc.words(sentIdx).slice(currentBiggest.startIdx, currentBiggest.endIdx) + ", head = " + rawDoc.words(sentIdx)(head));
//            }
//            // This one is bigger
//            if (currentBiggest == null || (ment.startIdx <= currentBiggest.startIdx && ment.endIdx >= currentBiggest.endIdx)) {
//              currentBiggest = ment;
//            }
//          }
//          filteredProtoMentionsSorted(sentIdx) += currentBiggest;
//          // ENGLISH ONLY: don't remove appositives
//          for (ment <- protoMentionsByHead(head)) {
//            val isNotBiggest = ment.startIdx != currentBiggest.startIdx || ment.endIdx != currentBiggest.endIdx;
//            val isAppositiveLike = ment.endIdx < rawDoc.pos(sentIdx).size && (rawDoc.pos(sentIdx)(ment.endIdx) == "," || rawDoc.pos(sentIdx)(ment.endIdx) == "CC");
//            if (isNotBiggest && isAppositiveLike && Driver.includeAppositives) {
//              filteredProtoMentionsSorted(sentIdx) += ment;
//            }
//          }
//        }
//      }
//      filteredProtoMentionsSorted.map(sortProtoMentionsLinear(_));
//    } else {
//      mentionExtents.map(protoMents => sortProtoMentionsLinear(new ArrayBuffer[ProtoMention] ++ protoMents));
//    }
  }
  
  private def getProtoMentionsSortedFancy(rawDoc: Document, gms: Seq[Mention], possibleChunks: Seq[Seq[Chunk[Boolean]]]): Seq[Seq[ProtoMention]] = {
    val mentionExtents = (0 until rawDoc.numSents).map(i => new HashSet[ProtoMention]);
    for (sentIdx <- 0 until rawDoc.numSents) {
      // Extract NPs and PRPs *except* for those contained in NE chunks (the NE tagger seems more reliable than the parser)
      val posAndConstituentsOfInterest = langPack.getMentionConstituentTypes ++ langPack.getPronominalTags;
      for (label <- posAndConstituentsOfInterest) {
        mentionExtents(sentIdx) ++= rawDoc.trees(sentIdx).getSpansAndHeadsOfType(label).map(span => new ProtoMention(sentIdx, span._1, span._2, span._3));
      }
      // Add NEs, appropriately filtered by type
      val neProtoMents = possibleChunks(sentIdx).filter(_.label).
          map(chunk => new ProtoMention(sentIdx, chunk.start, chunk.end, rawDoc.trees(sentIdx).getSpanHead(chunk.start, chunk.end)));
      val existingHeadIndices = mentionExtents(sentIdx).map(_.headIdx);
      // Detect conflicts
      mentionExtents(sentIdx) ++= neProtoMents.filter(protoMent => !existingHeadIndices.contains(protoMent.headIdx));
    }
    // Now take maximal mentions with the same heads
    if (Driver.filterNonMaximalNPs) {
      filterNonMaximalNPs(rawDoc, mentionExtents).map(CorefDocAssembler.sortProtoMentionsLinear(_));
    } else {
      mentionExtents.map(protoMents => CorefDocAssembler.sortProtoMentionsLinear(new ArrayBuffer[ProtoMention] ++ protoMents));
    }
  }
  
  private def filterNonMaximalNPs(rawDoc: Document, mentionExtents: Seq[HashSet[ProtoMention]]) = {
    val filteredProtoMentionsSorted = (0 until rawDoc.numSents).map(i => new ArrayBuffer[ProtoMention]);
    for (sentIdx <- 0 until mentionExtents.size) {
      val protoMentionsByHead = mentionExtents(sentIdx).groupBy(_.headIdx);
      // Look from smallest head first
      for (head <- protoMentionsByHead.keys.toSeq.sorted) {
        // Find the biggest span containing this head
        var currentBiggest: ProtoMention = null;
        for (ment <- protoMentionsByHead(head)) {
          // Overlapping but neither is contained in the other
          if (currentBiggest != null && ((ment.startIdx < currentBiggest.startIdx && ment.endIdx < currentBiggest.endIdx) || (ment.startIdx > currentBiggest.startIdx && ment.endIdx > currentBiggest.endIdx))) {
            Logger.logss("WARNING: mentions with the same head but neither contains the other");
            Logger.logss("  " + rawDoc.words(sentIdx).slice(ment.startIdx, ment.endIdx) + ", head = " + rawDoc.words(sentIdx)(head));
            Logger.logss("  " + rawDoc.words(sentIdx).slice(currentBiggest.startIdx, currentBiggest.endIdx) + ", head = " + rawDoc.words(sentIdx)(head));
          }
          // This one is bigger
          if (currentBiggest == null || (ment.startIdx <= currentBiggest.startIdx && ment.endIdx >= currentBiggest.endIdx)) {
            currentBiggest = ment;
          }
        }
        filteredProtoMentionsSorted(sentIdx) += currentBiggest;
        // ENGLISH ONLY: don't remove appositives
        for (ment <- protoMentionsByHead(head)) {
          val isNotBiggest = ment.startIdx != currentBiggest.startIdx || ment.endIdx != currentBiggest.endIdx;
          val isAppositiveLike = ment.endIdx < rawDoc.pos(sentIdx).size && (rawDoc.pos(sentIdx)(ment.endIdx) == "," || rawDoc.pos(sentIdx)(ment.endIdx) == "CC");
          if (isNotBiggest && isAppositiveLike && Driver.includeAppositives) {
            filteredProtoMentionsSorted(sentIdx) += ment;
          }
        }
      }
    }
    filteredProtoMentionsSorted;
  }
  
  private def getMaximalNNPSpans(tags: Seq[String]) = {
    var start = -1;
    var inside = false;
    val spans = new ArrayBuffer[(Int,Int)]
    for (i <- 0 until tags.size) {
      if (tags(i).startsWith("NNP") && (i == 0 || !tags(i-1).startsWith("NNP"))) {
        start = i
        inside = true;
      }
      if (inside && !tags(i).startsWith("NNP")) {
        spans += start -> i;
        start = -1;
        inside = false;
      }
    }
    spans;
  }
  
  //////////////////
  // COORDINATION //
  //////////////////
  
  
  def createCorefDocWithCoordination(rawDoc: Document, propertyComputer: MentionPropertyComputer): CorefDoc = {
    val (goldMentions, goldClustering) = extractGoldMentionsWithCoordination(rawDoc, propertyComputer);
    if (goldMentions.size == 0) {
      Logger.logss("WARNING: no gold mentions on document " + rawDoc.printableDocName);
    }
    val predMentions = if (useGoldMentions) goldMentions else extractPredMentionsWithCoordination(rawDoc, propertyComputer, goldMentions);
    new CorefDoc(rawDoc, goldMentions, goldClustering, predMentions)
  }
  
  def extractGoldMentionsWithCoordination(rawDoc: Document, propertyComputer: MentionPropertyComputer): (Seq[Mention], OrderedClustering) = {
    val goldProtoMentionsSorted = getGoldProtoMentionsSortedWithCoordination(rawDoc);
    val finalMentions = new ArrayBuffer[Mention]();
    val goldClusterLabels = new ArrayBuffer[Int]();
    for (sentProtoMents <- goldProtoMentionsSorted; protoMent <- sentProtoMents) {
      finalMentions += Mention.createMentionComputeProperties(rawDoc, finalMentions.size, protoMent.sentIdx, protoMent.startIdx, protoMent.endIdx, protoMent.headIndices.head, protoMent.headIndices, protoMent.headIndices.size > 1, propertyComputer, langPack)
      val correspondingChunks = rawDoc.corefChunks(protoMent.sentIdx).filter(chunk => chunk.start == protoMent.startIdx && chunk.end == protoMent.endIdx);
      if (correspondingChunks.size != 1) {
        Logger.logss("WARNING: multiple gold coref chunks matching span");
        Logger.logss("Location: " + rawDoc.printableDocName + ", sentence " + protoMent.sentIdx + ": (" + protoMent.startIdx + ", " + protoMent.endIdx + ") " +
                     rawDoc.words(protoMent.sentIdx).slice(protoMent.startIdx, protoMent.endIdx));
      }
      require(correspondingChunks.size >= 1);
      goldClusterLabels += correspondingChunks.map(_.label).reduce(Math.min(_, _));
    }
    (finalMentions, OrderedClustering.createFromClusterIds(goldClusterLabels));
  }
  
  def extractPredMentionsWithCoordination(rawDoc: Document, propertyComputer: MentionPropertyComputer, gms: Seq[Mention]): Seq[Mention] = {
    val protoMentionsSorted = getProtoMentionsSortedWithCoordination(rawDoc, gms);
    val finalMentions = new ArrayBuffer[Mention]();
    for (sentProtoMents <- protoMentionsSorted; protoMent <- sentProtoMents) {
      finalMentions += Mention.createMentionComputeProperties(rawDoc, finalMentions.size, protoMent.sentIdx, protoMent.startIdx, protoMent.endIdx, protoMent.headIndices.head, protoMent.headIndices, protoMent.headIndices.size > 1, propertyComputer, langPack)
    }
    finalMentions;
  }
  
  private def getGoldProtoMentionsSortedWithCoordination(rawDoc: Document): Seq[Seq[ProtoMentionFancy]] = {
    val goldProtoMentions = for (sentIdx <- 0 until rawDoc.corefChunks.size) yield {
       for (chunk <- rawDoc.corefChunks(sentIdx)) yield {
         val headIndices = rawDoc.trees(sentIdx).getSpanHeadOrNPCoordinatedHeads(chunk.start, chunk.end);
         new ProtoMentionFancy(sentIdx, chunk.start, chunk.end, headIndices.toSeq.sorted);
       }
    }
    goldProtoMentions.map(_.sortBy(ment => (ment.sentIdx, ment.headIndices.head, ment.endIdx, ment.startIdx)));
  }
  
  private def getProtoMentionsSortedWithCoordination(rawDoc: Document, gms: Seq[Mention]): Seq[Seq[ProtoMentionFancy]] = {
    val mentionExtents = (0 until rawDoc.numSents).map(i => new HashSet[ProtoMentionFancy]);
    for (sentIdx <- 0 until rawDoc.numSents) {
      // Extract NE spans: filter out O, QUANTITY, CARDINAL, CHUNK
      // Throw out NE types which aren't mentions
      val filterNEsByType: Chunk[String] => Boolean = chunk => !(chunk.label == "O" || chunk.label == "QUANTITY" || chunk.label == "CARDINAL" || chunk.label == "PERCENT");
      val neProtoMentions = rawDoc.nerChunks(sentIdx).filter(filterNEsByType).
          map(chunk => new ProtoMentionFancy(sentIdx, chunk.start, chunk.end, rawDoc.trees(sentIdx).getSpanHeadOrNPCoordinatedHeads(chunk.start, chunk.end).toSeq.sorted));
      mentionExtents(sentIdx) ++= neProtoMentions
      // Extract NPs and PRPs *except* for those contained in NE chunks (the NE tagger seems more reliable than the parser)
      val posAndConstituentsOfInterest = langPack.getMentionConstituentTypes ++ langPack.getPronominalTags;
      for (label <- posAndConstituentsOfInterest) {
        mentionExtents(sentIdx) ++= rawDoc.trees(sentIdx).getSpansAndCoordinatedHeadsOfType(label).map(span => new ProtoMentionFancy(sentIdx, span._1, span._2, span._3.toSeq.sorted));
      }
    }
    // Now take maximal mentions with the same heads
    val filteredProtoMentionsSorted = (0 until rawDoc.numSents).map(i => new ArrayBuffer[ProtoMentionFancy]);
    for (sentIdx <- 0 until mentionExtents.size) {
      val protoMentionsByHead = mentionExtents(sentIdx).groupBy(_.headIndices);
      // Look from smallest head first
      for (headSet <- protoMentionsByHead.keys.toSeq) {
        // Find the biggest span containing this head
        var currentBiggest: ProtoMentionFancy = null;
        for (ment <- protoMentionsByHead(headSet)) {
          // Overlapping but neither is contained in the other
          if (currentBiggest != null && ((ment.startIdx < currentBiggest.startIdx && ment.endIdx < currentBiggest.endIdx) || (ment.startIdx > currentBiggest.startIdx && ment.endIdx > currentBiggest.endIdx))) {
            Logger.logss("WARNING: mentions with the same head but neither contains the other");
            Logger.logss("  " + rawDoc.words(sentIdx).slice(ment.startIdx, ment.endIdx) + ", head(s) = " + headSet.map(rawDoc.words(sentIdx)(_)));
            Logger.logss("  " + rawDoc.words(sentIdx).slice(currentBiggest.startIdx, currentBiggest.endIdx) + ", head = " + headSet.map(rawDoc.words(sentIdx)(_)));
          }
          // This one is bigger
          if (currentBiggest == null || (ment.startIdx <= currentBiggest.startIdx && ment.endIdx >= currentBiggest.endIdx)) {
            currentBiggest = ment;
          }
        }
        filteredProtoMentionsSorted(sentIdx) += currentBiggest;
        // ENGLISH ONLY: don't remove appositives
        for (ment <- protoMentionsByHead(headSet)) {
          val isNotBiggest = ment.startIdx != currentBiggest.startIdx || ment.endIdx != currentBiggest.endIdx;
          val isAppositiveLike = ment.endIdx < rawDoc.pos(sentIdx).size && (rawDoc.pos(sentIdx)(ment.endIdx) == "," || rawDoc.pos(sentIdx)(ment.endIdx) == "CC");
          if (isNotBiggest && isAppositiveLike && Driver.includeAppositives) {
            filteredProtoMentionsSorted(sentIdx) += ment;
          }
        }
      }
    }
    filteredProtoMentionsSorted.map(_.sortBy(ment => (ment.sentIdx, ment.headIndices.head, ment.endIdx, ment.startIdx)));
  }
  
  /////////////////////////////
  /////////////////////////////
  /////////////////////////////
  /////////////////////////////
  /////////////////////////////
  /////////////////////////////
  
//  def getProtoMentionCandidatesSorted(rawDoc: ConllDoc, gms: Seq[Mention]): Seq[Seq[ProtoMention]] = {
//    val mentionExtents = (0 until rawDoc.numSents).map(i => new HashSet[ProtoMention]);
//    for (sentIdx <- 0 until rawDoc.numSents) {
//      // Throw out NE types which aren't mentions
//      val filterNEsByType: Chunk[String] => Boolean = chunk => !(chunk.label == "O" || chunk.label == "QUANTITY" || chunk.label == "CARDINAL" || chunk.label == "PERCENT");
//      val neProtoMentions = rawDoc.nerChunks(sentIdx).filter(filterNEsByType).
//          map(chunk => new ProtoMention(sentIdx, chunk.start, chunk.end, rawDoc.trees(sentIdx).getSpanHead(chunk.start, chunk.end)));
//      mentionExtents(sentIdx) ++= neProtoMentions
//      // Extract NPs and PRPs *except* for those contained in NE chunks (the NE tagger seems more reliable than the parser)
//      val filterSpanIfInNE: ((Int, Int, Int)) => Boolean = startEndHead => neProtoMentions.filter(ment => ment.startIdx <= startEndHead._1 && startEndHead._2 <= ment.endIdx).size == 0;
//      val posAndConstituentsOfInterest = langPack.getMentionConstituentTypes ++ langPack.getPronominalTags;
//      for (label <- posAndConstituentsOfInterest) {
//        if (Driver.includeSpansContainedInNE) {
//          mentionExtents(sentIdx) ++= rawDoc.trees(sentIdx).getSpansAndHeadsOfType(label).map(span => new ProtoMention(sentIdx, span._1, span._2, span._3));
////          mentionExtents(sentIdx) ++= rawDoc.trees(sentIdx).getSpansAndHeadsOfType(label).map(span => new ProtoMention(sentIdx, span._1, span._2, span._3)).map(contractPMToRemovePossessive);
//        } else {
//          mentionExtents(sentIdx) ++= rawDoc.trees(sentIdx).getSpansAndHeadsOfType(label).filter(filterSpanIfInNE).map(span => new ProtoMention(sentIdx, span._1, span._2, span._3));
//        }
//      }
//    }
//    // Now take maximal mentions with the same heads
//    val filteredProtoMentionsSorted = (0 until rawDoc.numSents).map(i => new ArrayBuffer[ProtoMention]);
//    for (sentIdx <- 0 until mentionExtents.size) {
//      val protoMentionsByHead = mentionExtents(sentIdx).groupBy(_.headIdx);
//      // Look from smallest head first
//      for (head <- protoMentionsByHead.keys.toSeq.sorted) {
//        // Find the biggest span containing this head
//        var currentBiggest: ProtoMention = null;
//        for (ment <- protoMentionsByHead(head)) {
//          // Overlapping but neither is contained in the other
//          if (currentBiggest != null && ((ment.startIdx < currentBiggest.startIdx && ment.endIdx < currentBiggest.endIdx) || (ment.startIdx > currentBiggest.startIdx && ment.endIdx > currentBiggest.endIdx))) {
//            Logger.logss("WARNING: mentions with the same head but neither contains the other");
//            Logger.logss("  " + rawDoc.words(sentIdx).slice(ment.startIdx, ment.endIdx) + ", head = " + rawDoc.words(sentIdx)(head));
//            Logger.logss("  " + rawDoc.words(sentIdx).slice(currentBiggest.startIdx, currentBiggest.endIdx) + ", head = " + rawDoc.words(sentIdx)(head));
//          }
//          // This one is bigger
//          if (currentBiggest == null || (ment.startIdx <= currentBiggest.startIdx && ment.endIdx >= currentBiggest.endIdx)) {
//            currentBiggest = ment;
//          }
//        }
//        filteredProtoMentionsSorted(sentIdx) += currentBiggest;
//        // ENGLISH ONLY: don't remove appositives
//        for (ment <- protoMentionsByHead(head)) {
//          val isNotBiggest = ment.startIdx != currentBiggest.startIdx || ment.endIdx != currentBiggest.endIdx;
//          val isAppositiveLike = ment.endIdx < rawDoc.pos(sentIdx).size && (rawDoc.pos(sentIdx)(ment.endIdx) == "," || rawDoc.pos(sentIdx)(ment.endIdx) == "CC");
//          if (isNotBiggest && isAppositiveLike && Driver.includeAppositives) {
//            filteredProtoMentionsSorted(sentIdx) += ment;
//          }
//        }
//        // ENGLISH ONLY: don't remove conjunctions
//        
//      }
//    }
//    filteredProtoMentionsSorted.map(sortProtoMentionsLinear(_));
//    
//    
//      
//    // Try including / removing 's
//    val mungePossessive: ProtoMention => Seq[ProtoMention] = pm => {
//      val pms = new ArrayBuffer[ProtoMention];
//      pms += pm;
//      val sent = rawDoc.words(pm.sentIdx);
//      if (pm.endIdx <= sent.size - 1 && sent(pm.endIdx) == "'s") {
//        pms += new ProtoMention(pm.sentIdx, pm.startIdx, pm.endIdx + 1, pm.headIdx)
//      }
//      if (pm.endIdx - pm.startIdx >= 2 && sent(pm.endIdx - 1) == "'s") {
//        pms += new ProtoMention(pm.sentIdx, pm.startIdx, pm.endIdx - 1, (if (pm.headIdx == pm.endIdx - 1) pm.endIdx - 2 else pm.headIdx))
//      }
//      pms;
//    }
//    
//    // Try removing an initial "the"
//    val mungeThe: ProtoMention => Seq[ProtoMention] = pm => {
//      val pms = new ArrayBuffer[ProtoMention];
//      pms += pm;
//      val sent = rawDoc.words(pm.sentIdx);
//      if (pm.endIdx > pm.startIdx + 1 && rawDoc.words(pm.sentIdx)(pm.startIdx).toLowerCase == "the") {
//        pms += new ProtoMention(pm.sentIdx, pm.startIdx + 1, pm.endIdx, pm.headIdx)
//      }
//      pms;
//    }
//    val mungeFollowingContent: ProtoMention => Seq[ProtoMention] = pm => {
//      val pms = new ArrayBuffer[ProtoMention];
//      pms += pm;
//      if (pm.headIdx < pm.endIdx - 1) {
//        pms += new ProtoMention(pm.sentIdx, pm.startIdx, pm.headIdx + 1, pm.headIdx)
//      }
//      pms;
//    }
//    
//    filteredProtoMentionsSorted
////    filteredProtoMentionsSorted.map(_.flatMap(mungePossessive));
////    filteredProtoMentionsSorted.map(_.flatMap(mungeThe));
////    filteredProtoMentionsSorted.map(_.flatMap(mungeFollowingContent));
//    
////    mentionExtents.map(protoMents => new ArrayBuffer[ProtoMention] ++ protoMents)
////    mentionExtents.map(protoMents => new ArrayBuffer[ProtoMention] ++ protoMents).map(_.flatMap(mungeFollowingContent));
//    
//    // Try shaving off "the X 's"
////    filteredProtoMentionsSorted.map(sentPms => sentPms.flatMap(pm => {
////      val finalPms = new ArrayBuffer[ProtoMention] ++ Seq(pm);
////      if (pm.endIdx - pm.startIdx == 2 &&
////          rawDoc.words(pm.sentIdx)(pm.endIdx - 1) == "'s" &&
////          rawDoc.nerChunks(pm.sentIdx).filter(chunk => chunk.start == pm.startIdx && chunk.end == pm.startIdx + 1).size >= 1) {
////        finalPms += new ProtoMention(pm.sentIdx, pm.startIdx, pm.startIdx + 1, pm.headIdx);
////      }
////      if (pm.endIdx - pm.startIdx == 3 &&
////          (rawDoc.words(pm.sentIdx)(pm.startIdx) == "the" || rawDoc.words(pm.sentIdx)(pm.startIdx) == "a") &&
////          rawDoc.words(pm.sentIdx)(pm.endIdx - 1) == "'s" &&
////          rawDoc.nerChunks(pm.sentIdx).filter(chunk => chunk.start == pm.startIdx && chunk.end == pm.startIdx + 1).size >= 1) {
////        finalPms += new ProtoMention(pm.sentIdx, pm.startIdx + 1, pm.startIdx + 2, pm.headIdx);
////      }
////      finalPms;
////    })).map(sortProtoMentionsLinear(_));
//  }
}

object CorefDocAssembler {
  
  def apply(language: Language, useGoldMentions: Boolean) = {
    val langPack = language match {
      case Language.ENGLISH => new EnglishCorefLanguagePack();
      case Language.CHINESE => new ChineseCorefLanguagePack(); 
      case Language.ARABIC => new ArabicCorefLanguagePack();
      case _ => throw new RuntimeException("Unrecognized language");
    }
    new CorefDocAssembler(langPack, useGoldMentions);
  }
  
  def extractGoldMentions(rawDoc: Document, propertyComputer: MentionPropertyComputer, langPack: CorefLanguagePack): (Seq[Mention], OrderedClustering) = {
    val goldProtoMentionsSorted = getGoldProtoMentionsSorted(rawDoc);
    val finalMentions = new ArrayBuffer[Mention]();
    val goldClusterLabels = new ArrayBuffer[Int]();
    for (sentProtoMents <- goldProtoMentionsSorted; protoMent <- sentProtoMents) {
      finalMentions += Mention.createMentionComputeProperties(rawDoc, finalMentions.size, protoMent.sentIdx, protoMent.startIdx, protoMent.endIdx, protoMent.headIdx, Seq(protoMent.headIdx), false, propertyComputer, langPack)
      val correspondingChunks = rawDoc.corefChunks(protoMent.sentIdx).filter(chunk => chunk.start == protoMent.startIdx && chunk.end == protoMent.endIdx);
      if (correspondingChunks.size != 1) {
        Logger.logss("WARNING: multiple gold coref chunks matching span");
        Logger.logss("Location: " + rawDoc.printableDocName + ", sentence " + protoMent.sentIdx + ": (" + protoMent.startIdx + ", " + protoMent.endIdx + ") " +
                     rawDoc.words(protoMent.sentIdx).slice(protoMent.startIdx, protoMent.endIdx));
      }
      require(correspondingChunks.size >= 1);
      goldClusterLabels += correspondingChunks.map(_.label).reduce(Math.min(_, _));
    }
    (finalMentions, OrderedClustering.createFromClusterIds(goldClusterLabels));
  }
  
  def getGoldProtoMentionsSorted(rawDoc: Document): Seq[Seq[ProtoMention]] = {
    val goldProtoMentions = for (sentIdx <- 0 until rawDoc.corefChunks.size) yield {
       for (chunk <- rawDoc.corefChunks(sentIdx)) yield {
         val headIdx = rawDoc.trees(sentIdx).getSpanHead(chunk.start, chunk.end);
         new ProtoMention(sentIdx, chunk.start, chunk.end, headIdx);
       }
    }
    goldProtoMentions.map(sortProtoMentionsLinear(_));
  }
  
  def sortProtoMentionsLinear(protoMentions: Seq[ProtoMention]): Seq[ProtoMention] = {
    protoMentions.sortBy(ment => (ment.sentIdx, ment.headIdx, ment.endIdx, ment.startIdx));
  }
  
  def checkGoldMentionRecallQuick(protoDocs: Seq[ProtoCorefDoc]) {
    val numGMs = protoDocs.foldLeft(0)((size, doc) => size + doc.goldMentions.size);
    val numPMs = protoDocs.foldLeft(0)((size, doc) => size + doc.predProtoMentions.size);
    var numGMsRecalled = 0;
    var numGMsUnrecalledNonConstituents = 0;
    var numGMsUnrecalledCrossingBrackets = 0;
    var numGMsVerbal = 0;
    for (doc <- protoDocs; gm <- doc.goldMentions) {
      if (doc.predProtoMentions.filter(pm => pm.sentIdx == gm.sentIdx && pm.startIdx == gm.startIdx && pm.endIdx == gm.endIdx).size >= 1) {
        numGMsRecalled += 1;
      } else {
        if (doc.doc.trees(gm.sentIdx).doesCrossBrackets(gm.startIdx, gm.endIdx)) {
          numGMsUnrecalledCrossingBrackets += 1;
        }
        if (!doc.doc.trees(gm.sentIdx).isConstituent(gm.startIdx, gm.endIdx)) {
          numGMsUnrecalledNonConstituents += 1;
        } else {
          if (doc.doc.trees(gm.sentIdx).getConstituentType(gm.startIdx, gm.endIdx).startsWith("V")) {
            numGMsVerbal += 1;
          }
        }
      }
    }
    Logger.logss("Pred proto mentions: " + numPMs);
    Logger.logss("Recall: " + numGMsRecalled + "/" + numGMs + " = " + (numGMsRecalled.toDouble / numGMs));
    Logger.logss("Num GMs non-constituents: " + numGMsUnrecalledNonConstituents + ", num verbal: " + numGMsVerbal);
    Logger.logss("Num GMs crossing brackets (NC includes these): " + numGMsUnrecalledCrossingBrackets);
  }
  
  def checkGoldMentionRecall(docs: Seq[CorefDoc]) {
    val numGMs = docs.map(_.goldMentions.size).reduce(_ + _);
    val numPMs = docs.map(_.predMentions.size).reduce(_ + _);
    val numNomPMs = docs.map(doc => doc.predMentions.filter(_.mentionType == MentionType.NOMINAL).size).reduce(_ + _);
    val numPropPMs = docs.map(doc => doc.predMentions.filter(_.mentionType == MentionType.PROPER).size).reduce(_ + _);
    val numPronPMs = docs.map(doc => doc.predMentions.filter(_.mentionType == MentionType.PRONOMINAL).size).reduce(_ + _);
    val numDemonstrativePMs = docs.map(doc => doc.predMentions.filter(_.mentionType == MentionType.DEMONSTRATIVE).size).reduce(_ + _);
    var numGMsRecalled = 0;
    var numGMsUnrecalledNonConstituents = 0;
    // These partition the errors
    var numGMsUnrecalledCrossingBrackets = 0;
    var numGMsUnrecalledInternal = 0;
    var numGMsUnrecalledPPAttach = 0;
    var numGMsUnrecalledCoord = 0;
    var numGMsUnrecalledOther = 0;
    val missedConstituentTypes = new Counter[String];
    for (doc <- docs; gm <- doc.goldMentions) {
      if (doc.predMentions.filter(pm => pm.sentIdx == gm.sentIdx && pm.startIdx == gm.startIdx && pm.endIdx == gm.endIdx).size >= 1) {
        numGMsRecalled += 1;
      } else {
        if (!doc.rawDoc.trees(gm.sentIdx).isConstituent(gm.startIdx, gm.endIdx)) {
          numGMsUnrecalledNonConstituents += 1;
        }
        if (doc.rawDoc.trees(gm.sentIdx).doesCrossBrackets(gm.startIdx, gm.endIdx)) {
          numGMsUnrecalledCrossingBrackets += 1;
        } else if (doc.rawDoc.pos(gm.sentIdx).slice(gm.startIdx, gm.endIdx).map(_.startsWith("N")).reduce(_ && _)) {
          numGMsUnrecalledInternal += 1;
        } else if (gm.endIdx < doc.rawDoc.pos(gm.sentIdx).size && (doc.rawDoc.pos(gm.sentIdx)(gm.endIdx) == "IN" ||
                    doc.rawDoc.pos(gm.sentIdx)(gm.endIdx) == "TO")) {
          numGMsUnrecalledPPAttach += 1;
        } else if ((gm.endIdx < doc.rawDoc.words(gm.sentIdx).size && doc.rawDoc.words(gm.sentIdx)(gm.endIdx) == "and") ||
                   (gm.startIdx > 0 && doc.rawDoc.words(gm.sentIdx)(gm.startIdx - 1) == "and")) {
//          Logger.logss("Didn't get coordination-like mention: " + doc.rawDoc.words(gm.sentIdx).slice(gm.startIdx, gm.endIdx) + "\n" + PennTreeRenderer.render(doc.rawDoc.trees(gm.sentIdx).constTree));
          numGMsUnrecalledCoord += 1;
        } else {
          numGMsUnrecalledOther += 1;
        }
        val constituentType = doc.rawDoc.trees(gm.sentIdx).getConstituentType(gm.startIdx, gm.endIdx);
        missedConstituentTypes.incrementCount(constituentType, 1.0);
        if (constituentType.startsWith("N")) {
//          Logger.logss("Missed mention: " + PronounAnalyzer.renderMentionWithHeadAndContext(gm));
//          Logger.logss("  Mentions we had that sentence: " + doc.predMentions.filter(pm => pm.sentIdx == gm.sentIdx).map(pm => pm.spanToString));
        }
      }
    }
    Logger.logss("Detected " + numPMs + " predicted mentions (" + numNomPMs + " nominal, " + numPropPMs + " proper, " + numPronPMs + " pronominal, " + numDemonstrativePMs + " demonstrative), " +
                 numGMsRecalled + " / " + numGMs + " = " + (numGMsRecalled.toDouble/numGMs) + " gold mentions recalled (" + numGMsUnrecalledNonConstituents + " missed ones are not constituents)")
    Logger.logss("Partition of errors: " + numGMsUnrecalledCrossingBrackets + " cross brackets, " + numGMsUnrecalledInternal + " look like internal NPs, " +
                 numGMsUnrecalledPPAttach + " look like PP attachment problems, " + numGMsUnrecalledCoord + " look like coordination problems, " + numGMsUnrecalledOther + " other");
    Logger.logss("  Missed constituent types: " + missedConstituentTypes);
  }
}
