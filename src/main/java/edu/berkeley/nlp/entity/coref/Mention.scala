package edu.berkeley.nlp.entity.coref
import scala.collection.JavaConverters._
import edu.berkeley.nlp.entity.lang.CorefLanguagePack
import edu.berkeley.nlp.entity.sem.SemClass
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.sem.SemClasser
import edu.berkeley.nlp.futile.syntax.Trees.PennTreeRenderer
import edu.berkeley.nlp.futile.util.Counter
import edu.berkeley.nlp.entity.Chunk
import edu.berkeley.nlp.entity.Document
import edu.berkeley.nlp.entity.Driver;
import edu.berkeley.nlp.entity.WordNetInterfacer

// TODO: Extract an interface for ConllDoc so I don't have to keep the whole
// document around...but while I'm feature engineering it's useful to be able
// to put my hands on anything I want
class Mention(val rawDoc: Document,
              val mentIdx: Int,
              val sentIdx: Int,
              val startIdx: Int,
              val endIdx: Int,
              val headIdx: Int,
              val allHeadIndices: Seq[Int],
              val isCoordinated: Boolean,
              val mentionType: MentionType,
              val nerString: String,
              val number: Number,
              val gender: Gender) {
  
  private val cachedHeadStringLc = headString.toLowerCase;
  private val cachedAllHeadsLc = allHeadIndices.map(rawDoc.words(sentIdx)(_));
  private val cachedWordsLc = rawDoc.words(sentIdx).slice(startIdx, endIdx).map(_.toLowerCase);
  private val cachedSpanStringLc = spanToString.toLowerCase;
  
  private var cachedConjFeatureStrings = Array.tabulate(ConjFeatures.values.size)(i => "");
  private var cachedSemanticDescriptors: Option[IndexedSeq[String]] = None;
  
  var cachedNerPossibilities: Option[Chunk[Counter[String]]] = None;
  var cachedNerGold: Option[Chunk[String]] = None;

  def speaker = rawDoc.speakers(sentIdx)(headIdx);

  def headString = rawDoc.words(sentIdx)(headIdx);
  def headStringLc = cachedHeadStringLc;
  def allHeadsLc = cachedAllHeadsLc;
  def spanToString = rawDoc.words(sentIdx).slice(startIdx, endIdx).reduce(_ + " " + _);
  def spanToStringLc = cachedSpanStringLc;
  def words = rawDoc.words(sentIdx).slice(startIdx, endIdx);
  def wordsLc = cachedWordsLc;
  def pos = rawDoc.pos(sentIdx).slice(startIdx, endIdx);
  def headPos = rawDoc.pos(sentIdx)(headIdx);
  
  def accessWordOrPlaceholder(idx: Int) = {
    if (idx < 0) Mention.StartWordPlaceholder else if (idx >= rawDoc.words(sentIdx).size) Mention.EndWordPlaceholder else rawDoc.words(sentIdx)(idx);
  }
  
  def accessPosOrPlaceholder(idx: Int) = {
    if (idx < 0) Mention.StartPosPlaceholder else if (idx >= rawDoc.pos(sentIdx).size) Mention.EndPosPlaceholder else rawDoc.pos(sentIdx)(idx);
  }
  
  def contextWordOrPlaceholder(idx: Int) = accessWordOrPlaceholder(startIdx + idx);
  def contextPosOrPlaceholder(idx: Int) = accessPosOrPlaceholder(startIdx + idx);
  
  def governor = governorHelper(false);
  def governorPos = governorHelper(true);
  private def governorHelper(pos: Boolean) = {
    val parentIdx = rawDoc.trees(sentIdx).childParentDepMap(headIdx);
    if (parentIdx == -1) {
      "[ROOT]"
    } else {
      (if (headIdx < parentIdx) "L" else "R") + "-" + (if (pos) rawDoc.pos(sentIdx)(parentIdx) else rawDoc.words(sentIdx)(parentIdx));
    }
  }
  
//  private def wordsFromBaseIndexAndOffset(baseIdx: Int, offsets: Seq[Int]) = offsets.map(offset => accessWordOrPlaceholder(baseIdx + offset)).reduce(_ + " " + _)
//  private def possFromBaseIndexAndOffset(baseIdx: Int, offsets: Seq[Int]) = offsets.map(offset => accessPosOrPlaceholder(baseIdx + offset)).reduce(_ + " " + _)
//  
//  def wordsFromStart(offsets: Seq[Int]) = wordsFromBaseIndexAndOffset(startIdx, offsets);
//  def wordsFromHead(offsets: Seq[Int]) = wordsFromBaseIndexAndOffset(headIdx, offsets);
//  def wordsFromEnd(offsets: Seq[Int]) = wordsFromBaseIndexAndOffset(endIdx, offsets);
//  def possFromStart(offsets: Seq[Int]) = possFromBaseIndexAndOffset(startIdx, offsets);
//  def possFromHead(offsets: Seq[Int]) = possFromBaseIndexAndOffset(headIdx, offsets);
//  def possFromEnd(offsets: Seq[Int]) = possFromBaseIndexAndOffset(endIdx, offsets);
//  
//  def wordFromStart(offset: Int) = accessWordOrPlaceholder(startIdx + offset);
//  def wordFromHead(offset: Int) = accessWordOrPlaceholder(headIdx + offset);
//  def wordFromEnd(offset: Int) = accessWordOrPlaceholder(endIdx + offset);
//  def posFromStart(offset: Int) = accessPosOrPlaceholder(startIdx + offset);
//  def posFromHead(offset: Int) = accessPosOrPlaceholder(headIdx + offset);
//  def posFromEnd(offset: Int) = accessPosOrPlaceholder(endIdx + offset);
  
  // These are explicit rather than in terms of Seq[Int] for lower overhead during
  // feature computation.
//  def wordBigramFromStart(offset1: Int, offset2: Int) = accessWordOrPlaceholder(startIdx + offset1) + " " + accessWordOrPlaceholder(startIdx + offset2);
//  def wordBigramFromHead(offset1: Int, offset2: Int) = accessWordOrPlaceholder(headIdx + offset1) + " " + accessWordOrPlaceholder(headIdx + offset2);
//  def wordBigramFromEnd(offset1: Int, offset2: Int) = accessWordOrPlaceholder(endIdx + offset1) + " " + accessWordOrPlaceholder(endIdx + offset2);
//  def posBigramFromStart(offset1: Int, offset2: Int) = accessPosOrPlaceholder(startIdx + offset1) + " " + accessPosOrPlaceholder(startIdx + offset2);
//  def posBigramFromHead(offset1: Int, offset2: Int) = accessPosOrPlaceholder(headIdx + offset1) + " " + accessPosOrPlaceholder(headIdx + offset2);
//  def posBigramFromEnd(offset1: Int, offset2: Int) = accessPosOrPlaceholder(endIdx + offset1) + " " + accessPosOrPlaceholder(endIdx + offset2);
  
  def computeConjStr(conjFeatures: ConjFeatures, wni: Option[WordNetInterfacer], semClasser: Option[SemClasser]) = {
    val ordinal = conjFeatures.ordinal();
    if (cachedConjFeatureStrings(ordinal).isEmpty) {
      cachedConjFeatureStrings(ordinal) = conjFeatures match {
        case ConjFeatures.NONE => "-"; // not just empty so that the isEmpty check above passes
        case ConjFeatures.TYPE => mentionType.toString;
        case ConjFeatures.TYPE_OR_RAW_PRON => if (mentionType.isClosedClass) headStringLc else mentionType.toString;
        case ConjFeatures.TYPE_OR_CANONICAL_PRON => computeTypeOrCanonicalPronConjStr;
        case ConjFeatures.SEMCLASS_OR_CANONICAL_PRON => computeSemClassOrCanonicalPronConjStr(wni.get);
        case ConjFeatures.SEMCLASS_OR_CANONICAL_PRON_COORD => computeSemClassOrCanonicalPronConjStr(wni.get) + (if (isCoordinated) "-x" + allHeadIndices.size else "");
        case ConjFeatures.NER_OR_CANONICAL_PRON => computeNerOrCanonicalPronConjStr;
        case ConjFeatures.NERFINE_OR_CANONICAL_PRON => computeNerFineOrCanonicalPronConjStr;
        case ConjFeatures.SEMCLASS_NER_OR_CANONICAL_PRON => computeSemClassNerOrCanonicalPronConjStr(wni.get);
        case ConjFeatures.CUSTOM_OR_CANONICAL_PRON => computeCustomOrCanonicalPronConjStr(wni.get, semClasser.get);
        case ConjFeatures.CUSTOM_NER_OR_CANONICAL_PRON => computeCustomNerOrCanonicalPronConjStr(wni.get, semClasser.get);
        case ConjFeatures.CUSTOM_NERMED_OR_CANONICAL_PRON => computeCustomNerMedOrCanonicalPronConjStr(wni.get, semClasser.get);
        case ConjFeatures.CUSTOM_NERFINE_OR_CANONICAL_PRON => computeCustomNerFineOrCanonicalPronConjStr(wni.get, semClasser.get);
        case _ => throw new RuntimeException("Haven't defined how to compute conjStr for " + conjFeatures);
      }
    }
    cachedConjFeatureStrings(ordinal);
  }
  
  private def computeTypeOrCanonicalPronConjStr = if (mentionType.isClosedClass) {
    if (!PronounDictionary.canonicalize(headStringLc).equals("")) {
      PronounDictionary.canonicalize(headStringLc);
    } else {
      headStringLc;
    }
  } else {
    mentionType.toString();
  }
  
  private def computeSemClassOrCanonicalPronConjStr(wni: WordNetInterfacer) = {
    if (mentionType.isClosedClass) {
      computeTypeOrCanonicalPronConjStr;
    } else if (mentionType == MentionType.NOMINAL) {
      "NOM" + computeSemClass(wni).toString;
    } else {
      "PROP" + computeSemClass(wni).toString;
    }
  }
  
  private def computeCustomOrCanonicalPronConjStr(wni: WordNetInterfacer, semClasser: SemClasser) = {
    if (mentionType.isClosedClass) {
      computeTypeOrCanonicalPronConjStr;
    } else if (mentionType == MentionType.NOMINAL) {
      "NOM" + semClasser.getSemClass(this, wni);
    } else {
      "PROP" + semClasser.getSemClass(this, wni);
    }
  }
  
  private def computeCustomNerOrCanonicalPronConjStr(wni: WordNetInterfacer, semClasser: SemClasser) = {
    if (mentionType.isClosedClass) {
      computeTypeOrCanonicalPronConjStr;
    } else if (mentionType == MentionType.NOMINAL) {
      "NOM" + semClasser.getSemClass(this, wni);
    } else {
      "PROP" + SemClass.getSemClassOnlyNer(nerString);
    }
  }
  
  private def computeCustomNerMedOrCanonicalPronConjStr(wni: WordNetInterfacer, semClasser: SemClasser) = {
    if (mentionType.isClosedClass) {
      computeTypeOrCanonicalPronConjStr;
    } else if (mentionType == MentionType.NOMINAL) {
      "NOM" + semClasser.getSemClass(this, wni);
    } else {
      "PROP" + SemClass.getStrSemClassOnlyNerFiner(nerString);
    }
  }
  
  private def computeCustomNerFineOrCanonicalPronConjStr(wni: WordNetInterfacer, semClasser: SemClasser) = {
    if (mentionType.isClosedClass) {
      computeTypeOrCanonicalPronConjStr;
    } else if (mentionType == MentionType.NOMINAL) {
      "NOM" + semClasser.getSemClass(this, wni);
    } else {
      "PROP" + nerString;
    }
  }
  
  private def computeNerOrCanonicalPronConjStr = {
    if (mentionType.isClosedClass) {
      computeTypeOrCanonicalPronConjStr;
    } else if (mentionType == MentionType.PROPER) {
      "PROP" + SemClass.getSemClassOnlyNer(nerString);
    } else {
      "NOMINAL";
    }
  }
  
  private def computeNerFineOrCanonicalPronConjStr = {
    if (mentionType.isClosedClass) {
      computeTypeOrCanonicalPronConjStr;
    } else if (mentionType == MentionType.PROPER) {
      "PROP" + nerString;
    } else {
      "NOMINAL";
    }
  }
  
  private def computeSemClassNerOrCanonicalPronConjStr(wni: WordNetInterfacer) = {
    if (mentionType.isClosedClass) {
      computeTypeOrCanonicalPronConjStr;
    } else if (mentionType == MentionType.PROPER) {
      "PROP" + SemClass.getSemClassOnlyNer(nerString);
    } else {
      "NOM" + SemClass.getSemClass(headStringLc, wni);
    }
  }
  
  def computeSemClass(wni: WordNetInterfacer) = SemClass.getSemClass(headStringLc, nerString, wni)
  
  def computeSemanticDescriptors(featsToUse: Set[String], wni: WordNetInterfacer, semClasser: SemClasser): IndexedSeq[String] = {
    if (!cachedSemanticDescriptors.isDefined) {
      val indicators = new ArrayBuffer[String];
      if (!mentionType.isClosedClass && featsToUse.contains("scsc")) {
        indicators += semClasser.getSemClass(this, wni);
      }
      if (mentionType == MentionType.PROPER && featsToUse.contains("scner")) {
        indicators += "NE:" + nerString;
      }
      cachedSemanticDescriptors = Some(indicators);
    }
    cachedSemanticDescriptors.get;
  }
  
  def iWi(other: Mention) = {
    sentIdx == other.sentIdx && ((other.startIdx <= this.startIdx && this.endIdx <= other.endIdx) ||
                                 (this.startIdx <= other.startIdx && other.endIdx <= this.endIdx));
  }
  
  def contextTree = rawDoc.trees(sentIdx);
  
  def computeSyntacticUnigram: String = contextTree.computeSyntacticUnigram(headIdx);
  def computeSyntacticBigram: String = contextTree.computeSyntacticBigram(headIdx);
  def computeSyntacticPosition: String = contextTree.computeSyntacticPositionSimple(headIdx);
}

object Mention {
  
  val StartWordPlaceholder = "<s>";
  val EndWordPlaceholder = "</s>";
  val StartPosPlaceholder = "<S>";
  val EndPosPlaceholder = "</S>";
  
  def createMentionComputeProperties(rawDoc: Document,
                                     mentIdx: Int,
                                     sentIdx: Int,
                                     startIdx: Int,
                                     endIdx: Int,
                                     headIdx: Int,
                                     allHeadIndices: Seq[Int],
                                     isCoordinated: Boolean,
                                     propertyComputer: MentionPropertyComputer,
                                     langPack: CorefLanguagePack): Mention = {
    // NER
    var nerString = "O";
    // This will always match on ACE
    val maybeExactMatchChunk = rawDoc.nerChunks(sentIdx).filter(chunk => chunk.start == startIdx && chunk.end == endIdx);
    if (maybeExactMatchChunk.size == 1) {
      nerString = maybeExactMatchChunk.head.label;
    } else {
      // Otherwise, take the smallest chunk that contains the head
      val matchingChunks = rawDoc.nerChunks(sentIdx).filter(chunk => chunk.start <= headIdx && headIdx < chunk.end);
      if (!matchingChunks.isEmpty) {
        nerString = matchingChunks.sortBy(chunk => chunk.end - chunk.start).head.label;
      }
//      for (chunk <- rawDoc.nerChunks(sentIdx)) {
//        if (chunk.start <= headIdx && headIdx < chunk.end) {
//          nerString = chunk.label;
//        }
//      }
    }
    // MENTION TYPE
    var mentionType = if (endIdx - startIdx == 1 && PronounDictionary.isDemonstrative(rawDoc.words(sentIdx)(headIdx))) {
      MentionType.DEMONSTRATIVE;
    } else if (endIdx - startIdx == 1 && (PronounDictionary.isPronLc(rawDoc.words(sentIdx)(headIdx)) || langPack.getPronominalTags.contains(rawDoc.pos(sentIdx)(headIdx)))) {
      MentionType.PRONOMINAL;
    } else if (langPack.getProperTags.contains(rawDoc.pos(sentIdx)(headIdx)) || (Driver.setProperMentionsFromNER && nerString != "O")) {
      MentionType.PROPER;
    } else {
      MentionType.NOMINAL;
    }
    // GENDER AND NUMBER
    var number: Number = Number.SINGULAR;
    var gender: Gender = Gender.MALE;
    if (mentionType == MentionType.PRONOMINAL) {
      val pronLc = rawDoc.words(sentIdx)(headIdx).toLowerCase;
      gender = if (PronounDictionary.malePronouns.contains(pronLc)) {
        Gender.MALE 
      } else if (PronounDictionary.femalePronouns.contains(pronLc)) {
        Gender.FEMALE
      } else if (PronounDictionary.neutralPronouns.contains(pronLc)) {
        Gender.NEUTRAL;
      } else {
        Gender.UNKNOWN;
      }
      number = if (PronounDictionary.singularPronouns.contains(pronLc)) {
        Number.SINGULAR
      } else if (PronounDictionary.pluralPronouns.contains(pronLc)) {
        Number.PLURAL;
      } else {
        Number.UNKNOWN;
      }
    } else {
      if (propertyComputer.maybeNumGendComputer.isDefined) {
        number = propertyComputer.maybeNumGendComputer.get.computeNumber(rawDoc.words(sentIdx).slice(startIdx, endIdx), rawDoc.words(sentIdx)(headIdx));
        gender = if (nerString == "PERSON") {
          propertyComputer.maybeNumGendComputer.get.computeGenderPerson(rawDoc.words(sentIdx).slice(startIdx, endIdx), headIdx - startIdx);
        } else {
          propertyComputer.maybeNumGendComputer.get.computeGenderNonPerson(rawDoc.words(sentIdx).slice(startIdx, endIdx), rawDoc.words(sentIdx)(headIdx));
        }
      }
    }
    return new Mention(rawDoc, mentIdx, sentIdx, startIdx, endIdx, headIdx, allHeadIndices, isCoordinated, mentionType, nerString, number, gender);
  }
}

