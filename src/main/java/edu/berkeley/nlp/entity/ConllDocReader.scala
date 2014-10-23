package edu.berkeley.nlp.entity
import java.util.IdentityHashMap
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.bufferAsJavaListConverter
import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.entity.lang.ModArabicHeadFinder
import edu.berkeley.nlp.entity.lang.ModCollinsHeadFinder
import edu.berkeley.nlp.futile.ling.BikelChineseHeadFinder
import edu.berkeley.nlp.futile.syntax.Trees.PennTreeRenderer
import edu.berkeley.nlp.futile.syntax.Tree
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import java.io.File
import edu.berkeley.nlp.futile.ling.AbstractCollinsHeadFinder
import edu.berkeley.nlp.futile.syntax.Trees.PennTreeReader

class ConllDocReader(val lang: Language,
                     val betterParsesFile: String = "") {
  val betterParses = new HashMap[ArrayBuffer[String],Tree[String]]
  if (betterParsesFile != "") {
    val lines = IOUtils.lineIterator(betterParsesFile);
    while (lines.hasNext) {
      val tree = PennTreeReader.parseEasy(lines.next);
      if (tree != null) {
        betterParses.put(new ArrayBuffer[String] ++ tree.getYield().asScala, tree);
      }
    }
    betterParses;
  }
  
  val headFinder = lang match {
    case Language.ENGLISH => new ModCollinsHeadFinder();
    case Language.CHINESE => new BikelChineseHeadFinder();
    case Language.ARABIC => new ModArabicHeadFinder();
    case _ => throw new RuntimeException("Bad language, no head finder for " + lang);
  }
  
  def readConllDocs(fileName: String): Seq[ConllDoc] = {
    val fcn = (docID: String, docPartNo: Int, docBySentencesByLines: ArrayBuffer[ArrayBuffer[String]]) => assembleConllDoc(docBySentencesByLines, docID, docPartNo);
    ConllDocReader.readConllDocsGeneral(fileName, fcn);
  }
  
  def assembleConllDoc(docBySentencesByLines: ArrayBuffer[ArrayBuffer[String]],
                       docID: String,
                       docPartNo: Int) = {
    // Filter any empty sentences that snuck in there
    var docBySentencesByLinesFixed = docBySentencesByLines.filter(!_.isEmpty);
    // Filter out any sentences that are too short
    docBySentencesByLinesFixed = docBySentencesByLinesFixed.map(sentence => {
      val badSentence = sentence.map(_.split("\\s+").size).reduce(Math.min(_, _)) < 12;
      if (badSentence) {
        Logger.logss("WARNING: Bad sentence, too few fields:\n" + sentence.reduce(_ + "\n" + _));
        if (sentence(0).startsWith("tc/ch/00/ch_0011") && sentence.size == 1) {
          val replacement = "tc/ch/00/ch_0011   2   0    fillerword    WRB   (TOP(FRAG*))  -   -   -    B  *   -"
          Logger.logss("Salvaging this sentence, replacing it with:\n" + replacement);
          ArrayBuffer(replacement);
        } else if (sentence(0).startsWith("tc/ch/00/ch_0021") && sentence.size == 1) {
          val replacement = "tc/ch/00/ch_0021   2   0    fillerword    ADD   (TOP(NP*))  -   -   -   -   *   -"
          Logger.logss("Salvaging this sentence, replacing it with:\n" + replacement);
          ArrayBuffer(replacement);
        } else {
          throw new RuntimeException("This sentence wasn't one of the CoNLL 2012 sentences we hardcoded in; you need to manually fix it");
        }
      } else {
        sentence;
      }
    });
    // Parse the individual sentences
    for (i <- 0 until docBySentencesByLinesFixed.size) {
      // Shouldn't have empty sentences
      require(!docBySentencesByLinesFixed(i).isEmpty, docBySentencesByLinesFixed.map(_.size.toString).reduce(_ + " " + _));
      for (j <- 0 until docBySentencesByLinesFixed(i).size) {
        // Shouldn't have empty lines
        require(!docBySentencesByLinesFixed(i)(j).trim.isEmpty);
      }
    }
    val docFields = docBySentencesByLinesFixed.map(_.map(_.split("\\s+")));
    val wordss = docFields.map(_.map(_(3)));
    val poss = docFields.map(_.map(_(4)));
    val parseBitss = docFields.map(_.map(_(5)));
    val speakerss = docFields.map(_.map(_(9)));
    val nerBitss = docFields.map(_.map(_(10)));
    val corefBitss = docFields.map(_.map(lineFields => lineFields(lineFields.size - 1)));
    
    val trees = for (i <- 0 until wordss.size) yield {
      val constTree = if (!betterParses.isEmpty && betterParses.contains(new ArrayBuffer[String] ++ wordss(i))) {
        val tree = betterParses(wordss(i));
//        Logger.logss("Better parse for sentence " + i + " in doc " + docID + " " + docPartNo);
        poss(i) = new ArrayBuffer[String] ++ tree.getPreTerminalYield().asScala;
        tree;
      } else {
        ConllDocReader.assembleConstTree(wordss(i), poss(i), parseBitss(i))
      }
      val childParentMap = DepConstTree.extractDependencyStructure(constTree, headFinder);
      new DepConstTree(constTree, poss(i), wordss(i), childParentMap)
    }
    
    new ConllDoc(docID,
                 docPartNo,
                 wordss,
                 poss,
                 trees,
                 nerBitss.map(ConllDocReader.assembleNerChunks(_)),
                 corefBitss.map(ConllDocReader.assembleCorefChunks(_)),
                 speakerss)
  }
}

object ConllDocReader {
  
  def readConllDocsJustWords(fileName: String): Array[ConllDocJustWords] = {
    val fcn = (docID: String, docPartNo: Int, docBySentencesByLines: ArrayBuffer[ArrayBuffer[String]]) => {
      new ConllDocJustWords(docID, docPartNo, docBySentencesByLines.filter(!_.isEmpty).map(_.map(_.split("\\s+")(3))));
    };
    ConllDocReader.readConllDocsGeneral(fileName, fcn).toArray;
  }
  
  def readConllDocsGeneral[T](fileName: String, fcn: (String, Int, ArrayBuffer[ArrayBuffer[String]]) => T): Seq[T] = {
    val results = new ArrayBuffer[T];
    val allLines = IOUtils.readLinesHard(fileName).asScala;
    var docBySentencesByLines = new ArrayBuffer[ArrayBuffer[String]];
    var docID = "";
    var docPartNo = -1;
    // Split documents up into parts and sentences
    for (i <- 0 until allLines.size) {
      val line = allLines(i);
      if (line.startsWith("#begin document")) {
        val thisLineDocID = line.substring(allLines(i).indexOf("(") + 1, allLines(i).indexOf(")"));
        val thisLinePartNo = line.substring(line.indexOf("part ") + 5).trim.toInt;
        if (docID == "") {
          docID = thisLineDocID;
          docPartNo = thisLinePartNo;
        } else {
          // We just changed docIDs; flush the document
          results += fcn(docID, docPartNo, docBySentencesByLines);
          docBySentencesByLines = new ArrayBuffer[ArrayBuffer[String]];
          docID = thisLineDocID;
          docPartNo = thisLinePartNo;
        }
        docBySentencesByLines += new ArrayBuffer[String]();
      } else if (line.startsWith("#end document")) {
        // Do nothing
      } else if (line.trim.isEmpty) {
        docBySentencesByLines += new ArrayBuffer[String]();
      } else {
        require(!docBySentencesByLines.isEmpty, fileName + " " + i);
        docBySentencesByLines.last += line;
      }
    }
    results += fcn(docID, docPartNo, docBySentencesByLines);
    results
  }
  
  def assembleConstTree(words: Seq[String], pos: Seq[String], parseBits: Seq[String]): Tree[String] = {
    var finalTree: Tree[String] = null;
    val stack = new ArrayBuffer[String];
    // When a constituent is closed, the guy on top of the stack will become
    // his parent. Build Trees as we go and register them with their parents so
    // that when we close the parents, their children are already all there.
    val childrenMap = new IdentityHashMap[String, ArrayBuffer[Tree[String]]];
    for (i <- 0 until parseBits.size) {
      require(parseBits(i).indexOf("*") != -1, parseBits(i) + " " + parseBits + "\n" + words);
      val openBit = parseBits(i).substring(0, parseBits(i).indexOf("*"));
      val closeBit = parseBits(i).substring(parseBits(i).indexOf("*") + 1);
      // Add to the end of the stack
      for (constituentType <- openBit.split("\\(").drop(1)) {
        // Make a new String explicitly so the IdentityHashMap works
        val constituentTypeStr = new String(constituentType);
        stack += constituentTypeStr;
        childrenMap.put(stack.last, new ArrayBuffer[Tree[String]]());
      }
      // Add the POS and word, which aren't specified in the parse bit but do need
      // to be in the Tree object
      val preterminalAndLeaf = new Tree[String](pos(i), IndexedSeq(new Tree[String](words(i))).asJava);
      childrenMap.get(stack.last) += preterminalAndLeaf;
      // Remove from the end of the stack
      var latestSubtree: Tree[String] = null;
      for (i <- 0 until closeBit.size) {
        require(closeBit(i) == ')');
        val constituentType = stack.last;
        stack.remove(stack.size - 1);
        latestSubtree = new Tree[String](constituentType, childrenMap.get(constituentType).asJava);
        if (!stack.isEmpty) {
          childrenMap.get(stack.last) += latestSubtree;
        }
      }
      if (stack.isEmpty) {
        finalTree = latestSubtree;
      }
    }
    require(finalTree != null, stack);
    // In Arabic, roots appear to be unlabeled sometimes, so fix this
    if (finalTree.getLabel() == "") {
      finalTree = new Tree[String]("ROOT", finalTree.getChildren);
    }
    finalTree;
  }
  
  def assembleNerChunks(nerBits: Seq[String]) = assembleNestedLabeledChunks(nerBits);
  
  def assembleSrlChunks(srlBits: Seq[String]) = assembleNestedLabeledChunks(srlBits);
  
  def assembleNestedLabeledChunks(bits: Seq[String]) = {
    // Bits have to look like some number of (LABEL followed by * followed by some number of ) 
    val chunks = new ArrayBuffer[Chunk[String]]();
    val currStartIdxStack = new ArrayBuffer[Int];
    val currTypeStack = new ArrayBuffer[String];
    for (i <- 0 until bits.size) {
      val containsStar = bits(i).contains("*");
      var bitRemainder = bits(i);
      while (bitRemainder.startsWith("(")) {
        var endIdx = bitRemainder.indexOf("(", 1);
        if (endIdx < 0) {
          endIdx = (if (containsStar) bitRemainder.indexOf("*") else bitRemainder.indexOf(")"));
        }
        require(endIdx >= 0, bitRemainder + " " + bits);
        currStartIdxStack += i
        currTypeStack += bitRemainder.substring(1, endIdx);
        bitRemainder = bitRemainder.substring(endIdx);
      }
      if (containsStar) {
        require(bitRemainder.startsWith("*"), bitRemainder + " " + bits);
        bitRemainder = bitRemainder.substring(1);
      }
      while (bitRemainder.startsWith(")")) {
        require(!currStartIdxStack.isEmpty, "Bad bits: " + bits);
        chunks += new Chunk[String](currStartIdxStack.last, i+1, currTypeStack.last);
        currStartIdxStack.remove(currStartIdxStack.size - 1);
        currTypeStack.remove(currTypeStack.size - 1);
        bitRemainder = bitRemainder.substring(1);
      }
      require(bitRemainder.size == 0);
    }
    chunks;
  }
  
  def assembleCorefChunks(corefBits: Seq[String]) = {
    val corefChunks = new ArrayBuffer[Chunk[Int]];
    // For each cluster, keep a stack of the indices where that cluster's guys started
    // (We need a stack because very occasionally there are nested coreferent mentions, often
    // in conversational text with disfluencies.)
    val exposedChunkStartIndices = new HashMap[Int,ArrayBuffer[Int]];
    for (i <- 0 until corefBits.size) {
      val bit = corefBits(i);
      if (bit != "-") {
        val parts = bit.split("\\|");
        for (part <- parts) {
          if (part.contains("(") && part.contains(")")) {
            corefChunks += new Chunk[Int](i, i+1, part.substring(part.indexOf("(") + 1, part.indexOf(")")).toInt);
          } else if (part.contains("(")) {
            val clusterIndex = part.substring(part.indexOf("(") + 1).toInt;
            if (!exposedChunkStartIndices.contains(clusterIndex)) {
              exposedChunkStartIndices.put(clusterIndex, new ArrayBuffer[Int]);
            }
            exposedChunkStartIndices(clusterIndex) += i;
          } else if (part.contains(")")) {
            val clusterIndex = part.substring(0, part.indexOf(")")).toInt;
            require(exposedChunkStartIndices.contains(clusterIndex), "Bad coref bit sequence: " + corefBits);
            val chunkStartIndexStack = exposedChunkStartIndices(clusterIndex);
            require(chunkStartIndexStack.size >= 1, "Bad coref bit sequence: " + corefBits);
            val startIndex = chunkStartIndexStack.remove(chunkStartIndexStack.size - 1);
            corefChunks += new Chunk[Int](startIndex, i+1, clusterIndex);
          } else {
            throw new RuntimeException("Bad part: " + part);
          }
        }
      }
    }
    // In wsj_0990 sentence 9 there are some chunks which have the same span but
    // different labels...filter these out and take the one with the smallest label.
    val corefChunksNoDupes = corefChunks.filter(chunk1 => {
      // Check if any identical chunk has a lower label, reduce on this (true if any chunk does),
      // then filter out if this is true
      !corefChunks.map(chunk2 => chunk1.start == chunk2.start && chunk1.end == chunk2.end && chunk2.label < chunk1.label).reduce(_ || _);
    });
    corefChunksNoDupes;
  }
  
//  def loadRawConllDocs(path: String, size: Int, gold: Boolean, lang: Language = Language.ENGLISH, betterParsesFile: String = ""): Seq[ConllDoc] = {
//    loadRawConllDocsWithSuffix(path, size, if (gold) "gold_conll" else "auto_conll", lang, betterParsesFile);
//  }
  
  def loadRawConllDocsWithSuffix(path: String, size: Int, suffix: String, lang: Language = Language.ENGLISH, betterParsesFile: String = ""): Seq[ConllDoc] = {
    Logger.logss("Loading " + size + " docs from " + path + " ending with " + suffix);
    val rawDir = new File(path);
    if (!rawDir.exists() || !rawDir.canRead() || rawDir.listFiles == null || rawDir.listFiles.isEmpty) {
      throw new RuntimeException("Couldn't find directory " + path);
    }
    val rawFiles = rawDir.listFiles.sortBy(_.getAbsolutePath());
    val files = rawFiles.filter(file => file.getAbsolutePath.endsWith(suffix));
    val reader = new ConllDocReader(lang, betterParsesFile);
    val docs = new ArrayBuffer[ConllDoc];
    var docCounter = 0;
    var fileIdx = 0;
    while (fileIdx < files.size && (size == -1 || docCounter < size)) {
      val newDocs = reader.readConllDocs(files(fileIdx).getAbsolutePath);
      docs ++= newDocs;
      docCounter += newDocs.size
      fileIdx += 1;
    }
    val numDocs = if (size == -1) docs.size else Math.min(size, files.size);
    Logger.logss(docs.size + " docs loaded from " + fileIdx + " files, retaining " + numDocs);
    if (docs.size == 0) {
      Logger.logss("WARNING: Zero docs loaded...double check your paths unless you meant for this happen");
    }
    docs.slice(0, numDocs);
  }
  
  def readDocNames(path: String): Seq[String] = {
    val file = new File(path);
    if (!file.exists()) Seq[String]() else file.listFiles.map(_.getName).map(name => {
      if (name.contains(".")) name.substring(0, name.indexOf(".")) else name;
    }).sorted;
  }
  
  def main(args: Array[String]) {
//    testDoc("clean-data/smtrain/phoenix_0001.v2_auto_conll");
//    testDoc("clean-data/problematic_ones/a2e_0024.v2_auto_conll");
//    testDoc("clean-data/problematic_ones/wsj_0990.v2_auto_conll");
    Logger.logss(readDocNames("data/dev"));
    
//    val directory = new File("data/conll-2012-en/test-orig/");
//    val blindFiles = directory.listFiles.filter(_.getName().endsWith("auto_conll")).sorted;
//    val goldFiles = directory.listFiles.filter(_.getName().endsWith("gold_conll")).sorted;
//    for ((blindFile, goldFile) <- blindFiles.zip(goldFiles)) {
//      boltInClusters(blindFile.getAbsolutePath, goldFile.getAbsolutePath, "data/conll-2012-en/test/" + blindFile.getName);
//    }
  }
  
  def boltInClusters(inputBlindPath: String, inputGoldPath: String, outputSightedPath: String) {
    val blindItr = IOUtils.lineIterator(inputBlindPath);
    val goldItr = IOUtils.lineIterator(inputGoldPath);
    val out = IOUtils.openOutHard(outputSightedPath);
    while (blindItr.hasNext) {
      val blindLine = blindItr.next;
      val goldLine = goldItr.next;
      if (blindLine.trim.isEmpty || blindLine.startsWith("#")) {
        out.println(blindLine);
      } else {
        var goldLastWhitespaceIndex = goldLine.size - 1;
        while (goldLastWhitespaceIndex >= 0 && !Character.isWhitespace(goldLine.charAt(goldLastWhitespaceIndex))) {
          goldLastWhitespaceIndex -= 1;
        }
        if (goldLastWhitespaceIndex == -1) {
          throw new RuntimeException("Error on " + goldLine);
        }
        out.println(blindLine.substring(0, blindLine.lastIndexOf("-")) + goldLine.substring(goldLastWhitespaceIndex + 1));
      }
    }
    out.close;
  }
  
  def testDoc(fileName: String) {
    val reader = new ConllDocReader(Language.ENGLISH);
    val conllDocs = reader.readConllDocs(fileName);
    conllDocs.foreach(conllDoc => {
      println(conllDoc.docID + " " + conllDoc.docPartNo)
      val words = conllDoc.words;
      require(words.size == conllDoc.pos.size);
      require(words.size == conllDoc.trees.size);
      require(words.size == conllDoc.nerChunks.size);
      require(words.size == conllDoc.corefChunks.size);
      require(words.size == conllDoc.speakers.size);
      for (i <- 0 until words.size) {
        println("==SENTENCE " + i + "==");
        require(words(i).size == conllDoc.pos(i).size);
        println((0 until words(i).size).map(j => conllDoc.pos(i)(j) + ":" + words(i)(j)).reduce(_ + " " + _));
        println(PennTreeRenderer.render(conllDoc.trees(i).constTree));
        println("NER: " + conllDoc.nerChunks(i));
        println("COREF: " + conllDoc.corefChunks(i));
        println("SPEAKERS: " + conllDoc.speakers(i));
      }
    });
  }
}
