package edu.berkeley.nlp.entity.wiki

import java.io.File
import java.io.PrintWriter
import java.util.Collections

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.seqAsJavaListConverter
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser
import edu.berkeley.nlp.entity.Chunk
import edu.berkeley.nlp.entity.ConllDocWriter
import edu.berkeley.nlp.entity.preprocess.PreprocessingDriver
import edu.berkeley.nlp.entity.preprocess.Reprocessor
import edu.berkeley.nlp.entity.preprocess.Tokenizer
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import edu.berkeley.nlp.futile.syntax.Tree
import edu.berkeley.nlp.futile.util.Logger

case class ACEConllDoc(val docID: String,
                       val docPartNo: Int,
                       val words: Seq[Seq[String]],
                       val mentTypeChunks: Seq[Seq[Chunk[String]]],
                       val mentIdxChunks: Seq[Seq[Chunk[Int]]]);

case class MentionAnnot(val typeStr: String,
                        val entityIdx: Int,
                        val headStart: Int,
                        val headEnd: Int);

object ACEMunger {

  def mungeACE(aceInPath: String, wikiInPath: String, aceOutPath: String, wikiOutPath: String, wikipediaPath: String, parser: CoarseToFineMaxRuleParser, backoffParser: CoarseToFineMaxRuleParser) = {
    val directories = new File(aceInPath).listFiles;
    val rawWikiAnnots = readWikiAnnots(wikiInPath);
    val processedWikiAnnots = new CorpusWikiAnnots;
    val docSentLens = new HashMap[String,Seq[Int]];
    for (dir <- directories) {
      Logger.logss("Processing " + dir.getName);
      val docName = dir.getName;
      val rawText = IOUtils.readLines(dir.getAbsolutePath + "/raw.txt").asScala.reduce(_ + "\n" + _);
      val sentTokBoundaries = getTokenBoundariesBySentence(dir);
      val tokChunks = readKey(dir.getAbsolutePath + "/key.xml");
      // val tokChunksBySent = old thing;
      val tokChunksBySent = (0 until sentTokBoundaries.size).map(sentIdx => new ArrayBuffer[Chunk[(String,Int,Seq[String])]]);
      for (tokChunk <- tokChunks) {
        val maybeAlignedChunk = alignTokChunkToTokens(sentTokBoundaries, tokChunk);
        if (!maybeAlignedChunk.isDefined) {
          Logger.logss("WARNING: dropping chunk in " + docName);
        } else {
          val (sentIdx, tokAlignedChunk) = maybeAlignedChunk.get;
          val wikiTuple = (docName, "E" + tokAlignedChunk.label.entityIdx, tokAlignedChunk.label.headStart)
          val backupWikiTuple = (docName, "E" + tokAlignedChunk.label.entityIdx, tokChunk.start)
          var wikification = Seq[String]();
          if (rawWikiAnnots.contains(wikiTuple)) {
            wikification = rawWikiAnnots(wikiTuple);
          } else if (rawWikiAnnots.contains(backupWikiTuple)) {
            wikification = rawWikiAnnots(backupWikiTuple);
          } else if (!tokAlignedChunk.label.typeStr.endsWith("PRO")) {
            // Do nothing
            Logger.logs("WARNING: no Wikification for non-pronominal: " + wikiTuple + " " + tokAlignedChunk.label);
          }
//          if (wikification != "*null*" && tokAlignedChunk.label._1.endsWith("PRO")) {
//            Logger.logss("Wikification for pronominal: " + wikification + " " + wikiTuple + " " + tokAlignedChunk.label);
//          }
          if (wikification.size >= 2) {
            Logger.logss("Double Wikification: " + wikification);
          }
//          if (wikification.size > 0) {
          val label = (tokAlignedChunk.label.typeStr, tokAlignedChunk.label.entityIdx, wikification);
          tokChunksBySent(sentIdx) += new Chunk[(String,Int,Seq[String])](tokAlignedChunk.start, tokAlignedChunk.end, label);
//          }
        }
      }
      
      val words = sentTokBoundaries.map(_.map(pair => rawText.substring(pair._1, pair._2)));
      docSentLens.put(docName, words.map(_.size));
      
      val wordsPTB = words.map(_.map(word => {
        var newWord = word;
        for (entry <- Tokenizer.ReplacementMap) {
          newWord = newWord.replace(entry._1, entry._2);
        }
        newWord;
      }));
      
      val trees = if (parser != null && backoffParser != null) {
        wordsPTB.map(sentWords => Reprocessor.convertToFutileTree(PreprocessingDriver.parse(parser, backoffParser, sentWords.asJava)));
      } else {
        wordsPTB.map(sentWords => {
          new Tree[String]("ROOT", sentWords.map(sentWord => {
            new Tree[String]("X", Collections.singletonList(new Tree[String](sentWord)));
          }).asJava)
        })
      }
      val nerChunks = tokChunksBySent.map(_.map(chunk => new Chunk(chunk.start, chunk.end, chunk.label._1)))
      val corefChunks = tokChunksBySent.map(_.map(chunk => new Chunk(chunk.start, chunk.end, chunk.label._2)))
      val wikiChunks: Seq[SentWikiAnnots] = tokChunksBySent.map(_.map(chunk => new Chunk(chunk.start, chunk.end, chunk.label._3)))
      processedWikiAnnots.put(docName, new HashMap[Int,SentWikiAnnots] ++ (0 until wikiChunks.size).map(sentIdx => sentIdx -> wikiChunks(sentIdx)));
      
//      for (sentNerChunks <- nerChunks) {
//        def doCross(chunk1: Chunk[String], chunk2: Chunk[String]) = {
//          (chunk1.start < chunk2.start && chunk1.end > chunk2.start && chunk1.end < chunk2.end)
//        }
//        for (chunk1 <- sentNerChunks) {
//          for (chunk2 <- sentNerChunks) {
//            if (doCross(chunk1, chunk2)) {
//              Logger.logss("CROSSING CHUNKS: " + chunk1.toString + " " + chunk2.toString);
//            }
//          }
//        }
//      }
      
      val writer = new PrintWriter(aceOutPath + "/" + docName);
      ConllDocWriter.writeIncompleteConllDocNestedNER(writer,
                                                      docName,
                                                      0, 
                                                      words,
                                                      trees.map(tree => tree.getPreTerminalYield().asScala),
                                                      trees,
                                                      sentTokBoundaries.map(_.map(pair => "-")),
                                                      nerChunks,
                                                      corefChunks);
      writer.close();
    }
    Logger.logss("Getting redirects for Wikipedia entries");
    val allWikiTitles = processedWikiAnnots.flatMap(docMapEntry => docMapEntry._2.flatMap(sentMapEntry => sentMapEntry._2.flatMap(_.label))).toSet;
    Logger.logss(allWikiTitles.size + " redirect candidates: " + allWikiTitles);
    val redirectMapping = WikipediaRedirectsDB.processWikipediaGetRedirects(wikipediaPath, allWikiTitles)
    for (docName <- processedWikiAnnots.keySet) {
      val standoffWriter = new PrintWriter(wikiOutPath + "/" + docName + "-wiki");
      val annots = processedWikiAnnots(docName);
      val finalAnnots = annots.map(entry => entry._1 -> entry._2.map(chunk => {
        new Chunk(chunk.start, chunk.end, chunk.label.map(label => {
          if (redirectMapping.contains(label)) {
            Logger.logss("Redirecting " + chunk.label + " -> " + redirectMapping(label));
            redirectMapping(label);
          } else {
            label;
          }
        }))
      }));
      WikiAnnotReaderWriter.writeStandoffAnnots(standoffWriter, docName, 0, finalAnnots, docSentLens(docName))
      standoffWriter.close();
    }
  }
  
  // Extracts start and end token boundaries for heads in ACE (note that there are two
  // indices because sometimes two words are indicated as the head, such as "Barack Obama"
  // or "Middle East")
  def mungeACEToGetHeads(aceInPath: String) = {
    val directories = new File(aceInPath).listFiles;
    val processedHeadAnnots = new HashMap[String,HashMap[Int,Seq[Chunk[(Int,Int)]]]];
    for (dir <- directories) {
//      Logger.logss("Processing " + dir.getName);
      val docName = dir.getName;
      val sentTokBoundaries = getTokenBoundariesBySentence(dir);
      val tokChunks = readKey(dir.getAbsolutePath + "/key.xml");
      val headChunksBySent = (0 until sentTokBoundaries.size).map(sentIdx => new ArrayBuffer[Chunk[(Int,Int)]]);
      for (tokChunk <- tokChunks) {
        val maybeAlignedChunk = alignTokChunkToTokens(sentTokBoundaries, tokChunk);
        if (!maybeAlignedChunk.isDefined) {
          Logger.logss("WARNING: dropping chunk in " + docName);
        } else {
          val (sentIdx, tokAlignedChunk) = maybeAlignedChunk.get;
          val headStart = tokChunk.label.headStart;
          val headEnd = tokChunk.label.headEnd;
          val matchingHeadStartToken = sentTokBoundaries(sentIdx).filter(tok => tok._1 == headStart);
          val matchingHeadEndToken = sentTokBoundaries(sentIdx).filter(tok => tok._2 == headEnd);
          val label = if (matchingHeadStartToken.size == 1 && matchingHeadEndToken.size == 1) {
            val startIdx = sentTokBoundaries(sentIdx).indexOf(matchingHeadStartToken.head);
            val endIdx = sentTokBoundaries(sentIdx).indexOf(matchingHeadEndToken.head) + 1
            if (startIdx < endIdx) {
              (startIdx, endIdx);
            } else {
              (-1, -1)
            }
          } else {
            Logger.logss("WARNING: No exact match on head token in " + docName + " ; using the default head the default head");
//            Logger.logss(sentIdx + " "  + sentTokBoundaries(sentIdx) + " " + headStart + " " + headEnd)
            (-1, -1)
          }
          if (label != (-1, -1)) {
            headChunksBySent(sentIdx) += new Chunk[(Int,Int)](tokAlignedChunk.start, tokAlignedChunk.end, label);
          }
        }
      }
      processedHeadAnnots.put(docName, new HashMap[Int,Seq[Chunk[(Int,Int)]]] ++ (0 until headChunksBySent.size).map(sentIdx => sentIdx -> headChunksBySent(sentIdx)));
//      for (sentIdx <- processedHeadAnnots(docName).keySet) {
//        for (chunk <- processedHeadAnnots(docName)(sentIdx)) {
//          Logger.logss(docName + ": " + sentIdx + ": " + chunk);
//        }
//      }
    }
    Logger.logss("Processed heads");
    processedHeadAnnots;
  }
  
  /** 
   * Used to produce output like Angela Fahrni's
   */
  def mungeACEToGetChunkLabels(aceInPath: String, wikiInPath: String) = {
    val directories = new File(aceInPath).listFiles;
    val rawWikiAnnots = readWikiAnnots(wikiInPath);
    val processedAnnots = new CorpusAnnots[(String,String,Int)];
    for (dir <- directories) {
//      Logger.logss("Processing " + dir.getName);
      val docName = dir.getName;
      val sentTokBoundaries = getTokenBoundariesBySentence(dir);
      val tokChunks = readKey(dir.getAbsolutePath + "/key.xml");
      val chunksBySent = (0 until sentTokBoundaries.size).map(sentIdx => new SentAnnots[(String,String,Int)]);
      for (tokChunk <- tokChunks) {
        val maybeAlignedChunk = alignTokChunkToTokens(sentTokBoundaries, tokChunk);
        if (!maybeAlignedChunk.isDefined) {
          Logger.logss("WARNING: dropping chunk in " + docName);
        } else {
          val (sentIdx, tokAlignedChunk) = maybeAlignedChunk.get;
          val headStart = tokChunk.label.headStart;
          val headEnd = tokChunk.label.headEnd;
          val matchingHeadStartToken = sentTokBoundaries(sentIdx).filter(tok => tok._1 == headStart);
          val matchingHeadEndToken = sentTokBoundaries(sentIdx).filter(tok => tok._2 == headEnd);
          val label = (docName, "E" + tokAlignedChunk.label.entityIdx, tokAlignedChunk.label.headStart);
          chunksBySent(sentIdx) += new Chunk[(String,String,Int)](tokAlignedChunk.start, tokAlignedChunk.end, label);
        }
      }
      processedAnnots.put(docName, new DocAnnots[(String,String,Int)] ++ (0 until chunksBySent.size).map(sentIdx => sentIdx -> chunksBySent(sentIdx)));
//      for (sentIdx <- processedAnnots(docName).keySet) {
//        for (chunk <- processedAnnots(docName)(sentIdx)) {
//          Logger.logss(docName + ": " + sentIdx + ": " + chunk);
//        }
//      }
    }
    Logger.logss("Processed labels; " + processedAnnots.size + " documents, " + corpusAnnotsSize(processedAnnots) + " mentions");
    processedAnnots;
  }
  
  private def getAnnotationName(dir: File): String = {
    val dirContents = dir.listFiles;
    val annotationName = if (dirContents.map(_.getName).contains("annotation")) {
      "annotation"
    } else if (dirContents.map(_.getName).contains("annotations")) {
      "annotations"
    } else {
      throw new RuntimeException("No annotations for " + dir.getName);
    }
    annotationName;
  }
  
  private def getTokenFileName(annotDir: File): String = {
    if (annotDir.listFiles.map(_.getName).contains("tok")) {
      "tok"
    } else if (annotDir.listFiles.map(_.getName).contains("token")) {
      "token"
    } else {
      throw new RuntimeException("No token annotations in " + annotDir.getAbsolutePath);
    }
  }
  
  def getTokenBoundariesBySentence(dir: File): Seq[Seq[(Int,Int)]] = {
    val annotationName = getAnnotationName(dir);
    val annotDir = new File(dir.getAbsolutePath + "/" + annotationName);
    val tokName = getTokenFileName(annotDir);
    val sentBoundaries = getReconcileSentencesOrTokens(dir.getAbsolutePath + "/" + annotationName + "/sent");
    val tokBoundaries = getReconcileSentencesOrTokens(dir.getAbsolutePath + "/" + annotationName + "/" + tokName)
    val sentTokBoundaries = (0 until sentBoundaries.size).map(i => new ArrayBuffer[(Int,Int)]);
    for (tok <- tokBoundaries) {
      val sentIndex = (0 until sentBoundaries.size).filter(i => sentBoundaries(i)._1 <= tok._1 && tok._2 <= sentBoundaries(i)._2);
      if (sentIndex.size != 1) {
        Logger.logss("ERROR: Couldn't identify sentence for token");
      }
      sentTokBoundaries(sentIndex.head) += tok._1 -> tok._2
    }
    sentTokBoundaries;
  }
  
  def getPredMentionsBySentence(dir: File): Seq[Seq[(Int,Int)]] = {
    val tokBoundariesBySentence = getTokenBoundariesBySentence(dir);
    val annotationName = getAnnotationName(dir);
    val annotDir = new File(dir.getAbsolutePath + "/" + annotationName);
    val predNPs = getReconcileSentencesOrTokens(annotDir.getAbsolutePath + "/nps").map(pair => new Chunk[String](pair._1, pair._2, ""));
    val mentSpansPerSentence = (0 until tokBoundariesBySentence.size).map(i => new ArrayBuffer[(Int,Int)]);
    predNPs.foreach(np => {
      val maybeSentStartAndEnd = alignTokChunkToTokens(tokBoundariesBySentence, np);
      if (maybeSentStartAndEnd.isDefined) {
        val sentIdx = maybeSentStartAndEnd.get._1;
        mentSpansPerSentence(sentIdx) += maybeSentStartAndEnd.get._2.start -> maybeSentStartAndEnd.get._2.end;
      } 
    });
    mentSpansPerSentence;
  }
  
  
//  def getTokChunksBySentence(docName: String, sentTokBoundaries: Seq[Seq[(Int,Int)]], tokChunks: Seq[Chunk[(String,Int)]]) = {
//    val tokChunksBySent = (0 until sentTokBoundaries.size).map(sentIdx => new ArrayBuffer[Chunk[(String,Int)]]);
//    for (tokChunk <- tokChunks) {
//      var sentIdx = -1;
//      var tokIdxBefore = -1;
//      var tokIdxAfter = -1;
//      for (i <- 0 until sentTokBoundaries.size) {
//        for (j <- 0 until sentTokBoundaries(i).size) {
//          if (sentIdx == -1 && sentTokBoundaries(i)(j)._2 >= tokChunk.start) {
//            sentIdx = i;
//            tokIdxBefore = j;
//          }
//          if (tokIdxAfter == -1 && sentTokBoundaries(i)(j)._1 >= tokChunk.end) {
//            tokIdxAfter = j;
//          }
//        }
//        if (sentIdx != -1 && tokIdxBefore != -1 && tokIdxAfter == -1) {
//          val sentEndIdx = sentTokBoundaries(i).last._2;
//          if (tokIdxAfter > sentEndIdx) {
//            Logger.logss("WARNING: Crosses sentence boundary");
//          } else {
//            tokIdxAfter = sentTokBoundaries(i).size;
//          }
//        }
//      }
//      if (sentIdx == -1) {
//        sentIdx = sentTokBoundaries.size - 1;
//        tokIdxBefore = sentTokBoundaries(sentIdx).size - 1;
//        tokIdxAfter = sentTokBoundaries(sentIdx).size;
//      }
//      if (tokIdxBefore >= tokIdxAfter) {
//        Logger.logss("WARNING: " + tokIdxBefore + " before " + tokIdxAfter + " for file " + docName + ", sent = " + sentIdx + ", chunk = " + tokChunk + "; dropping it");
//      } else {
//        tokChunksBySent(sentIdx) += new Chunk[(String,Int)](tokIdxBefore, tokIdxAfter, tokChunk.label);
//      }
//    }
//    tokChunksBySent
//  }
  
  def alignTokChunkToTokens[T](sentTokBoundaries: Seq[Seq[(Int,Int)]], tokChunk: Chunk[T]): Option[(Int, Chunk[T])] = {
    var sentIdx = -1;
    var tokIdxBefore = -1;
    var tokIdxAfter = -1;
    for (i <- 0 until sentTokBoundaries.size) {
      for (j <- 0 until sentTokBoundaries(i).size) {
        if (sentIdx == -1 && sentTokBoundaries(i)(j)._2 >= tokChunk.start) {
          sentIdx = i;
          tokIdxBefore = j;
        }
        if (tokIdxAfter == -1 && sentTokBoundaries(i)(j)._1 >= tokChunk.end) {
          tokIdxAfter = j;
        }
      }
      if (sentIdx != -1 && tokIdxBefore != -1 && tokIdxAfter == -1) {
        val sentEndIdx = sentTokBoundaries(i).last._2;
        if (tokIdxAfter > sentEndIdx) {
          Logger.logss("WARNING: Crosses sentence boundary");
        } else {
          tokIdxAfter = sentTokBoundaries(i).size;
        }
      }
    }
    if (sentIdx == -1) {
      sentIdx = sentTokBoundaries.size - 1;
      tokIdxBefore = sentTokBoundaries(sentIdx).size - 1;
      tokIdxAfter = sentTokBoundaries(sentIdx).size;
    }
    if (tokIdxBefore >= tokIdxAfter) {
      Logger.logss("WARNING: " + tokIdxBefore + " before " + tokIdxAfter + ", sent = " + sentIdx + ", chunk = " + tokChunk + "; dropping it");
      None
    } else {
      Some(sentIdx -> new Chunk[T](tokIdxBefore, tokIdxAfter, tokChunk.label));
    }
  }
  
  // @return Chunk boundaries are char offsets, tuple is type, entity index, head start index (char offset)
  def readKey(keyFile: String): Seq[Chunk[MentionAnnot]] = {
    val elem = scala.xml.XML.loadFile(new File(keyFile))
    val allEntityNodes = elem \ "document" \ "entity";
    val usedEntityNames = new HashSet[String];
    allEntityNodes.flatMap(entity => {
      val entityType = (entity \ "@TYPE").head.text;
      val rawEntityName = (entity \ "@ID").head.text;
      val entityName = rawEntityName.substring(rawEntityName.lastIndexOf("-") + 1);
      if (usedEntityNames.contains(entityName)) {
        Logger.logss("WARNING: Duplicate entity name in " + keyFile + ": " + entityName);
      }
      require(entityName.startsWith("E"), entityName);
      val entityIdx = entityName.drop(1).toInt;
//      Logger.logss("TYPE: " + entityType);
      val tokOffsetMentions = (entity \ "entity_mention").map(mention => {
        val mentType = mention \ "@TYPE";
        val charSeqNode = mention \ "extent" \ "charseq"
        // Add one to the end to get fenceposts
        val span = (charSeqNode \ "@START").head.text.toInt -> ((charSeqNode \ "@END").head.text.toInt + 1)
        val headCharSeqNode = mention \ "head" \ "charseq"; 
        val headStart = (headCharSeqNode \ "@START").head.text.toInt
        val headEnd = (headCharSeqNode \ "@END").head.text.toInt + 1
        new Chunk[MentionAnnot](span._1, span._2, MentionAnnot(entityType + "-" + mentType, entityIdx, headStart, headEnd));
//        Logger.logss(span);
      })
      tokOffsetMentions;
    })
  }
  
  def readWikiAnnots(wikiFile: String): HashMap[(String,String,Int),Seq[String]] = {
    val wikiAnnots = new HashMap[(String,String,Int),Seq[String]];
    val missingSense = Seq("MISSING", "SENSE")
    val noPage = Seq("NO", "PAGE")
    val noAnnotation = Seq("NO_ANNOTATION")
    for (line <- IOUtils.readLines(wikiFile).asScala) {
      val entries = line.split("\\s+");
      var doc = entries(entries.size - 3);
      doc = if (doc.endsWith(".sgm")) doc.dropRight(4) else doc;
      val entity = entries(entries.size - 2);
      val startPos = entries(entries.size - 1).toInt;
      val thisLineWikiAnnots = entries.slice(0, entries.size - 3);
      if (thisLineWikiAnnots.head.startsWith(WikiUrl)) {
        wikiAnnots.put((doc, entity, startPos), thisLineWikiAnnots.map(_.substring(WikiUrl.size)));
      } else if (thisLineWikiAnnots.sameElements(noPage) || thisLineWikiAnnots.sameElements(missingSense) || thisLineWikiAnnots.sameElements(noAnnotation)) {
        wikiAnnots.put((doc, entity, startPos), Seq(NilToken));
      } else {
        Logger.logss("WARNING: Unrecognized annotation: " + (doc, entity, startPos) + " " + thisLineWikiAnnots.toSeq);
      }
    }
    Logger.logss(wikiAnnots.size + " wiki annotations read");
    wikiAnnots;
  }
  
  def getReconcileSentencesOrTokens(path: String): Seq[(Int,Int)] = {
    IOUtils.readLines(path).asScala.map(line => {
      val pair = line.split("\\s+")(1).split(",")
      require(pair.size == 2);
      pair(0).toInt -> pair(1).toInt;
    }).toSeq;
  }
  
  def main(args: Array[String]) {
//    mungeACEToGetHeads(args(0));
    val parser = if (args.size > 5) PreprocessingDriver.loadParser(args(5)) else null;
    val backoffParser = if (args.size > 5) PreprocessingDriver.loadParser(args(6)) else null;
    mungeACE(args(0), args(1), args(2), args(3), args(4), parser, backoffParser);
  }
}
