package edu.berkeley.nlp.entity.wiki

import scala.collection.mutable.HashMap
import edu.berkeley.nlp.entity.Chunk
import java.io.PrintWriter
import edu.berkeley.nlp.entity.ConllDocWriter
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.ConllDocReader
import edu.berkeley.nlp.entity.coref.UID
import java.io.File
import edu.berkeley.nlp.futile.util.Logger

object WikiAnnotReaderWriter {
  
  // This can never be in a Wikipedia article title
  val WikiTitleSeparator = "|";
  
  def readAllStandoffAnnots(dirName: String): HashMap[UID,Seq[Seq[Chunk[Seq[String]]]]] = {
    val standoffAnnots = new HashMap[UID,Seq[Seq[Chunk[Seq[String]]]]]
    for (file <- new File(dirName).listFiles) {
      standoffAnnots ++= readStandoffAnnots(file.getAbsolutePath)
    }
    standoffAnnots
  }
  
  def readStandoffAnnots(fileName: String): HashMap[UID,Seq[Seq[Chunk[Seq[String]]]]] = {
    val fcn = (docID: String, docPartNo: Int, docBySentencesByLines: ArrayBuffer[ArrayBuffer[String]]) => {
      // type = (UID, Seq[Seq[Chunk[Seq[String]]]])
      new UID(docID, docPartNo) -> docBySentencesByLines.map(assembleWikiChunks(_));
    };
    val uidsWithStandoffAnnots: Seq[(UID, Seq[Seq[Chunk[Seq[String]]]])] = ConllDocReader.readConllDocsGeneral(fileName, fcn);
    new HashMap[UID,Seq[Seq[Chunk[Seq[String]]]]] ++ uidsWithStandoffAnnots;
  }
  
  def writeStandoffAnnots(writer: PrintWriter, docName: String, docPartNo: Int, annots: HashMap[Int,ArrayBuffer[Chunk[Seq[String]]]], sentLens: Seq[Int]) {
    writeStandoffAnnots(writer, docName, docPartNo, (0 until sentLens.size).map(annots(_)), sentLens);
  }
  
  def writeStandoffAnnots(writer: PrintWriter, docName: String, docPartNo: Int, annots: Seq[Seq[Chunk[Seq[String]]]], sentLens: Seq[Int]) {
    val numZeroesToAddToPartNo = 3 - docPartNo.toString.size;
    writer.println("#begin document (" + docName + "); part " + ("0" * numZeroesToAddToPartNo) + docPartNo); 
    for (sentBits <- getWikiBits(sentLens, annots)) {
      for (bit <- sentBits) {
        writer.println(bit);
      }
      writer.println();
    }
    writer.println("#end document");
  }
  
  def wikiTitleSeqToString(titles: Seq[String]): String = {
    if (titles.isEmpty) {
      ExcludeToken
    } else {
      titles.map(_.replace("(", "-LRB-").replace(")", "-RRB-").replace("*", "-STAR-")).reduce(_ + "|" + _);
    }
  }
  
  def stringToWikiTitleSeq(str: String): Seq[String] = {
    if (str == ExcludeToken) {
      Seq[String]();
    } else {
      str.split("\\|").map(_.replace("-LRB-", "(").replace("-RRB-", ")").replace("-STAR-", "*")).toSeq;
    }
  }
  
  def getWikiBits(sentLens: Seq[Int], wikiChunks: Seq[Seq[Chunk[Seq[String]]]]): Seq[Seq[String]] = {
    for (sentIdx <- 0 until sentLens.size) yield {
      for (tokenIdx <- 0 until sentLens(sentIdx)) yield {
        val chunksStartingHere = wikiChunks(sentIdx).filter(chunk => chunk.start == tokenIdx).sortBy(- _.end);
        val numChunksEndingHere = wikiChunks(sentIdx).filter(chunk => chunk.end - 1 == tokenIdx).size;
        var str = "";
        for (chunk <- chunksStartingHere) {
          str += "(" + wikiTitleSeqToString(chunk.label);
        }
        str += "*";
        for (i <- 0 until numChunksEndingHere) {
          str += ")";
        }
        str;
      }
    }
  }
  
  def assembleWikiChunks(bits: Seq[String]) = {
    // Bits have to look like some number of (LABEL followed by * followed by some number of ) 
    val chunks = new ArrayBuffer[Chunk[Seq[String]]]();
    val currStartIdxStack = new ArrayBuffer[Int];
    val currTypeStack = new ArrayBuffer[Seq[String]];
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
        currTypeStack += stringToWikiTitleSeq(bitRemainder.substring(1, endIdx));
        bitRemainder = bitRemainder.substring(endIdx);
      }
      if (containsStar) {
        require(bitRemainder.startsWith("*"), bitRemainder + " " + bits);
        bitRemainder = bitRemainder.substring(1);
      }
      while (bitRemainder.startsWith(")")) {
        require(!currStartIdxStack.isEmpty, "Bad bits: " + bits);
        chunks += new Chunk[Seq[String]](currStartIdxStack.last, i+1, currTypeStack.last);
        currStartIdxStack.remove(currStartIdxStack.size - 1);
        currTypeStack.remove(currTypeStack.size - 1);
        bitRemainder = bitRemainder.substring(1);
      }
      require(bitRemainder.size == 0);
    }
    chunks;
  }
}