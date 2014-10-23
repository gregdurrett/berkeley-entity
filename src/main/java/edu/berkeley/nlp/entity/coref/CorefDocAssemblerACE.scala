package edu.berkeley.nlp.entity.coref

import edu.berkeley.nlp.entity.lang.EnglishCorefLanguagePack
import edu.berkeley.nlp.futile.util.Logger
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.wiki.ACEMunger
import java.io.File
import edu.berkeley.nlp.entity.ConllDoc

class CorefDocAssemblerACE(dirPath: String) {
  
  val langPack = new EnglishCorefLanguagePack()

  def createCorefDoc(rawDoc: ConllDoc, propertyComputer: MentionPropertyComputer): CorefDoc = {
    val (goldMentions, goldClustering) = CorefDocAssembler.extractGoldMentions(rawDoc, propertyComputer, langPack);
    if (goldMentions.size == 0) {
      Logger.logss("WARNING: no gold mentions on document " + rawDoc.printableDocName);
    }
    // TODO: Load pred mentions here
    val mentSpansEachSent: Seq[Seq[(Int,Int)]] = ACEMunger.getPredMentionsBySentence(new File(dirPath + "/" + rawDoc.docID));
    val predMentions = new ArrayBuffer[Mention]();
    for (sentIdx <- 0 until mentSpansEachSent.size; mentSpan <- mentSpansEachSent(sentIdx)) {
      val headIdx = rawDoc.trees(sentIdx).getSpanHead(mentSpan._1, mentSpan._2);
      predMentions += Mention.createMentionComputeProperties(rawDoc, predMentions.size, sentIdx, mentSpan._1, mentSpan._2, headIdx, Seq(headIdx), false, propertyComputer, langPack)
    }
    
    Logger.logss(rawDoc.docID);
    for (i <- 0 until rawDoc.numSents) {
      Logger.logss(goldMentions.filter(_.sentIdx == i).map(ment => ment.startIdx -> ment.endIdx));
      Logger.logss(goldMentions.filter(_.sentIdx == i).map(ment => ment.words));
      Logger.logss(predMentions.filter(_.sentIdx == i).map(ment => ment.startIdx -> ment.endIdx));
      Logger.logss(predMentions.filter(_.sentIdx == i).map(ment => ment.words));
    }
    
    new CorefDoc(rawDoc, goldMentions, goldClustering, predMentions)
  }
}
