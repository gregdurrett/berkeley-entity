package edu.berkeley.nlp.entity.ner
import edu.berkeley.nlp.futile.fig.basic.Indexer
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.futile.util.Logger

case class NerExample(val words: Seq[String],
                      val poss: Seq[String],
                      val goldLabels: Seq[String]) {
  def wordAt(i: Int) = NerExample.wordAt(words, i); 
  def posAt(i: Int) = NerExample.posAt(poss, i);
}

object NerExample {
  def wordAt(words: Seq[String], i: Int) = if (i < 0) "<<START>>" else if (i >= words.size) "<<END>>" else words(i);
  def posAt(poss: Seq[String], i: Int) = if (i < 0) "<<START>>" else if (i >= poss.size) "<<END>>" else poss(i);
}
