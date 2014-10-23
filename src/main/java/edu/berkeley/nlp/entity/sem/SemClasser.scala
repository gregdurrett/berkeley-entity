package edu.berkeley.nlp.entity.sem

import edu.berkeley.nlp.entity.coref.Mention
import edu.berkeley.nlp.entity.WordNetInterfacer
import edu.berkeley.nlp.entity.coref.CorefDoc

trait SemClasser extends Serializable {
  // We only bother to define these for NOMINAL and PROPER mentions; it shouldn't be
  // called for anything else
  def getSemClass(ment: Mention, wni: WordNetInterfacer): String;
}
