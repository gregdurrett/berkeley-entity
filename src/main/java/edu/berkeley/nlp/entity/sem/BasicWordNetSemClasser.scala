package edu.berkeley.nlp.entity.sem

import edu.berkeley.nlp.entity.WordNetInterfacer
import edu.berkeley.nlp.entity.coref.Mention

@SerialVersionUID(1L)
class BasicWordNetSemClasser extends SemClasser {
  def getSemClass(ment: Mention, wni: WordNetInterfacer): String = {
    SemClass.getSemClassNoNer(ment.headStringLc, wni).toString
  }
}
