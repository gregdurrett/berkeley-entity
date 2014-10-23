package edu.berkeley.nlp.entity.ner

import edu.berkeley.nlp.entity.ConllDocReader
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.entity.ConllDoc
import edu.berkeley.nlp.futile.syntax.Trees.PennTreeRenderer
import edu.berkeley.nlp.futile.fig.basic.IOUtils

object NESentenceMunger {
  
  def writeSentences(file: String, docs: Seq[ConllDoc]) {
    val out = IOUtils.openOutHard(file);
    for (doc <- docs; words <- doc.words) {
      out.println(words.foldLeft("")(_ + " " + _).trim);
    }
    out.close();
  }

  def main(args: Array[String]) {
    val devDocs = ConllDocReader.loadRawConllDocsWithSuffix("data/conll-2012-en/dev/", -1, "gold_conll");
    writeSentences("data/conll-2012-dev.sentences", devDocs);
    val testDocs = ConllDocReader.loadRawConllDocsWithSuffix("data/conll-2012-en/test/", -1, "gold_conll");
    writeSentences("data/conll-2012-test.sentences", testDocs);
    val dev2011Docs = ConllDocReader.loadRawConllDocsWithSuffix("data/ontonotes-conll/dev/", -1, "gold_conll");
    writeSentences("data/conll-2011-dev.sentences", dev2011Docs);
    val test2011Docs = ConllDocReader.loadRawConllDocsWithSuffix("data/ontonotes-conll/test/", -1, "gold_conll");
    writeSentences("data/conll-2011-test.sentences", test2011Docs);
  }
}
