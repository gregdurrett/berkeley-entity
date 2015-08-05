package edu.berkeley.nlp.entity.coref

trait AuxiliaryFeaturizer extends Serializable {
  def featurize(docGraph: DocumentGraph, currMentIdx: Int, antecedentIdx: Int): Seq[String];
}