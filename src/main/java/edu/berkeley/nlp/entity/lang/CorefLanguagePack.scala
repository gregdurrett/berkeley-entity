package edu.berkeley.nlp.entity.lang

trait CorefLanguagePack {
  def getMentionConstituentTypes: Seq[String];
  def getPronominalTags: Seq[String];
  def getProperTags: Seq[String];
}

class EnglishCorefLanguagePack extends CorefLanguagePack {
  def getMentionConstituentTypes: Seq[String] = Seq("NP", "NML");
  def getPronominalTags: Seq[String] = Seq("PRP", "PRP$");
  def getProperTags: Seq[String] = Seq("NNP");
}

class ChineseCorefLanguagePack extends CorefLanguagePack {
  def getMentionConstituentTypes: Seq[String] = Seq("NP");
  def getPronominalTags: Seq[String] = Seq("PN");
  def getProperTags: Seq[String] = Seq("NR");
}

class ArabicCorefLanguagePack extends CorefLanguagePack {
  def getMentionConstituentTypes: Seq[String] = Seq("NP");
  def getPronominalTags: Seq[String] = Seq("PRP", "PRP$");
  def getProperTags: Seq[String] = Seq("NNP");
}
