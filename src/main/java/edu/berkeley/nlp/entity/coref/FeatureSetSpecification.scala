package edu.berkeley.nlp.entity.coref

import edu.berkeley.nlp.futile.util.Logger

@SerialVersionUID(1L)
case class FeatureSetSpecification(val featsToUse: Set[String],
                                   val conjScheme: ConjScheme,
                                   val conjFeatures: ConjFeatures,
                                   val conjListedTypePairs: Set[(MentionType,MentionType)],
                                   val conjListedTemplates: Set[String])

object FeatureSetSpecification {
  
  def apply(featsToUse: String,
            conjScheme: ConjScheme,
            conjFeatures: ConjFeatures): FeatureSetSpecification = {
    apply(featsToUse, conjScheme, conjFeatures, "", "");
  }
  
  def apply(featsToUse: String,
            conjScheme: ConjScheme,
            conjFeatures: ConjFeatures,
            conjListedTypePairs: String,
            conjListedTemplates: String): FeatureSetSpecification = {
    val typePairs = conjListedTypePairs.split("\\+").toIndexedSeq.filter(!_.isEmpty).map(entry => MentionType.valueOf(entry.split("-")(0)) -> MentionType.valueOf(entry.split("-")(1))).toSet;
    val templates = conjListedTemplates.split("\\+").filter(!_.isEmpty).toSet;
    Logger.logss("Feature set: " + featsToUse + "\n" + conjScheme + "\n" + conjFeatures + "\nType pairs: " + typePairs + "\nTemplates: " + templates)
    new FeatureSetSpecification(featsToUse.split("\\+").toSet, conjScheme, conjFeatures, typePairs, templates);
  }
}
