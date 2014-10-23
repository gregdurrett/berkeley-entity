package edu.berkeley.nlp.entity.coref;

// REALLY_ONLY_FINE = don't even add the completely unconjoined features
public enum ConjType {
  NONE, TYPE, TYPE_OR_RAW_PRON, CANONICAL_NOPRONPRON,
     CANONICAL, CANONICAL_AND_SEM,
     CANONICAL_ONLY_FINE, CANONICAL_AND_SEM_ONLY_FINE,
     CANONICAL_AND_SEM_REALLY_ONLY_FINE;
}
