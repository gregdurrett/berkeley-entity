package edu.berkeley.nlp.entity.lang;

import edu.berkeley.nlp.futile.treebank.AbstractTreebankLanguagePack;


public class ArabicTreebankLanguagePack extends AbstractTreebankLanguagePack {
  private static final String[] collinsPunctTags = {"PUNC"};

  private static final String[] pennPunctTags = {"PUNC"};

  private static final String[] pennPunctWords = {".","\"",",","-LRB-","-RRB-","-",":","/","?","_","*","%","!",">","-PLUS-","...",";","..","&","=","ر","'","\\","`","......"};
  
  private static final String[] pennSFPunctTags = {"PUNC"};

  private static final String[] pennSFPunctWords = {".", "!", "?"};

  /**
   * The first 3 are used by the Penn Treebank; # is used by the
   * BLLIP corpus, and ^ and ~ are used by Klein's lexparser.
   * Chris deleted '_' for Arabic as it appears in tags (NO_FUNC).
   * June 2006: CDM tested _ again with true (new) Treebank tags to see if it
   * was useful for densening up the tag space, but the results were negative.
   * Roger added + for Arabic but Chris deleted it again, since unless you've
   * recoded determiners, it screws up DET+NOUN, etc.  (That is, it would only be useful if
   * you always wanted to cut at the first '+', but in practice that is not viable, certainly
   * not with the IBM ATB processing either.)
   */
  private static final char[] annotationIntroducingChars = {'-', '=', '|', '#', '^', '~'};

  /**
   * This is valid for "BobChrisTreeNormalizer" conventions only. 
   * wsg: "ROOT" should always be the first value. See {@link #startSymbol} in
   * the parent class.
   */
  private static final String[] pennStartSymbols = {"ROOT"};


  /**
   * Returns a String array of punctuation tags for this treebank/language.
   *
   * @return The punctuation tags
   */
  @Override
  public String[] punctuationTags() {
    return pennPunctTags;
  }


  /**
   * Returns a String array of punctuation words for this treebank/language.
   *
   * @return The punctuation words
   */
  @Override
  public String[] punctuationWords() {
    return pennPunctWords;
  }


  /**
   * Returns a String array of sentence final punctuation tags for this
   * treebank/language.
   *
   * @return The sentence final punctuation tags
   */
  @Override
  public String[] sentenceFinalPunctuationTags() {
    return pennSFPunctTags;
  }

  /**
   * Returns a String array of sentence final punctuation words for this
   * treebank/language.
   *
   * @return The sentence final punctuation tags
   */
  public String[] sentenceFinalPunctuationWords() {
    return pennSFPunctWords;
  }
  
  /**
   * Returns a String array of treebank start symbols.
   *
   * @return The start symbols
   */
  @Override
  public String[] startSymbols() {
    return pennStartSymbols;
  }

  /**
   * Returns the extension of treebank files for this treebank.
   * This is "tree".
   */
  public String treebankFileExtension() {
    return "tree";
  }
}
