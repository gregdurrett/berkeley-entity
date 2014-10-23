package edu.berkeley.nlp.entity.wiki;

import info.bliki.wiki.filter.PlainTextConverter;
import info.bliki.wiki.model.WikiModel;
import edu.berkeley.nlp.futile.util.Logger;


public class BlikiInterface {
  
  public static String renderPlaintext(String text) {
    // Remove references or they will endure after processing
    String normalizedText = removeReferences(normalizeGtLtQuot(text));
    WikiModel wikiModel = new WikiModel("http://www.mywiki.com/wiki/${image}", "http://www.mywiki.com/wiki/${title}");
    String plainText = wikiModel.render(new PlainTextConverter(), normalizedText);
    return removeCurlyBrackets(plainText);
  }
  
  public static String normalizeGtLtQuot(String line) {
    return line.replaceAll("&lt;", "<").replaceAll("&gt;", ">").replaceAll("&quot;", "\"");
  }
  
  public static String removeComments(String line) {
    return removeDelimitedContent(line, "<!--", "-->");
  }
  
  public static String removeReferences(String line) {
    return removeDelimitedContent(line, "<ref", "</ref>");
  }

  public static String removeSquareBrackets(String line) {
    return removeDelimitedContent(line, "[[", "]]");
  }
  
  public static String removeCurlyBrackets(String line) {
    return removeDelimitedContent(line, "{{", "}}");
  }
  
  public static String removeParentheticals(String line) {
    return removeDelimitedContent(line, "(", ")");
  }

  private static String removeDelimitedContent(String line, String startDelim, String endDelim) {
    String newLine = line;
    int endIdx = line.indexOf(endDelim) + endDelim.length();
    int startIdx = line.lastIndexOf(startDelim, endIdx - endDelim.length());
//    System.out.println(startIdx + " " + endIdx);
    while (startIdx >= 0 && endIdx >= 0 && endIdx > startIdx) {
      newLine = newLine.substring(0, startIdx) + newLine.substring(endIdx);
      endIdx = newLine.indexOf(endDelim) + endDelim.length();
      startIdx = newLine.lastIndexOf(startDelim, endIdx - endDelim.length());
    }
    return newLine;
  }
  
  
//  public static final String TEST = "This is a [[Hello World]] '''example'''";
  public static final String TEST = "'''Autism''' is a [[Neurodevelopmental disorder|disorder of neural development]] characterized by impaired [[Interpersonal relationship|social interaction]] and [[verbal communication|verbal]] and [[non-verbal communication]], and by restricted, repetitive or [[stereotypy|stereotyped]] behavior.";

  public static void main(String[] args) {
    WikiModel wikiModel = new WikiModel("http://www.mywiki.com/wiki/${image}", "http://www.mywiki.com/wiki/${title}");
    String plainStr = wikiModel.render(new PlainTextConverter(), TEST);
    System.out.print(plainStr);
  }
  
}
