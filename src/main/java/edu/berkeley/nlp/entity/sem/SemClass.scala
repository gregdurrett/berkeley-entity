package edu.berkeley.nlp.entity.sem

import edu.berkeley.nlp.entity.WordNetInterfacer
import edu.berkeley.nlp.entity.coref.Mention
import edu.mit.jwi.item.ISynsetID
import edu.berkeley.nlp.entity.coref.NumberGenderComputer
import edu.berkeley.nlp.entity.coref.CorefSystem
import edu.berkeley.nlp.entity.coref.DocumentGraph
import java.util.regex.Pattern
import edu.berkeley.nlp.futile.util.Counter
import java.io.File
import scala.collection.mutable.HashMap
import edu.mit.jwi.item.ISynset

object SemClass extends Enumeration {
  type SemClass = Value
  val Person, Location, Organization, Date, Event, Other = Value
  
  // These do slightly less well than well-trained NER tags
//  val DateWords = Set("today", "yesterday", "tomorrow", "day", "days", "week", "weeks", "month", "months", "year", "years",
//                      "january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december");
//  val DatePattern = Pattern.compile("[0-9]{4}");
  
  def getSemClass(headStringLc: String, wni: WordNetInterfacer): SemClass = getSemClass(headStringLc, "", wni);
  
  def getSemClassNoNer(headStringLc: String, wni: WordNetInterfacer): SemClass = getSemClass(headStringLc, "", wni);
  
  def getSemClassNoNer(synsets: Seq[ISynset], wni: WordNetInterfacer): SemClass = {
    if (wni.isAnySynsetHypernym(synsets, wni.personSynset, 10)) {
      SemClass.Person;
    } else if (wni.isAnySynsetHypernym(synsets, wni.locationSynset, 10)) {
      SemClass.Location;
    } else if (wni.isAnySynsetHypernym(synsets, wni.organizationSynset, 10)) {
      SemClass.Organization;
    } else {
      SemClass.Other;
    }
  }
  
  def getSemClassOnlyNer(nerString: String): SemClass = {
    if (nerString == "PERSON") {
      SemClass.Person
    } else if (nerString == "LOC") {
      SemClass.Location
    } else if (nerString == "ORG" || nerString == "GPE") {
      SemClass.Organization
    } else if (nerString == "DATE" || nerString == "TIME") {
      SemClass.Date;
    } else {
      SemClass.Other;
    }
  }
  
  def getStrSemClassOnlyNerFiner(nerString: String): String = {
    if (nerString == "PERSON" || nerString == "LOC" || nerString == "ORG" || nerString == "GPE" || nerString == "NORP" || nerString == "DATE" || nerString == "CARDINAL") {
      nerString
    } else {
      "OTHER"
    }
  }
  
  def getSemClass(headStringLc: String, nerString: String, wni: WordNetInterfacer): SemClass = {
    if (wni.isAnySynsetHypernym(headStringLc, wni.personSynset) || nerString == "PERSON") {
      SemClass.Person;
    } else if (wni.isAnySynsetHypernym(headStringLc, wni.locationSynset) || nerString == "LOC") {
      SemClass.Location;
    } else if (wni.isAnySynsetHypernym(headStringLc, wni.organizationSynset) || nerString == "ORG" || nerString == "GPE") {
      SemClass.Organization;
      // Event seems unhelpful based on an experiment, and it makes for even more features
//    } else if (wni.isAnySynsetHypernym(headStringLc, wni.eventSynset) || nerString == "EVENT") {
//      SemClass.Event;
    } else if (nerString == "DATE" || nerString == "TIME") {
      SemClass.Date;
    } else {
      SemClass.Other;
    }
  }
  
  def isDate(headStringLc: String) = false //DateWords.contains(headStringLc) || DatePattern.matcher(headStringLc).matches();
  
}
