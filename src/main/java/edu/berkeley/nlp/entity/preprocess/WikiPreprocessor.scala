package edu.berkeley.nlp.entity.preprocess

import java.io.File

import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser
import edu.berkeley.nlp.entity.{Chunk, WikiDocReader}
import edu.berkeley.nlp.entity.ner.NerSystemLabeled
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.syntax.Tree

import scala.xml._
import scala.concurrent._
import scala.collection.JavaConverters._

import ExecutionContext.Implicits.global

/**
 * Created by matthew on 2/21/15.
 */
object WikiPreprocessor {

  def processesDocs (inputDir : String, outputDir : String,
                     docReader : WikiDocReader,
                     splitter : SentenceSplitter,
                     parser : CoarseToFineMaxRuleParser,
                     backoffParser : CoarseToFineMaxRuleParser,
                     nerSystem : NerSystemLabeled) = {
    new File(inputDir).listFiles.map(file => {
      val input_file = file.getAbsolutePath
      val output_file = outputDir + file.getName
      //Future {
        process(input_file, output_file, docReader, splitter, parser, backoffParser, nerSystem)
      //}
    })//.foreach(Await.result(_, duration.Duration.Inf))
  }

  def process(inputFile : String, outputFile : String,
              docReader : WikiDocReader,
              splitter : SentenceSplitter,
              parser : CoarseToFineMaxRuleParser,
              backoffParser : CoarseToFineMaxRuleParser,
              nerSystem : NerSystemLabeled) = {
    /*String docName = inputPath;
    String[] lines = IOUtils.readLinesHard(inputPath).toArray(new String[0]);
    String[] canonicalizedParagraphs = splitter.formCanonicalizedParagraphs(lines, respectInputLineBreaks, respectInputTwoLineBreaks);
    String[] sentences = null;
    if (skipSentenceSplitting) {
      sentences = canonicalizedParagraphs;
    } else {
      sentences = splitter.splitSentences(canonicalizedParagraphs);
    }
    String[][] tokenizedSentences = (useAlternateTokenizer ? splitter.tokenizeAlternate(sentences) : splitter.tokenize(sentences));
    Logger.logss("Document " + docName + " contains " + lines.length + " lines and " + tokenizedSentences.length + " sentences");
    String[][] docConllLines = renderDocConllLines(docName, tokenizedSentences, parser, backoffParser, nerSystem);
    writeConllLines(docName, docConllLines, outputPath);
*/

     /*
        String[][] conllLines = new String[tokenizedSentences.length][];
    for (int sentIdx = 0; sentIdx < tokenizedSentences.length; sentIdx++) {
      String[] tokenizedSentence = tokenizedSentences[sentIdx];
      Tree<String> parse = parse(parser, backoffParser, Arrays.asList(tokenizedSentence));
      if (parse.getYield().size() != tokenizedSentence.length) {
        Logger.logss("WARNING: couldn't parse sentence, dropping it: " + Arrays.toString(tokenizedSentence));
        Logger.logss("  (This will be fixed to backing off to an X-bar grammar in a future release)");
      } else {
        String[] posTags = new String[tokenizedSentence.length];
        List<String> preterminals = parse.getPreTerminalYield();
        for (int i = 0; i < preterminals.size(); i++) {
          posTags[i] = preterminals.get(i);
        }
        String[] nerBioLabels = null;
        if (nerSystem != null) {
          nerBioLabels = nerSystem.tagBIO(tokenizedSentence, posTags);
        } else {
          nerBioLabels = new String[tokenizedSentence.length];
          Arrays.fill(nerBioLabels, "O");
        }
        conllLines[sentIdx] = renderSentenceConllLines(docName, 0, tokenizedSentence, posTags, parse, nerBioLabels);
      }
    }
    return conllLines;

    */

    Logger.logss("starting processing of " + inputFile)
    val referencesFile = inputFile.replace("RawTexts", "Problems")
    val refxml = XML.loadFile(referencesFile)
    val document = scala.io.Source.fromFile(inputFile).mkString.split("\n")

    val references = (refxml \ "ReferenceInstance").map(r => (
      (r \ "SurfaceForm")(0).text.trim,
      (r \ "Offset")(0).text.trim.toInt,
      (r \ "Length")(0).text.trim.toInt,
      (r \ "ChosenAnnotation")(0).text.trim,
      (r \ "AnnotatorId")(0).text.trim,
      (r \ "Annotation")(0).text.trim
      ))

    val canonicalizedParagraphs = splitter.formCanonicalizedParagraphs(document, false, false)
    val sentences = splitter.splitSentences(canonicalizedParagraphs)
    val tokens = SentenceSplitter.tokenize(sentences)


    val doclenratio = sentences.map(_.size).sum.toFloat / document.map(_.size + 1).sum
    def refFinder (ref : (String, Int, Int, String, String, String)) : (Int, Chunk[String]) = {
      val d = doclenratio * (ref._2 + ref._3 / 2.0)
      var cnt = 0
      val wrds = ref._1.replace(" ", "")
      def rank_match(i : Int, j : Int) : Double = {
        val res = tokens(i).drop(j).reduce(_+_)
        for(q <- 0 until Math.min(wrds.size, res.size)) {
          if (res(q) != wrds(q))
            return q.toDouble / wrds.size
        }
        1.0
      }
      for(i <- 0 to sentences.size) {
        cnt += sentences(i).size
        if(cnt > d) {
          // assume that the reference is in this sentence
          var ll = cnt - sentences(i).size + d // estimated place in sentence
          var tcnt = 0
          var best_start = 0
          var best_rank = Double.NegativeInfinity

          for(j <- 0 until tokens(i).size) {
            val r = rank_match(i,j) * Math.abs(ll - tcnt) // try and make the item close to where it should be
            if(r > best_rank) {
              best_start = j
              best_rank = r
            }
            tcnt += tokens(i)(j).size
          }
          var len = 0
          var len_cnt = 0
          for(j <- best_start until tokens(i).size; if len_cnt < wrds.size) {
            len_cnt += tokens(i)(j).size
            len += 1
          }
          return (i, new Chunk(best_start, best_start + len, ref._4))
        }
      }
      (-1, null)
    }

    val refplaces = references.map(refFinder)

    val refsorted = refplaces.foldLeft(Map[Int, List[Chunk[String]]]().withDefaultValue(List()))((m, itm) => {
      if(itm._1 != -1) {
        m.updated(itm._1, m(itm._1) :+ itm._2)
      } else
        m
    })

    val parses : Array[Tree[String]] = tokens.map(t => PreprocessingDriver.parse(parser, backoffParser, t.toList.asJava))
    // ... filter out the ones where the parses don't match, idk how that is going to effect
    var tps = (tokens zip parses).filter((t) => t._1.length == t._2.getYield.size)

    Logger.logss("done with "+inputFile)




  }

}
