package edu.berkeley.nlp.entity.preprocess
import edu.berkeley.nlp.entity.ConllDoc
import edu.berkeley.nlp.entity.coref.CorefSystem
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._
import edu.berkeley.nlp.futile.fig.basic.IOUtils
import java.util.regex.Pattern
import scala.collection.mutable.HashSet
import edu.berkeley.nlp.futile.classify.GeneralLogisticRegression
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.util.Counter
import scala.collection.mutable.HashMap
import edu.berkeley.nlp.futile.classify.BinaryExample
import edu.berkeley.nlp.futile.tokenizer.PTBLineLexer
import edu.berkeley.nlp.entity.ConllDocReader

/**
 * An unfaithful reimplementation of the method described in:
 * "Sentence Boundary Detection and the Problem with the U.S."
 * Dan Gillick. NAACL 2009.
 * 
 * Supports training on raw text or CoNLL-formatted data (by
 * de-tokenization and splitting and then learning to re-tokenize
 * and split).
 * 
 * @author gdurrett
 */
class SentenceSplitter(val featureIndexer: Indexer[String], val weights: Array[Double]) {
  
  def formCanonicalizedParagraphs(lines: Array[String], respectInputLineBreaks: Boolean, respectInputTwoLineBreaks: Boolean) = {
    val rawTextLines = lines.toSeq;
    val canonicalizedParagraphs = if (respectInputLineBreaks) {
      rawTextLines.filter(!_.trim.isEmpty).map(SentenceSplitter.canonicalizeLine(_));
    } else if (respectInputTwoLineBreaks) {
      val currPars = new ArrayBuffer[String]();
      var currLine = new StringBuffer();
      for (line <- rawTextLines) {
        if (line.trim.isEmpty) {
          currPars += SentenceSplitter.canonicalizeLine(currLine.toString().trim);
          currLine = new StringBuffer();
        } else {
          currLine = currLine.append(" ").append(line);
        }
      }
      if (!currLine.toString().trim.isEmpty) {
        currPars += SentenceSplitter.canonicalizeLine(currLine.toString().trim);
      }
      currPars;
    } else {
      Seq(SentenceSplitter.canonicalizeLine(rawTextLines.reduce(_ + " " + _)));
    }
    canonicalizedParagraphs.toArray;
  }
  
  def splitSentences(canonicalizedParagraphs: Array[String]) = {
    // Read the text and assemble into paragraphs. This is made more complicated if we try to respect
    // line breaks in the input.
    // Do the sentence splitting and write the output, one sentence per line.
    val goodSplits = new ArrayBuffer[Int];
    val outputLines = new ArrayBuffer[String];
    for (paragraph <- canonicalizedParagraphs) {
      val splits = SentenceSplitter.findPossibleSentenceSplits(paragraph);
      val keptSplits = splits.filter(split => {
        // Go back one to get past after-period space, then find previous space before that
        val prevTokenStartMinusOne = paragraph.substring(0, split - 1).lastIndexOf(" ");
        // Find the first space in the next sentence
        val nextTokenEnd = paragraph.indexOf(" ", split)
        if (prevTokenStartMinusOne == -1 || nextTokenEnd == -1) {
          false;
        } else {
          val splitEx = new SentenceSplitter.SplitExample(paragraph.substring(prevTokenStartMinusOne + 1, split-1), paragraph.substring(split, nextTokenEnd), false);
          val feats = splitEx.featurize(featureIndexer, false);
          val score = feats.map(weights(_)).reduce(_ + _);
          val isPos = score > 0;
          isPos;
        } 
      });
      var currIdx = 0;
      for (split <- keptSplits) {
        outputLines += paragraph.substring(currIdx, split-1)
        currIdx = split;
      }
      outputLines += paragraph.substring(currIdx)
    }
    outputLines.toArray;
  }
}

object SentenceSplitter {
  
  val PuncTokensEndingWord = Set(".", "?", "!", ",", ":", ";", "'", "\"", "``", "''", ")");
  val SentFinalPuncTokens = Set(".", "?", "!");
  val QuoteTokensEndingSents = Set("\"", "''", "``", "'", ")");
  val SentEndPattern = Pattern.compile("[.?!](\"|''|``|'|\\))? ");
  
  case class SplitExample(val prevWord: String, val followingWord: String, val isBoundary: Boolean) {
    
    def featurize(featureIndexer: Indexer[String], addToIndexer: Boolean): Array[Int] = {
      val featStrs = new ArrayBuffer[String];
      val pw = prevWord;
      val fw = followingWord;
      val fwcls = (if (Character.isUpperCase(fw.charAt(0))) "UC" else if (Character.isLowerCase(fw.charAt(0))) "LC" else if (!Character.isLetterOrDigit(fw.charAt(0))) "PU" else "OTHER");
      featStrs += ("Bias=1");
      featStrs += ("LastChar=" + pw.last);
      featStrs += ("FirstChar=" + fw.head);
      featStrs += ("LastCharFirstChar=" + pw.last + "-" + fw.head);
      featStrs += ("Prev=" + pw);
      featStrs += ("Next=" + fw);
      featStrs += ("PrevLen=" + pw.size);
      featStrs += ("NextCls=" + fwcls);
      featStrs += ("LastCharNextCls=" + pw.last + "-" + fwcls);
      featStrs += ("SecondToLastCharNextCls=" + (if (pw.size >= 2) pw(pw.size - 2)) + "-" + fwcls);
      featStrs += ("PrevWordNextCls=" + pw + "-" + fwcls);
      // From Gillick but unused
//      featStrs += ("PrevWordNextWord=" + pw + "-" + fw);
      val featInts = new ArrayBuffer[Int]();
      for (str <- featStrs) {
        if (addToIndexer) {
          featInts += featureIndexer.getIndex(str);
        } else {
          val idx = featureIndexer.indexOf(str);
          if (idx != -1) {
            featInts += idx;
          }
        }
      }
      featInts.toArray;
    }
  }
  
  def loadSentenceSplitter(modelPath: String) = {
    val modelReader = IOUtils.openInHard(modelPath)
    val featureIndexer = new Indexer[String]();
    val weightsBuf = new ArrayBuffer[Double]()
    while (modelReader.ready()) {
      val line = modelReader.readLine();
      val lineSplit = line.split("\\s+");
      require(lineSplit.size == 2);
      featureIndexer.getIndex(lineSplit(0));
      weightsBuf += lineSplit(1).toDouble;
    }
    val weights = weightsBuf.toArray;
    new SentenceSplitter(featureIndexer, weights);
  }

  def tokenize(sentences: Array[String]): Array[Array[String]] = {
    val tokenizer = new PTBLineLexer();
    sentences.map(sentence => tokenizeSingle(sentence, tokenizer));
  }
  
  def tokenizeSingle(sentence: String): Array[String] = {
    tokenizeSingle(sentence, new PTBLineLexer());
  }
  
  def tokenizeSingle(sentence: String, tokenizer: PTBLineLexer): Array[String] = {
    // The split step is because very occasionally the tokenizer appears to keep
    // spaces within tokens
    tokenizer.tokenize(sentence).asScala.flatMap(_.split("\\s+")).toArray;
  }
  
  def tokenizeAlternate(sentences: Array[String]): Array[Array[String]] = {
    val tokenizer = new CustomPTBTokenizer();
    sentences.map(tokenizer.tokenize(_));
  }
  
  def tokenizeAlternateSingle(sentence: String): Array[String] = {
    new CustomPTBTokenizer().tokenize(sentence);
  }
  
  def tokenizeWhitespace(sentences: Array[String]): Array[Array[String]] = {
    sentences.map(sentence => tokenizeWhitespaceSingle(sentence));
  }
  
  def tokenizeWhitespaceSingle(sentence: String): Array[String] = {
    sentence.split("\\s+")
  }
  
  private def canonicalizeLine(line: String) = {
    line.replaceAll("“", "``").replaceAll("”", "''").replaceAll("\"", "''").replaceAll("’", "'").replaceAll("\\s+", " ");
  }
  
  private def treatConllWord(word: String) = {
    // Return parens and stuff to their normal state, 
    word.replaceAll("-LRB-", "(").replaceAll("-RRB-", ")").replaceAll("-LSB-", "[").replaceAll("-RSB-", "]").
      replaceAll("/\\.", ".").replaceAll("/\\?", "?").replaceAll("/!", "!");
  }
  
  private def findPossibleSentenceSplits(line: String): Seq[Int] = {
    val matcher = SentenceSplitter.SentEndPattern.matcher(line);
    val possibleIndices = new ArrayBuffer[Int]();
    var nextMatchIdx = 0;
    while (matcher.find(nextMatchIdx)) {
      nextMatchIdx = matcher.end();
      possibleIndices += nextMatchIdx;
    }
    possibleIndices;
  }
  
  def trainSentenceSplitter {
    // Extract examples
    val (trainExamples, testExamples) = if (SentenceSplitterTokenizerDriver.trainFromConll) {
      (readExamplesFromConll(ConllDocReader.loadRawConllDocsWithSuffix(SentenceSplitterTokenizerDriver.conllTrainPath, SentenceSplitterTokenizerDriver.conllTrainSize, "auto_conll")),
       readExamplesFromConll(ConllDocReader.loadRawConllDocsWithSuffix(SentenceSplitterTokenizerDriver.conllTestPath, SentenceSplitterTokenizerDriver.conllTestSize, "auto_conll")));
    } else {
      (readExamplesFromLines(Source.fromFile(SentenceSplitterTokenizerDriver.trainPath).getLines.toSeq),
       readExamplesFromLines(Source.fromFile(SentenceSplitterTokenizerDriver.trainPath).getLines.toSeq));
    }
    Logger.logss(trainExamples.size + " train examples (" + trainExamples.filter(_.isBoundary).size + " positive), " +
                 testExamples.size + " test examples (" + testExamples.filter(_.isBoundary).size + " positive)");
    Logger.logss("Some positive examples: " + trainExamples.filter(_.isBoundary).slice(0, 100));
    Logger.logss("Some negative examples: " + trainExamples.filter(!_.isBoundary).slice(0, 100));
    // Featurize examples
    val featureIndexer = new Indexer[String]();
    val trainBinaryExs = trainExamples.map(splitEx => {
      new BinaryExample(splitEx.featurize(featureIndexer, true), splitEx.isBoundary);
    });
    val testBinaryExs = testExamples.map(splitEx => {
      new BinaryExample(splitEx.featurize(featureIndexer, false), splitEx.isBoundary);
    });
    val featsByType = featureIndexer.getObjects().asScala.groupBy(str => str.substring(0, str.indexOf("=")));
    Logger.logss(featureIndexer.size + " features");
    Logger.logss("Num feats each type:\n" + featsByType.map(pair => pair._1 + ": " + pair._2.size).reduce(_ + "\n" + _));
    // Train
    val weights = new Array[Double](featureIndexer.size);
    new GeneralLogisticRegression(false, false).trainWeightsLbfgsL2R(trainBinaryExs.asJava, 0.1, 0.01, 50, weights);
    // Evaluate on the test set
    var numCorrect = 0.0;
    var total = 0.0
    val confusions = new Counter[String]();
    for (i <- 0 until testBinaryExs.size) {
      val testEx = testBinaryExs(i);
      val isPos = testEx.feats.map(weights(_)).reduce(_ + _) > 0;
      if ((isPos && testEx.isPositive) || (!isPos && !testEx.isPositive)) {
        numCorrect += 1;
      } else {
        Logger.logss("ERROR: " + testExamples(i));
      }
      confusions.incrementCount(isPos + "-" + testEx.isPositive, 1.0);
      total += 1;
    }
    Logger.logss("ACCURACY: " + numCorrect + "/" + total);
    Logger.logss("CONFUSIONS: " + confusions);
    val modelWriter = IOUtils.openOutHard(SentenceSplitterTokenizerDriver.modelPath);
    for (i <- 0 until featureIndexer.size) {
      modelWriter.println(featureIndexer.get(i) + " " + weights(i));
    }
    modelWriter.close();
  }
  
  
  private def readExamplesFromConll(docs: Seq[ConllDoc]): Seq[SplitExample] = {
    // N.B. we only loop up until size - 1 since the end of the last sentence
    // has no following context and isn't a good training example.
    // We extract pretty much all positives except for really weird stuff.
    // (things like -- and : as sentence splitters, etc.)
    val examples = new ArrayBuffer[SplitExample]();
    var numLinesExtracted = 0;
    var numLinesPossibleExtraction = 0;
    val finalDocs = docs.filter(_.docID.startsWith("nw"));
    Logger.logss("Filtered from " + docs.size + " docs down to " + finalDocs.size + " (only kept newswire)");
    for (doc <- finalDocs; lineIdx <- 0 until doc.words.size) {
      val wordsLine = doc.words(lineIdx).map(word => treatConllWord(word));
      val numDocLines = doc.words.size;
      // Does this line hold a good example?
      if (lineIdx < doc.words.size - 1) {
        numLinesPossibleExtraction += 1;
        val wordsNextLine = doc.words(lineIdx+1);
        val endsWithGoodPunc = SentFinalPuncTokens.contains(wordsLine.last);
        val endsWithGoodPuncAndQuote = wordsLine.size >= 2 && SentFinalPuncTokens.contains(wordsLine(wordsLine.size - 2)) && QuoteTokensEndingSents.contains(wordsLine.last);
        val nextToken = getUntokenizedStartingAtIndex(wordsNextLine, 0);
//        val nextToken = wordsNextLine(0);
        // We can only extract a good training example if 
        if (endsWithGoodPunc && wordsLine.size >= 2) {
          numLinesExtracted += 1;
          examples += new SplitExample(wordsLine(wordsLine.size - 2) + wordsLine.last, nextToken, true);
        } else if (endsWithGoodPuncAndQuote && wordsLine.size >= 3) {
          numLinesExtracted += 1;
          examples += new SplitExample(wordsLine(wordsLine.size - 3) + wordsLine(wordsLine.size - 2) + wordsLine.last, nextToken, true);
        }
      }
      // Extract matches within the sentence.
      // N.B. wordsLine.size - 1; 
      for (i <- 0 until wordsLine.size - 1) {
        val word = wordsLine(i);
        val isGoodPunc = SentFinalPuncTokens.contains(word);
        val thisTokenEndsWithGoodPunc = SentFinalPuncTokens.contains(word.substring(word.size - 1));
        val isGoodPuncAndQuote = i > 0 && SentFinalPuncTokens.contains(wordsLine(i-1)) && QuoteTokensEndingSents.contains(word);
        val tokenEndsWithGoodPuncThenQuote = i > 0 && SentFinalPuncTokens.contains(wordsLine(i-1).substring(wordsLine(i-1).size - 1)) && QuoteTokensEndingSents.contains(word);
        val nextToken = getUntokenizedStartingAtIndex(wordsLine, i+1);
//        val nextToken = wordsLine(i+1);
        if (isGoodPunc && i > 0) {
          examples += new SplitExample(wordsLine(i - 1) + word, nextToken, false);
        } else if (thisTokenEndsWithGoodPunc) {
          examples += new SplitExample(word, nextToken, false);
        } else if (isGoodPuncAndQuote && i >= 2) {
          examples += new SplitExample(wordsLine(i - 2) + wordsLine(i - 1) + word, nextToken, false);
        } else if (tokenEndsWithGoodPuncThenQuote && i > 0) {
          examples += new SplitExample(wordsLine(i - 1) + word, nextToken, false);
        }
      }
    }
    Logger.logss("Positive examples extracted from " + numLinesExtracted + "/" + numLinesPossibleExtraction + " lines");
    examples;
  }
  
  private def getUntokenizedStartingAtIndex(sentence: Seq[String], startIdx: Int) = {
    require(sentence.size >= startIdx + 1);
    val endIdx = if (sentence.size >= startIdx + 2 && QuoteTokensEndingSents.contains(sentence(startIdx))) {
      if (sentence.size >= startIdx + 3 && PuncTokensEndingWord.contains(sentence(startIdx + 2))) {
        startIdx + 3
      } else {
        startIdx + 2;
      }
    } else {
      startIdx + 1;
    }
    sentence.slice(startIdx, endIdx).reduce(_ + "" + _);
  }
  
  private def readExamplesFromLines(lines: Seq[String]) = {
    val examples = new ArrayBuffer[SplitExample]();
    for (line <- lines) {
      if (!line.trim.isEmpty) {
        val splitLine = line.split("\\s+");
        examples == new SplitExample(splitLine(0), splitLine(1), splitLine(2) == "1");
      }
    }
    examples;
  }
  
  def main(args: Array[String]) {
    require(SentEndPattern.matcher("stuff. A").find());
    require(SentEndPattern.matcher("stuff! A").find());
    require(SentEndPattern.matcher("stuff? A").find());
    require(!SentEndPattern.matcher("stuff; a").find());
    require(SentEndPattern.matcher("stuff.\" A").find());
    require(SentEndPattern.matcher("stuff!\" A").find());
    require(SentEndPattern.matcher("stuff?\" A").find());
    require(!SentEndPattern.matcher("stuff;\" a").find());
    require(SentEndPattern.matcher("stuff.'' A").find());
    require(SentEndPattern.matcher("stuff!'' A").find());
    require(SentEndPattern.matcher("stuff?'' A").find());
  }
}
