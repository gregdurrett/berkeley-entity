package edu.berkeley.nlp.entity.preprocess;

import java.io.File;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.List;

import edu.berkeley.nlp.PCFGLA.CoarseToFineMaxRuleParser;
import edu.berkeley.nlp.PCFGLA.Grammar;
import edu.berkeley.nlp.PCFGLA.Lexicon;
import edu.berkeley.nlp.PCFGLA.ParserData;
import edu.berkeley.nlp.PCFGLA.TreeAnnotations;
import edu.berkeley.nlp.entity.ConllDocJustWords;
import edu.berkeley.nlp.entity.ConllDocReader;
import edu.berkeley.nlp.entity.lang.Language;
import edu.berkeley.nlp.entity.ner.NerSystemLabeled;
import edu.berkeley.nlp.futile.fig.basic.IOUtils;
import edu.berkeley.nlp.futile.fig.basic.Option;
import edu.berkeley.nlp.futile.fig.exec.Execution;
import edu.berkeley.nlp.futile.util.Logger;
import edu.berkeley.nlp.syntax.Tree;
import edu.berkeley.nlp.util.Numberer;

/**
 * Preprocessing pipeline that can sentence-split, tokenize, parse, and optionally
 * NER tag data. It can operate from raw text (RAW_TEXT) or from documents that
 * are in a CoNLL skeleton with words but no other predicted annotations
 * (in which case only the parser and NER system will be run since the data is already
 * tokenized and sentence split).
 * 
 * The models used are as follows:
 * 
 * Sentence-splitting: Binary classifier after Gillick (2009) (edu.berkeley.nlp.entity.preprocess.SentenceSplitter)
 * 
 * Tokenization: Penn Treebank tokenizer
 * 
 * Parsing: Berkeley Parser
 * 
 * OPTIONAL NER system: CRF-based NER system (edu.berkeley.nlp.entity.ner.NerDriver)
 * 
 * To run this using the pretrained models, run:
 * java -cp /path/to/jar -Xmx8g edu.berkeley.nlp.entity.preprocess.PreprocessingDriver ++base.conf \
 *  -execDir path-to-execution-directory \
 *  -inputDir path/to/input-docs-directory-one-doc-per-file \
 *  -outputDir path/to/output-dir
 *
 * If your data is already one sentence per line, add the -skipSentenceSplitting
 * flag. If it's split into paragraphs (i.e. some sentence boundaries are known),
 * use -respectInputLineBreaks (default: false) or -respectInputTwoLineBreaks
 * (default: true) to tell the system this, depending on whether paragraphs are
 * split by one new line or two.
 * 
 * Look in edu.berkeley.nlp.entity.ner.NerDriver and
 * edu.berkeley.nlp.coref.preprocess.SentenceSplitterTokenizerDriver for how to
 * train new sentence splitting and NER models. New parser grammars can be trained
 * using the Berkeley Parser.
 * 
 * -mode RAW_TEXT (default): runs from raw text (untokenized, not sentence split, etc.), one document per file.
 * Required arguments: -inputDir, -outputDir
 * 
 * -mode CONLL_JUST_WORDS: runs from a CoNLL skeleton, does not do sentence splitting or tokenization
 * because this is presumed to already be done.
 * Required arguments: -inputDir, -outputDir
 * 
 * @author gdurrett
 *
 */
public class PreprocessingDriver implements Runnable {
  @Option(gloss = "")
  public static Mode mode = Mode.RAW_TEXT;

  @Option(gloss = "Path to read the sentence splitter model from")
  public static String sentenceSplitterModelPath = "models/sentsplit.txt.gz";
  @Option(gloss = "Path to read the Berkeley Parser grammar from")
  public static String grammarPath = "models/eng_sm6.gr";
  @Option(gloss = "Path to read a backoff grammar from")
  public static String backoffGrammarPath = "models/eng_sm1.gr";
  @Option(gloss = "Path to read the NER model from")
  public static String nerModelPath = "";
  
  @Option(gloss = "Raw text or CoNLL input directory")
  public static String inputDir = "";
  @Option(gloss = "CoNLL annotation output directory")
  public static String outputDir = "";
  @Option(gloss = "Skip sentence splitting entirely.")
  public static boolean skipSentenceSplitting = false;
  @Option(gloss = "Respect line breaks for sentence segmentation (i.e. a new line always means a new sentence). False by default.")
  public static boolean respectInputLineBreaks = false;
  @Option(gloss = "Respect two consecutive line breaks for sentence segmentation (i.e. a blank line always means a new sentence). True by default.")
  public static boolean respectInputTwoLineBreaks = true;
  @Option(gloss = "Use an alternate tokenizer that may respect the original input a little bit more.")
  public static boolean useAlternateTokenizer = false;
  @Option(gloss = "Use full filesystem paths as document names rather than just file names")
  public static boolean useFullPathsAsDocNames = false;
  
  public static enum Mode {
    RAW_TEXT, CONLL_JUST_WORDS, REDO_CONLL;
  }
  
  public static void main(String[] args) {
    PreprocessingDriver main = new PreprocessingDriver();
    Execution.run(args, main); // add .class here if that class should receive command-line args
  }
  
  public void run() {
    Logger.setFig();
    Logger.logss("Loading sentence splitter");
    SentenceSplitter splitter = SentenceSplitter.loadSentenceSplitter(sentenceSplitterModelPath);
    Logger.logss("Loading parser");
    CoarseToFineMaxRuleParser parser = loadParser(grammarPath);
    Logger.logss("Loading backoff parser");
    CoarseToFineMaxRuleParser backoffParser = loadParser(backoffGrammarPath);
    Logger.logss("Loading NER system");
    NerSystemLabeled nerSystem = (nerModelPath.isEmpty() ? null : NerSystemLabeled.loadNerSystem(nerModelPath));
    if (!new File(inputDir).exists()) {
      throw new RuntimeException("Couldn't locate input directory " + inputDir);
    }
    if (!inputDir.isEmpty() && !outputDir.isEmpty() && !inputDir.equals(outputDir)) {
      if (mode == Mode.RAW_TEXT) {
        for (File inputFile : new File(inputDir).listFiles()) {
          processDocument(splitter, parser, backoffParser, nerSystem, inputDir + "/" + inputFile.getName(), outputDir + "/" + inputFile.getName());
        }
      } else if (mode == Mode.CONLL_JUST_WORDS) {
        ConllDocJustWords[] conllDocs = ConllDocReader.readConllDocsJustWords(inputDir);
        PrintWriter writer = IOUtils.openOutHard(outputDir);
        for (ConllDocJustWords conllDoc : conllDocs) {
          String docName = conllDoc.docID();
          String[][] docConllLines = renderDocConllLines(docName, conllDoc.wordsArrs(), parser, backoffParser, nerSystem);
          writeConllLines(docName, docConllLines, writer);
          Logger.logss("Processed document " + docName + " and wrote result to " + outputDir);
        }
        writer.close();
      } else {
        ConllDocReader docReader = new ConllDocReader(Language.ENGLISH, "");
        for (File inputFile : new File(inputDir).listFiles()) {
          Reprocessor.redoConllDocument(parser, backoffParser, nerSystem, docReader, inputDir + "/" + inputFile.getName(), outputDir + "/" + inputFile.getName());
        }
      }
    } else {
      Logger.logss("Need to provide either a distinct inputPath/outputPath pair or a distinct inputDir/outputDir");
    }
  }
  
  public static void processDocument(SentenceSplitter splitter, CoarseToFineMaxRuleParser parser, CoarseToFineMaxRuleParser backoffParser, NerSystemLabeled nerSystem, String inputPath, String outputPath) {
    String docName = inputPath;
    if (!useFullPathsAsDocNames && docName.contains("/")) {
      docName = docName.substring(docName.lastIndexOf("/") + 1);
    }
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
  }
  
  public static void writeConllLines(String docName, String[][] docConllLines, String fileName) {
    PrintWriter writer = IOUtils.openOutHard(fileName);
    writeConllLines(docName, docConllLines, writer);
    writer.close();
  }
  
  public static void writeConllLines(String docName, String[][] docConllLines, PrintWriter writer) {
    writer.println("#begin document (" + docName + "); part 000");
    for (String[] sentenceConllLines : docConllLines) {
      for (String conllLine : sentenceConllLines) {
        writer.println(conllLine);
      }
      writer.println();
    }
    writer.println("#end document");
  }
  
  public static String[][] renderDocConllLines(String docName, String[][] tokenizedSentences, CoarseToFineMaxRuleParser parser, CoarseToFineMaxRuleParser backoffParser, NerSystemLabeled nerSystem) {
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
  }

  public static String[] renderSentenceConllLines(String docName, int partNo, String[] words, String[] pos, Tree<String> parse, String[] nerBioLabels) {
    assert words.length == pos.length;
    assert words.length == parse.getYield().size();
    assert words.length == nerBioLabels.length;
    // PARSE PROCESSING
    String[] parseBits = computeParseBits(parse);
    String[] nerBits = computeNerBits(nerBioLabels);
    String[] conllLines = new String[words.length];
    for (int i = 0; i < words.length; i++) {
      conllLines[i] = new StringBuffer(docName).append("\t").append(partNo).append("\t").append(i).append("\t").append(words[i]).append("\t").
          append(pos[i]).append("\t").append(parseBits[i]).append("\t-\t-\t-\t-\t").append(nerBits[i]).append("\t-").toString();
    }
    return conllLines;
  }  
  
  public static String[] computeParseBits(Tree<String> parse) {
    String parseBitsConcat = parseTraversalHelper(parse);
    String[] bitsSplitAtStars = parseBitsConcat.split("\\*");
    for (int i = 1; i < bitsSplitAtStars.length; i++) {
      int firstIndexOfNonCloseParen = 0;
      while (firstIndexOfNonCloseParen < bitsSplitAtStars[i].length() && bitsSplitAtStars[i].charAt(firstIndexOfNonCloseParen) == ')') {
        firstIndexOfNonCloseParen++;
      }
      bitsSplitAtStars[i-1] += "*" + bitsSplitAtStars[i].substring(0, firstIndexOfNonCloseParen);
      bitsSplitAtStars[i] = bitsSplitAtStars[i].substring(firstIndexOfNonCloseParen);
    }
    assert bitsSplitAtStars[bitsSplitAtStars.length - 1].isEmpty();
    return bitsSplitAtStars;
  }

  private static String parseTraversalHelper(Tree<String> currTree) {
    if (currTree.isPreTerminal()) {
      return "*";
    } else {
      String childrenConcat = "";
      for (Tree<String> child : currTree.getChildren()) {
        childrenConcat += parseTraversalHelper(child);
      }
      String label = currTree.getLabel();
      if (label.equals("ROOT")) {
        label = "TOP";
      }
      return "(" + label + childrenConcat + ")";
    }
  }
  
  public static String[] computeNerBits(String[] nerBioLabels) {
    int size = nerBioLabels.length;
    String[] nerBits = new String[size];
    boolean inNer = false;
    for (int i = 0; i < size; i++) {
      if (nerBioLabels[i].startsWith("B")) {
        String nerType = "MISC";
        if (nerBioLabels[i].contains("-")) {
          nerType = nerBioLabels[i].substring(nerBioLabels[i].indexOf("-") + 1);
        }
        if (i == size - 1 || !nerBioLabels[i+1].startsWith("I")) {
          nerBits[i] = "(" + nerType + ")";
          inNer = false;
        } else {
          nerBits[i] = "(" + nerType + "*";
          inNer = true;
        }
      } else if (nerBioLabels[i].startsWith("I")) {
        assert inNer;
        if (i == size - 1 || !nerBioLabels[i+1].startsWith("I")) {
          nerBits[i] = "*)";
          inNer = false;
        } else {
          nerBits[i] = "*";
        }
      } else {
        nerBits[i] = "*";
        inNer = false;
      }
    }
    return nerBits;
  }
  
  public static CoarseToFineMaxRuleParser loadParser(String grFileName) {
    String inFileName = grFileName;
    ParserData pData = ParserData.Load(inFileName);
    if (pData == null) {
      System.out.println("Failed to load grammar from file " + inFileName);
      System.exit(1);
    }
    Grammar grammar = pData.getGrammar();
    Lexicon lexicon = pData.getLexicon();
    Numberer.setNumberers(pData.getNumbs());
    // Defaults from edu.berkeley.nlp.PCFGLA.BerkeleyParser
    double threshold = 1.0;
    boolean viterbi = false;
    boolean substates = false;
    boolean scores = false;
    boolean accurate = false;
    boolean variational = false;
    CoarseToFineMaxRuleParser parser = new CoarseToFineMaxRuleParser(grammar, lexicon,
          threshold, -1, viterbi, substates,
          scores, accurate, variational, true,
          true);
    parser.binarization = pData.getBinarization();
    return parser;
  }
  
  public static Tree<String> parse(CoarseToFineMaxRuleParser parser, CoarseToFineMaxRuleParser backoffParser, List<String> sentence) {
    Tree<String> result = parseSoft(parser, backoffParser, sentence);
    if (result == null) {
      throw new RuntimeException("Couldn't parse even with backoff parser!");
    }
    return result;
  }
  
  public static Tree<String> parseSoft(CoarseToFineMaxRuleParser parser, CoarseToFineMaxRuleParser backoffParser, List<String> sentence) {
    int maxLength = 200;
    boolean goodParseFound = false;
    Tree<String> parsedTree = null;
    List<String> posTags = null;
    if (sentence.size() <= maxLength) {
      parsedTree = parser.getBestConstrainedParse(sentence, posTags, null);
      goodParseFound = parsedTree.getYield().size() == sentence.size();
    }
    if (!goodParseFound && backoffParser != null) {
      Logger.logss("Using backoff parser on sentence: " + sentence.toString());
      parsedTree = backoffParser.getBestConstrainedParse(sentence, posTags, null);
      goodParseFound = parsedTree.getYield().size() == sentence.size();
      if (!goodParseFound) {
        Logger.logss("WARNING: Backoff parser failed on sentence: " + sentence.toString());
      }
    }
    if (!goodParseFound) {
      return null;
    } else {
      // Debinarize
      boolean keepFunctionLabels = false;
      parsedTree = TreeAnnotations.unAnnotateTree(parsedTree, keepFunctionLabels);
      return parsedTree;
    }
  }
}
