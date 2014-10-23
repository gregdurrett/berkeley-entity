package edu.berkeley.nlp.entity.preprocess;

import java.io.PrintWriter;
import java.util.List;

import edu.berkeley.nlp.futile.tokenizer.PTBLineLexer;
import edu.berkeley.nlp.futile.util.Logger;
import edu.berkeley.nlp.futile.fig.basic.IOUtils;
import edu.berkeley.nlp.futile.fig.basic.Option;
import edu.berkeley.nlp.futile.fig.exec.Execution;

/**
 * Driver for training new sentence splitting models and running just the
 * sentence splitting and tokenization phases of the preprocessing pipeline.
 * 
 * TRAIN: Trains a sentence splitter
 * Required arguments: -inputPath
 *   or -trainFromConll, -conllTrainPath, -conllTestPath (can be the same as train)
 * 
 * RUN: Sentence-splits and tokenizes some data
 * Required arguments: -inputPath, -modelPath
 * 
 * @author gdurrett
 *
 */
public class SentenceSplitterTokenizerDriver implements Runnable {
  @Option(gloss = "")
  public static Mode mode = Mode.TRAIN;
  
  @Option(gloss = "Raw text input")
  public static String inputPath = "";
  @Option(gloss = "")
  public static String outputPath = "";
  @Option(gloss = "")
  public static boolean respectInputLineBreaks = false;
  @Option(gloss = "")
  public static boolean respectInputTwoLineBreaks = true;
  
  @Option(gloss = "Path to read/write the model")
  public static String modelPath = "";
  
  // TRAINING OPTIONS
  @Option(gloss = "Train the sentence splitter from the CoNLL data. If false, you " +
  		"must provide your own data in the format\n" +
  		"<word>. <word> <0 or 1>\n" +
  		"where 0 indicates not a boundary and 1 indicates a boundary.")
  public static boolean trainFromConll = true;

  @Option(gloss = "Path to training set")
  public static String trainPath = "";
  @Option(gloss = "Path to test set")
  public static String testPath = "";
  @Option(gloss = "Path to CoNLL training set")
  public static String conllTrainPath = "";
  @Option(gloss = "Training set size, -1 for all")
  public static int conllTrainSize = -1;
  @Option(gloss = "Path to CoNLL test set")
  public static String conllTestPath = "";
  @Option(gloss = "Test set size, -1 for all")
  public static int conllTestSize = -1;
  
  public static enum Mode {
    TRAIN, RUN;
  }
  
  public static void main(String[] args) {
    SentenceSplitterTokenizerDriver main = new SentenceSplitterTokenizerDriver();
    Execution.run(args, main); // add .class here if that class should receive command-line args
  }
  
  public void run() {
    Logger.setFig();
    switch (mode) {
      case TRAIN: SentenceSplitter.trainSentenceSplitter();
        break;
      case RUN:
        SentenceSplitter splitter = SentenceSplitter.loadSentenceSplitter(modelPath);
        String[] lines = IOUtils.readLinesHard(inputPath).toArray(new String[0]);
        String[] canonicalizedParagraphs = splitter.formCanonicalizedParagraphs(lines, respectInputLineBreaks, respectInputTwoLineBreaks);
        String[] sentences = splitter.splitSentences(canonicalizedParagraphs);
        String[][] tokenizedSentences = splitter.tokenize(sentences);
        PrintWriter writer = IOUtils.openOutHard(outputPath);
        for (String[] sentence : tokenizedSentences) {
          for (int i = 0; i < sentence.length; i++) {
            writer.print(sentence[i]);
            if (i < sentence.length - 1) {
              writer.print(" ");
            }
          }
          writer.println();
        }
        writer.close();
        break;
    }
  }
}
