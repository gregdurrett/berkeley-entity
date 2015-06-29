package edu.berkeley.nlp.entity.ner;

import edu.berkeley.nlp.futile.fig.basic.Option;
import edu.berkeley.nlp.futile.fig.exec.Execution;
import edu.berkeley.nlp.futile.util.Logger;

/**
 * Driver for training and a CRF-based NER system. Currently does not run as a standalone
 * NER predictor and only integrates with the preprocessor described in
 * edu.berkeley.nlp.entity.preprocess.PreprocessingDriver
 * 
 * TRAIN_EVALUATE: Trains and evaluates the NER system.
 * Required arguments: -trainPath, -testPath, -brownClustersPath (path to bllip-clusters)
 * 
 * TRAIN_RUN_KFOLD: Generates pruning masks for training and test data. k-fold cross
 * validation is used to generate training masks (so we don't train on the same data
 * that we evaluate with).
 * Required arguments: -kFoldInputPath, -testPath, -brownClustersPath (path to bllip-clusters),
 * -marginalsPath (output path), -modelOutPath
 * 
 * Other arguments allow you to tweak the trained model
 * 
 * @author gdurrett
 *
 */
public class NerDriver implements Runnable {
  @Option(gloss = "")
  public static Mode mode = Mode.TRAIN_EVALUATE;

  @Option(gloss = "Path to read/write the model")
  public static String modelPath = "";

  // TRAINING_OPTIONS
  @Option(gloss = "Path to CoNLL training set")
  public static String trainPath = "";
  @Option(gloss = "Training set size, -1 for all")
  public static int trainSize = -1;
  @Option(gloss = "Path to CoNLL test set")
  public static String testPath = "";
  @Option(gloss = "Test set size, -1 for all")
  public static int testSize = -1;
  @Option(gloss = "Path to Brown clusters")
  public static String brownClustersPath = "";
  @Option(gloss = "Path to write evaluation output to")
  public static String outputPath = "";
  
  @Option(gloss = "Use predicted POS tags")
  public static boolean usePredPos = true;
  
  @Option(gloss = "Use variational decoding")
  public static boolean variational = false;

  @Option(gloss = "Path to serialized NE marginals for the training set")
  public static String marginalsPath = "";
  @Option(gloss = "Path to serialized model")
  public static String modelOutPath = "";
  
  @Option(gloss = "Path to document sets to do the k-fold trick on")
  public static String kFoldInputPath = "";
  @Option(gloss = "K-fold set size, -1 for all")
  public static int kFoldSize = -1;
  @Option(gloss = "Path to write documents after k-fold prediction")
  public static String kFoldOutputPath = "";
  @Option(gloss = "Number of folds to use")
  public static int numFolds = 5;
  
  @Option(gloss = "Regularization")
  public static double reg = 1e-8;
  @Option(gloss = "Iterations to optimize for")
  public static int numItrs = 30;
  @Option(gloss = "Adagrad batch size")
  public static int batchSize = 100;
  
  @Option(gloss = "Feature set")
  public static String featureSet = "bigrams+brown";
  @Option(gloss = "Unigram cutoff threshold")
  public static int unigramThreshold = 1;
  @Option(gloss = "Bigram cutoff threshold")
  public static int bigramThreshold = 10;
  @Option(gloss = "Prefix/suffix cutoff threshold")
  public static int prefSuffThreshold = 2;
  
  
  public static enum Mode {
    TRAIN_EVALUATE, TRAIN_RUN_KFOLD;
  }
  
  public static void main(String[] args) {
    NerDriver main = new NerDriver();
    Execution.run(args, main); // add .class here if that class should receive command-line args
  }
  
  public void run() {
    Logger.setFig();
    switch (mode) {
      case TRAIN_EVALUATE: NerSystemLabeled.trainEvaluateNerSystem(trainPath, trainSize, testPath, testSize);
        break;
      case TRAIN_RUN_KFOLD:
        NerSystemLabeled.trainPredictTokenMarginalsKFold(kFoldInputPath, kFoldSize, brownClustersPath, testPath.split(","), testSize, numFolds, marginalsPath, modelOutPath);
        break;
    }
  }
}
