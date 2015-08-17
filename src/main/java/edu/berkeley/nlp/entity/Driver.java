package edu.berkeley.nlp.entity;

import edu.berkeley.nlp.entity.coref.ConjFeatures;
import edu.berkeley.nlp.entity.coref.ConjScheme;
import edu.berkeley.nlp.entity.coref.CorefPrunerJavaHack;
import edu.berkeley.nlp.entity.coref.CorefSystem;
import edu.berkeley.nlp.entity.lang.Language;
import edu.berkeley.nlp.futile.fig.basic.Option;
import edu.berkeley.nlp.futile.fig.exec.Execution;
import edu.berkeley.nlp.futile.util.Logger;

/**
 * Main entry point for the Berkeley Entity Resolution System. See
 * the README and run-test.sh for some example commands.
 * 
 * There are three basic configurations you can use: coreference only
 * (equivalent to the system of Durrett and Klein (2013)), coreference
 * and NER (does not require a Wikipedia dump), and coreference + NER
 * + entity linking (require Wikipedia dump but gives best performance).
 * 
 * Modes starting with COREF_ correspond to the coreference system.
 * 
 * ================
 * PREDICT: runs the fully joint system or the system without Wikification.
 * Required arguments: -execDir, -testPath, -modelPath, -wikipediaPath (for linking)
 * 
 * To run this with entity linking, you must have already created the Wikipedia
 * database for your test set. See edu.berkeley.nlp.entity.wiki.WikipediaInterface
 * for details on how to do this.
 * 
 * Output (in couple of formats) is provided in the given execution directory.
 * 
 * ================
 * PREDICT_EVALUATE: similar to PREDICT but also runs the evaluation, so it requires access
 * to gold annotation as well. See the README for an example command.
 * 
 * ================
 * TRAIN_EVALUATE: trains and evaluates the fully joint system or
 * the system without entity linking.
 * Required arguments: -trainPath, -testPath, -wikipediaPath (for linking)
 * 
 * To run this with entity linking, you must have already created the Wikipedia
 * database for your test set. See edu.berkeley.nlp.entity.wiki.WikipediaInterface
 * for details on how to do this.
 * 
 * Note that running the system this way for the first time will result in training
 * coarse models, which can be very slow because cross-validation is used. Use
 * MAKE_MASK_MODELS to train coreference mask models offline and TRAIN_RUN_KFOLD
 * in edu.berkeley.nlp.entity.ner.NerDriver to train NER mask models. Alternatively,
 * run this TRAIN_EVALUATE loop once and reuse the coarse models that come out of it.
 * 
 * You specify coarse model paths with pruningStrategy/nerPruningStrategy. The coref
 * pruner (pruningStrategy) is specified as either:
 * 
 * models:path/to/models.ser.gz:threshold
 * build:path/to/models.ser.gz:threshold:num-folds
 * 
 * where {models,build} implies that we use saved models or rebuild them, the path
 * has {read,write} semantics in these two cases, threshold is a negative number
 * that gives the pruning cutoff (-5 is good for coref, and num-folds is the
 * number of folds to use when masking the training data.
 * 
 * The NER masks are handled slightly differently; rather than storing num-folds
 * models, we instead store pruning masks for the training set. These are loaded
 * in conjunction with a single model:
 * 
 * model:path/to/model.ser.gz:threshold
 * marginals:path/to/marginals.ser.gz:threshold
 * 
 * marginalsmodel:path/to/marginals.ser.gz:path/to/model.ser.gz:threshold
 * build:path/to/models.ser.gz:threshold:num-folds
 * 
 * ================
 * PREDICT_ACE: runs prediction using the ACE model (ACE-style mentions and mention-synchronous
 * NER chunks)
 * 
 * PREDICT_EVALUATE_ACE: similar to PREDICT_ACE but also runs the evaluation, so it requires access
 * to gold annotation as well. See the README for an example command. 
 * 
 * TRAIN_EVALUATE_ACE: similar to TRAIN_EVALUATE but uses per-mention semantic types
 * rather than a decoupled NER chain. No NER pruning is done as a result.
 * 
 * COREF_PREDICT: runs the prediction phase with coreference only
 * Required arguments: -testPath, -modelPath, -outputPath
 * 
 * COREF_TRAIN_EVALUATE: train and evaluate stages
 * Required arguments: -trainPath, -testPath
 * 
 * COREF_TRAIN_PREDICT: trains and outputs predicted
 * Required arguments: -trainPath, -testPath, -outputPath
 * 
 * MAKE_MASK_MODELS: Makes coreference masks, which are models trained in an
 * n-fold fashion over the training data, such that there is always some model
 * that can be applied to any document that comes in.
 * Required arguments: -trainPath
 * 
 * @author gdurrett
 *
 */
public class Driver implements Runnable {
  
  @Option(gloss = "Which experiment to run?")
  public static Mode mode = Mode.PREDICT;
  @Option(gloss = "Language choice")
  public static Language lang = Language.ENGLISH;
  
  // DATA AND PATHS
  @Option(gloss = "Path to training set")
  public static String trainPath = "";
  @Option(gloss = "Training set size, -1 for all")
  public static int trainSize = -1;
  @Option(gloss = "Path to test set")
  public static String testPath = "";
  @Option(gloss = "Test set size, -1 for all")
  public static int testSize = -1;
  @Option(gloss = "Suffix to use for documents")
  public static String docSuffix = "";
  @Option(gloss = "Suffix to use for documents in the coref system; this agrees with the EMNLP 2013 system")
  public static String corefDocSuffix = "auto_conll";
  @Option(gloss = "File of better train/test parses to use")
  public static String betterParsesFile = "";

  @Option(gloss = "Path to number/gender data")
  public static String numberGenderDataPath = "data/gender.data";
  @Option(gloss = "Path to pre-extracted serialized WikipediaInterface")
  public static String wikipediaPath = "";
  @Option(gloss = "Path to WordNet dictionary")
  public static String wordNetPath = "";
  @Option(gloss = "Path to Brown clusters (either bllip-clusters from Koo or brown.txt from Turian)")
  public static String brownPath = "data/bllip-clusters";
  @Option(gloss = "Path to CoNLL evaluation script")
  public static String conllEvalScriptPath = "scorer/v7/scorer.pl";

  @Option(gloss = "Path to read/write the model")
  public static String modelPath = "";
  @Option(gloss = "Path to write system output to")
  public static String outputPath = "";
  @Option(gloss = "Directory to write output CoNLL files to when using the scorer. If blank, uses the default temp directory and deletes them after. " +
     "You might want this because calling the scorer forks the process and may give an out-of-memory error, " +
     "so this is some insurance that at least you'll have your output.")
  public static String conllOutputDir = "";
  
  @Option(gloss = "Use gold mentions.")
  public static boolean useGoldMentions = false;
  @Option(gloss = "Can toggle whether written output is filtered for singletons or not")
  public static boolean doConllPostprocessing = true;
  @Option(gloss = "Include appositive mentions?")
  public static boolean includeAppositives = true;
  @Option(gloss = "Include spans contained in NEs? Helps mention recall but hurts precision, good overall")
  public static boolean includeSpansContainedInNE = true;
  @Option(gloss = "Filter non-maximal NPs? Hurts recall but standard")
  public static boolean filterNonMaximalNPs = true;
  @Option(gloss = "Extract coordinated mentions and track multiple heads (necessary for computing some optional features)")
  public static boolean useCoordination = false;

  @Option(gloss = "Print per-document scores for bootstrap significance testing")
  public static boolean printSigSuffStats = false;

  @Option(gloss = "")
  public static String neMentType = "all";
  @Option(gloss = "")
  public static boolean setProperMentionsFromNER = true;
  
  // TRAINING AND INFERENCE
  // These settings are reasonable and quite robust; you really shouldn't have
  // to tune the regularizer. I didn't find adjusting it useful even when going
  // from thousands to millions of features.
  @Option(gloss = "eta for Adagrad")
  public static double eta = 1.0;
  @Option(gloss = "Regularization constant (might be lambda or c depending on which algorithm is used)")
  public static double reg = 0.001;
  @Option(gloss = "Batch size; right now batchSize > 1 works badly for some reason")
  public static int batchSize = 1;
  
  // COREFERENCE OPTIONS
  @Option(gloss = "Loss fcn to use")
  public static String lossFcn = "customLoss-0.1-3-1";
  @Option(gloss = "Loss fcn to use")
  public static String lossFcnSecondPass = "customLoss-0.1-3-1";
  @Option(gloss = "Number of iterations")
  public static int numItrs = 20;
  @Option(gloss = "Pruning strategy for coarse pass. No pruning by default")
  public static String pruningStrategy = "distance:10000:5000";
  
  @Option(gloss = "Features to use")
  public static String pairwiseFeats = "FINAL";
  @Option(gloss = "Conjunction features: what bits do we add to each feature?")
  public static ConjFeatures conjFeats = ConjFeatures.TYPE_OR_CANONICAL_PRON;
  @Option(gloss = "Conjunction scheme: where to apply conjunctions?")
  public static ConjScheme conjScheme = ConjScheme.COARSE_BOTH;
  @Option(gloss = "Directed pairs of mention types to whitelist/blacklist for conjunctions, e.g. PROPER-NOMINAL+PROPER-PROPER")
  public static String conjMentionTypes = "";
  @Option(gloss = "Templates to whitelist/blacklist for conjunctions (depending on how conjScheme is set)")
  public static String conjTemplates = "";
  @Option(gloss = "Way of computing semantic classes")
  public static String semClasserType = "basic";
  
  @Option(gloss = "Cutoff below which lexical features fire POS tags instead")
  public static int lexicalFeatCutoff = 20;
  
  @Option(gloss = "Path to lexical inference outputs (DB of nom/prop<->nom/prop synonymy)")
  public static String lexInfPath = "";
  @Option(gloss = "Use new training scheme")
  public static boolean alternateTraining = false;
  @Option(gloss = "Use stochastic primal subgradient on SVM objective instead of SGD on likelihood")
  public static boolean doSps = false;
  @Option(gloss = "Compute the downstream loss approximation using the current weights")
  public static boolean lossFromCurrWeights = false;
  @Option(gloss = "Compute the downstream loss approximation using the gold clusters")
  public static boolean lossFromGold = false;
  
  @Option(gloss = "Analyses to print; see CorefEvaluator for details")
  public static String analysesToPrint = "";
  
  // MASK MAKING OPTIONS
  @Option(gloss = "")
  public static int maskNumFolds = 5;
  @Option(gloss = "")
  public static double maskLogRatio = 5;
  @Option(gloss = "Cutoff for coarse pass")
  public static String maskOutPath = "";
  
  // JOINT OPTIONS
  @Option(gloss = "Use fancy message passing")
  public static boolean useFancyMessagePassing = false;
  @Option(gloss = "Use fancy message passing")
  public static boolean includeExtraNodePasses = false;
  @Option(gloss = "Number of iterations to run BP for")
  public static int numBPItrs = 5;
  
  
  // JOINT NER OPTIONS
  @Option(gloss = "Use loss-augmentation for NER as well")
  public static boolean useNerLossFcn = true;
  @Option(gloss = "How much to loss-augment NER")
  public static double nerLossScale = 3.0;
  @Option(gloss = "Leave NER latent, don't incorporate it into objective function")
  public static boolean leaveNERLatent = false;
  @Option(gloss = "Feature set for the NER backbone")
  public static String nerFeatureSet = "bigrams+brown";
  @Option(gloss = "True if we should add a layer of variables to convert from NER tags to semantic types")
  public static boolean addIntermediateTypeLayer = false;
  @Option(gloss = "Joint coref/NER features")
  public static String corefNerFeatures = "indicators+currlex+antlex";
  @Option(gloss = "Number of clusters to use in coref/NER features")
  public static int corefNerBrownLength = 4;
  @Option(gloss = "NER pruning strategy")
  public static String nerPruningStrategy = "";
  @Option(gloss = "NER pruning strategy")
  public static String nerMarginalsPath = "";
  @Option(gloss = "NER pruning threshold")
  public static double nerPruningNegThreshold = Double.NEGATIVE_INFINITY;

  @Option(gloss = "Path to CoNLL 2011 so we can compare fairly to Passos")
  public static String conll2011Path = "data/conll-2011-en/dev,data/conll-2011-en/test";

  // JOINT WIKIFICATION OPTIONS
  @Option(gloss = "Leave Wikification latent, don't incorporate it into objective function")
  public static boolean leaveWikificationLatent = false;
  @Option(gloss = "Path to QueryChooser model file to load")
  public static String queryChooserPath = "";
  @Option(gloss = "Inject possible coref targets' options")
  public static boolean injectSharedWikificationOptions = false;
  @Option(gloss = "Inject possible coref targets' options")
  public static int maxNumWikificationOptions = 7;
  @Option(gloss = "Change the type of Wikifier to use")
  public static WikifierType wikifierType = WikifierType.STANDARD;
  @Option(gloss = "For the memorizing Wikifier, what threshold?")
  public static int memorizingWikifierThreshold = 3;
  @Option(gloss = "Wikipedia gold annotations")
  public static String wikiGoldPath = "";
  @Option(gloss = "Raw Wikipedia gold annotations (i.e. not munged yet)")
  public static String rawWikiGoldPath = "";
  @Option(gloss = "How much to penalize Wikification confusion (both in KB)")
  public static double wikiKbKbLoss = 0.0;
  @Option(gloss = "How much to penalize tagging non-NILs as NILs")
  public static double wikiKbNilLoss = 0.0;
  @Option(gloss = "How much to penalize tagging NILs as non-NILs")
  public static double wikiNilKbLoss = 0.0;
  @Option(gloss = "Wikification features")
  public static String wikiFeatures = "";
  @Option(gloss = "Joint Wikification/NER features")
  public static String wikiNerFeatures = "categories+infoboxes+appositives";
  @Option(gloss = "Joint coref/Wikification features")
  public static String corefWikiFeatures = "basic+lastnames";

  @Option(gloss = "")
  public static String allAcePath = "";

  @Option(gloss = "")
  public static boolean writeNerOutput = true;
  @Option(gloss = "")
  public static boolean writeWikiOutput = true;
  @Option(gloss = "")
  public static boolean writeGold = false;
  
  public static enum Mode {
    PREDICT, PREDICT_EVALUATE, TRAIN_EVALUATE,
    PREDICT_ACE, PREDICT_EVALUATE_ACE, TRAIN_EVALUATE_ACE,
    TRAIN_EVALUATE_ACE_JOINT_INF,
    COREF_TRAIN, COREF_PREDICT, COREF_TRAIN_EVALUATE, COREF_TRAIN_PREDICT,
    MAKE_MASK_MODELS;
  }
  
  public static enum WikifierType {
    NONE, STANDARD, MEMORIZING, CHOOSING;
  }
  
  public static void main(String[] args) {
    Driver main = new Driver();
    Execution.run(args, main); // add .class here if that class should receive command-line args
  }
  
  public void run() {
    Logger.setFig();
    if (mode == Mode.PREDICT) {
      EntitySystem.runOntoPredict(testPath, testSize, modelPath);
    } else if (mode == Mode.PREDICT_EVALUATE) {
      EntitySystem.runOntoPredictEvaluate(testPath, testSize, modelPath);
    } else if (mode == Mode.TRAIN_EVALUATE) {
      EntitySystem.runTrainEvaluate(trainPath, trainSize, testPath, testSize);
    } else if (mode == Mode.PREDICT_ACE) {
      EntitySystem.runACEPredict(testPath, testSize, modelPath);
    } else if (mode == Mode.PREDICT_EVALUATE_ACE) {
      EntitySystem.runACEPredictEvaluate(testPath, testSize, modelPath);
    } else if (mode == Mode.TRAIN_EVALUATE_ACE) {
      EntitySystem.runTrainEvaluateACE(trainPath, trainSize, testPath, testSize);
    } else if (mode == Mode.TRAIN_EVALUATE_ACE_JOINT_INF) {
      EntitySystem.runTrainEvaluateACEJointInf(trainPath, trainSize, testPath, testSize);
    } else if (mode == Mode.COREF_PREDICT) {
      CorefSystem.runPredictWriteOutput(testPath, testSize, modelPath, outputPath, doConllPostprocessing);
    } else if (mode == Mode.COREF_TRAIN_EVALUATE) {
      CorefSystem.runTrainEvaluate(trainPath, trainSize, testPath, testSize, modelPath);
    } else if (mode == Mode.COREF_TRAIN_PREDICT) {
      CorefSystem.runTrainPredict(trainPath, trainSize, testPath, testSize, modelPath, outputPath, doConllPostprocessing);
    } else if (mode == Mode.MAKE_MASK_MODELS) {
      CorefPrunerJavaHack.trainAndSaveKFoldModels(trainPath, trainSize, maskNumFolds, maskOutPath);
    } else {
      throw new RuntimeException("Unknown mode: " + mode);
    }
  }
}
