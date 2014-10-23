berkeley-entity
===============

The Berkeley Entity Resolution System jointly solves the problems of named entity recognition, coreference resolution, and entity linking with a feature-rich discriminative model.



##Preamble

The Berkeley Entity Resolution System is a state-of-the-art English coreference
resolution system described in:

"A Joint Model for Entity Analysis: Coreference, Typing, and Linking" Greg Durrett and Dan Klein. TACL 2014.

The coreference portion is described in:

"Easy Victories and Uphill Battles in Coreference Resolution." Greg Durrett and Dan Klein. EMNLP 2013.

See http://www.eecs.berkeley.edu/~gdurrett/ for papers and BibTeX.

Questions? Bugs? Email me at gdurrett@eecs.berkeley.edu



##License

Copyright (c) 2013-2014 Greg Durrett. All Rights Reserved.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.



##Setup

####Datasets

See the [CoNLL 2012 shared task page](http://conll.cemantix.org/2012/data.html)
for more information about the data formats. All of our files (input and
output) follow this standard; when we have subsets of annotations, the
corresponding columns are simply left blank (i.e. no coreference chunks or NER
chunks, vacuous trees, etc.).  Entity links are included in a standoff file so
that we avoid modifying these files: they are presented as an extra column with
the same specification as NER chunks, with the exception that they can overlap.

The system generally takes directories for input and outputs single files with
all documents concatenated. Note that a directory can contain a single file of
this form. For training, files are required to have auto_conll and gold_conll
suffixes as appropriate; for testing, you can filter the documents to read with
-docSuffix.

Flattened directories of CoNLL files can be produced from the CoNLL shared task
data as follows:

    find . -path .*conll | while read file; do
      cp $file path/to/flattened/directory
    done

We also require [number and gender data](http://www.clsp.jhu.edu/~sbergsma/Gender/)
(default path the system expects this data at: data/gender.data) and [Brown clusters]
(http://people.csail.mit.edu/maestro/papers/bllip-clusters.gz) (default path:
data/bllip-clusters). ```pull-datasets.sh``` should pull these datasets for you
and put htem in the appropriate locations.

####CoNLL Scorer

Available at https://code.google.com/p/reference-coreference-scorers/

There will be three things in the download: scorer.pl, CorScorer.pm, and a
directory called Algorithm.  Put Algorithm and CorScorer.pl in the directory
you run the jar from, or in lib/ under that directory. This way they'll be
located for scoring. scorer.pl can go anywhere as long as you pass in the
appropriate path with -conllEvalScriptPath; the system expects it at
scorer/v7/scorer.pl.

Again, ```pull-datasets.sh``` will do all this for you.

Note that all results in the paper come from version 7 of the CoNLL scorer.
Other versions of the scorer may return different results.

####Models

Models are not included in GitHub due to their large size. Download the latest
models from http://nlp.cs.berkeley.edu/projects/entity.shtml



##Running the system

The main class is ```edu.berkeley.nlp.entity.Driver``` The running of the system is
documented more thoroughly there. It supports running pretrained models on raw
text as well as training and evaluating new models.

An example run on new data is included in ```run-test.sh```

Note that a trained model includes not just feature specifications and weights
for the joint model, but also trained coarse models for coreference and NER.

To reproduce CoNLL results, run:

    java -jar berkeley-entity-1.0.jar ++config/base.conf -execDir scratch -mode PREDICT_EVALUATE -testPath data/conll-2012-en/test\
      -modelPath "models/cached/joint-onto.ser.gz" -wikipediaPath "models/cached/wiki-db-onto.ser.gz" \
      -docSuffix auto_conll

To reproduce ACE results, run with:

    java -jar berkeley-entity-1.0.jar ++config/base.conf -execDir scratch -mode PREDICT_EVALUATE_ACE -testPath data/ace05/test \
      -modelPath "models/cached/joint-ace.ser.gz" -wikipediaPath "models/cached/wiki-db-ace.ser.gz" \
      -doConllPostprocessing false -useGoldMentions -wikiGoldPath data/ace05/ace05-all-conll-wiki

Note that this requires the ACE data to be in the CoNLL standard with standoff
Wikipedia annotations in ace05-all-conll-wiki. This whole process is sensitive
to tokenization and sentence-splitting.  If you're interested in reproducing
these results, please contact me.



##Preprocessing

The system is runnable from raw text as input. It runs a sentence splitter
(Gillick, 2009), tokenizer (Penn Treebank), and parser (Berkeley parser), or a
subset of these.  See ```edu.berkeley.nlp.entity.preprocess.PreprocessingDriver```
for more information about these tools and command line options. See
```run-test.sh``` for an example usage.



##Training

The system expects automatic annotations in files ending with auto_conll (i.e.
parses) and gold annotations (i.e. coref and NER) in gold_conll files.
Currently the OntoNotes version of the system cannot take gold entity links
as supervision; email me if you are interested in such functionality.

To train a CoNLL model, run:

    java -jar berkeley-entity-1.0.jar ++config/base.conf -execDir scratch -mode TRAIN_EVALUATE \
      -trainPath data/conll-2012-en/train -testPath data/conll-2012-en/dev -modelPath models/cached/joint-new-onto.ser.gz \
      -wikipediaPath models/cached/wiki-model-onto.ser.gz \
      -pruningStrategy build:models/cached/corefpruner-onto.ser.gz:-5:5 \
      -nerPruningStrategy build:models/cached/nerpruner-onto.ser.gz:-9:5 \
      -numItrs 30



##Building from source

The easiest way to build is with SBT:
https://github.com/harrah/xsbt/wiki/Getting-Started-Setup

then run

    sbt assembly

which will compile everything and build a runnable jar.

You can also import it into Eclipse and use the Scala IDE plug-in for Eclipse
http://scala-ide.org



##Adding features

Features can be specified on the command line and are instantiated in a few
different places.

Coreference: ```edu.berkeley.nlp.entity.coref.PairwiseIndexingFeaturizerJoint```, control with ```-pairwiseFeats```

NER: ```edu.berkeley.nlp.entity.ner.NerFeaturizer```, control with ```-nerFeatureSet```

Linking: ```edu.berkeley.nlp.entity.wiki.QueryChoiceComputer```

Joint: ```edu.berkeley.nlp.entity.joint.JointFeaturizerShared```, control with
```-corefNerFeatures, -wikiNerFeatures, -corefWikiFeatures```

The methods to instantiate features are extensible.  Additional information
sources can either be passed to the featurizers or accessed in a static
fashion.

For reasonable default values, see config/train-ace.conf



##Troubleshooting

Calling the coreference scorer (in TRAIN_EVALUATE mode) may cause an
out-of-memory error because under the hood, Java forks the process and if
you're running with a lot of memory, it may crash. You can use the coreference
system in COREF_PREDICT or COREF_TRAIN_PREDICT and then evaluate separately
to avoid this.

