berkeley-entity
===============

The Berkeley Entity Resolution System jointly solves the problems of named entity recognition, coreference resolution, and entity linking with a feature-rich discriminative model.

##Preamble

The Berkeley Entity Resolution System is a state-of-the-art English coreference
resolution system described in:

"A Joint Model for Entity Analysis." Greg Durrett and Dan Klein. TACL 2014.

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

Coreference training and testing data: Training and testing data are provided
to the system as directories containing files ending in auto_conll or
gold_conll (usually the former, all experiments are done over auto annotation).
These can be produced from the CoNLL shared task data by flattening the
directories as follows:

```
find . -path .*conll | while read file; do
  cp $file path/to/flattened/directory
done
```

####Number and gender data

Available from:

http://old-site.clsp.jhu.edu/~sbergsma/Gender/index.html

Unzip to produce gender.data. The system expects this at data/gender.data, so
if you put it here you won't have to think about it.

####Brown clusters

We use Koo et al. (2008)'s, available from:

http://people.csail.mit.edu/maestro/papers/bllip-clusters.gz

These worked better than the Turian et al. (2010) clusters. The system
expects these at data/bllip-clusters

####CoNLL scorer

Available from:

https://code.google.com/p/reference-coreference-scorers/

There will be three things in the download: scorer.pl, CorScorer.pm, and a
directory called Algorithm.  Put Algorithm and CorScorer.pl in the directory
you run the jar from, or in lib/ under that directory. This way they'll be
located for scoring. scorer.pl can go anywhere as long as you pass in the
appropriate path with -conllEvalScriptPath

Note that all results in the paper come from version 7 of the CoNLL scorer.
Other versions of the scorer may return different results.

####Input/output spec

See the [CoNLL 2012 shared task
page](http://conll.cemantix.org/2012/data.html) for more information about the
data formats. All of our files follow this standard; when we have subsets of
annotations, the corresponding columns are simply left blank (i.e. no
coreference chunks or NER chunks, vacuous trees, etc.).  Entity links are
included in a standoff file so that we avoid modifying these files: they are
presented as an extra column with the same specification as NER chunks, with
the exception that they can overlap.

All of this information is for data in the OntoNotes format. If you are
interested in reproducing the ACE results from the paper, please email me. ACE
was preprocessed with external modules and different annotation formats than
the released system is set up to handle.



##Running the system

The main class is ```edu.berkeley.nlp.entity.Driver``` The running of the system is
documented more thoroughly there. It supports running pretrained models on raw
text as well as training and evaluating new models.

An example run on new data is included in ```run-test.sh```

Note that a trained model includes not just feature specifications and weights
for the joint model, but also trained coarse models for coreference and NER.



##Training

The system expects automatic annotations in files ending with auto_conll (i.e.
parses) and gold annotations (i.e. coref and NER) in gold_conll files.
Currently the OntoNotes version of the system cannot take gold entity links
as supervision; email me if you are interested in such functionality.



##Preprocessing

The system is runnable from raw text as input. It runs a sentence splitter
(Gillick, 2009), tokenizer (Penn Treebank), and parser (Berkeley parser), or a
subset of these.  See edu.berkeley.nlp.entity.preprocess.PreprocessingDriver
for more information about these tools and command line options. See
run-test.sh for an example usage.

##Building from source

The easiest way to build is with SBT:
https://github.com/harrah/xsbt/wiki/Getting-Started-Setup

then run

```
sbt assembly
```

which will compile everything and build a runnable jar.

You can also import it into Eclipse and use the Scala IDE plug-in for Eclipse
http://scala-ide.org



##Adding features

Features can be specified on the command line and are instantiated in a few
different places.

```edu.berkeley.nlp.entity.coref.PairwiseIndexingFeaturizerJoint```: coreference, control with ```-pairwiseFeats```

```edu.berkeley.nlp.entity.ner.NerFeaturizer```: NER, control with ```-nerFeatureSet```

```edu.berkeley.nlp.entity.wiki.QueryChoiceComputer```: linking

```edu.berkeley.nlp.entity.joint.JointFeaturizerShared```: joint features, control with
```-corefNerFeatures, -wikiNerFeatures, -corefWikiFeatures```

The methods to instantiate features are extensible, so you can play around with
features as you desire.  Additional information sources can either be passed to
the featurizers or accessed in a static fashion.

For reasonable default values, see config/train-ace.conf



##ACE

If you are interested in reproducing the ACE results, please email me. We
preprocessed the ACE data with a different pipeline tuned to the ACE standard
and assumed gold mentions, and the ACE system assumes access to this data
which we cannot freely distribute due to licensing restrictions.



##Troubleshooting

Calling the coreference scorer (in TRAIN_EVALUATE mode) may cause an
out-of-memory error because under the hood, Java forks the process and if
you're running with a lot of memory, it may crash. You can use the coreference
system in COREF_PREDICT or COREF_TRAIN_PREDICT and then evaluate separately
to avoid this.

