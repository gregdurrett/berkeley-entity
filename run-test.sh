#!/bin/bash

jarpath=berkeley-entity-1.0.jar

mkdir test/scratch
mkdir test/preprocessed
mkdir test/coref
mkdir test/corefner
mkdir test/joint

# Preprocess the data, no NER
if [ ! -f test/preprocessed/government.txt ]; then
  echo "RUNNING PREPROCESSING"
  java -Xmx2g -cp $jarpath edu.berkeley.nlp.entity.preprocess.PreprocessingDriver ++config/base.conf -execDir test/scratch/preprocess -inputDir test/text -outputDir test/preprocessed
else
  echo "Skipping preprocessing..."
fi

# The following commands demonstrate running:
# 1) the coref system in isolation
# 2) the coref + NER system
# 3) the full joint system
# Note that the joint system does not depend on either of the earlier two;
# this is merely meant to demonstrate possible modes of operation.

# Run the coreference system
if [ ! -f test/coref/government.txt-0.pred_conll ]; then
  echo "RUNNING COREF"
  java -Xmx2g -cp $jarpath edu.berkeley.nlp.entity.Driver ++config/base.conf -execDir test/scratch/coref -mode COREF_PREDICT -modelPath models/coref-onto.ser.gz -testPath test/preprocessed -outputPath test/coref
else
  echo "Skipping coref..."
fi

# Run the coref+NER system
if [ ! -f test/corefner/output.conll ]; then
  echo "RUNNING COREF+NER"
  java -Xmx6g -cp $jarpath edu.berkeley.nlp.entity.Driver ++config/base.conf -execDir test/scratch/corefner -mode PREDICT -modelPath models/corefner-onto.ser.gz -testPath test/preprocessed
  cp test/scratch/corefner/output*.conll test/corefner/
else
  echo "Skipping coref+ner..."
fi

# Run the full joint system
# Now run the joint prediction
if [ ! -f test/joint/output.conll ]; then
  echo "RUNNING COREF+NER+WIKI"
  # First, need to extract the subset of Wikipedia relevant to these documents. We have already
  # done this to avoid having. Here is the command used:
  #java -Xmx4g -cp $jarpath:lib/bliki-resources edu.berkeley.nlp.entity.wiki.WikipediaInterface -datasetPaths test/preprocessed -wikipediaDumpPath data/wikipedia/enwiki-latest-pages-articles.xml -outputPath models/wiki-db-test.ser.gz
  java -Xmx8g -cp $jarpath edu.berkeley.nlp.entity.Driver ++config/base.conf -execDir test/scratch/joint -mode PREDICT -modelPath models/joint-onto.ser.gz -testPath test/preprocessed -wikipediaPath models/wiki-db-test.ser.gz
  cp test/scratch/joint/output*.conll test/joint/
else
  echo "Skipping coref+ner+wiki..."
fi

