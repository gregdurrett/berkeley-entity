#!/bin/bash

mkdir data

# Number and gender data
rm data/gender.data
letters=( a b c d e f )
for letter in "${letters[@]}"; do
  wget http://www.clsp.jhu.edu/~sbergsma/Gender/Data/gender.a${letter}.gz
  gunzip gender.a${letter}.gz
  cat gender.a${letter} >> data/gender.data
  rm gender.a${letter}
done

# Brown clusters
wget http://people.csail.mit.edu/maestro/papers/bllip-clusters.gz
gunzip bllip-clusters.gz
mv bllip-clusters data/

# CoNLL scorer
wget http://conll.cemantix.org/download/reference-coreference-scorers.v7.tar.gz
tar -xvf reference-coreference-scorers.v7.tar.gz
mkdir scorer
mv reference-coreference-scorers/v7/ scorer
cp scorer/v7/lib/CorScorer.pm lib/
cp -r scorer/v7/lib/Algorithm lib/
rm -rf reference-coreference-scorers*

