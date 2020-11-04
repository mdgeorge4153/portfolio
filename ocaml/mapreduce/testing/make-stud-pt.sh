#!/bin/bash

# $1 = student directory

if [ -z "$1" ]; then
    echo "Supply a student directory"
    exit 1
fi

cp -rdf release/* $1;
cp -rdf solution/apps/parallel_test $1/apps/parallel_test
cp solution/.depend $1;  
cp release/.opam_packages $1;  
cp release/.libs $1;  
cp solution/apps/parallel_test/ptappList.ml $1/apps/appList.ml

cd $1;

mv -f dnaSequencing.ml apps/dna_sequencing;
mv -f remoteController.ml map_reduce;
mv -f warmup.ml async;
mv -f aQueue.ml async;
mv -f invertedIndex.ml apps/inverted_index;
mv -f worker.ml map_reduce;

rm -rf apps/*/data
