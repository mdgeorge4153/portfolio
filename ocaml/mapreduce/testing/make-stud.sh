#!/bin/bash

# $1 = student directory

if [ -z "$1" ]
   then
      echo "Supply a student directory"
      exit 1
fi


cp -rdf release/* $1;
cp release/.depend $1;  
cp release/.opam_packages $1;  
cp release/.libs $1;  

cd $1;

mv -f dnaSequencing.ml apps/dna_sequencing;
mv -f remoteController.ml map_reduce;
mv -f warmup.ml async;
mv -f aQueue.ml async;
mv -f invertedIndex.ml apps/inverted_index;
mv -f worker.ml map_reduce;
