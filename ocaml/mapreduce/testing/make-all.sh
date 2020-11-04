#!/bin/bash

# $1 = Submissions folder

if [ -z "$1" ]
then
  echo "Please pass in submissions folder"
  exit 1
fi

for i in $(ls $1);
do ./make-stud.sh $1/$i;
done
