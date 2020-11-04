#!/bin/bash

# $1 = Submissions folder
# $2 = app name
# $3+ = args



if [ -z "$1" ]
then
  echo "First argument is submissions folder"
  echo "Second argument is app name"
  echo "Rest of arguments are arguments to app"
  exit 1
fi

if [ -z "$2" ]
then
  echo "First argument is submissions folder"
  echo "Second argument is app name"
  echo "Rest of arguments are arguments to app"
  exit 1
fi

A=( "$@" );

for i in $(ls $1);
do ./run-stud-pt.sh $1/$i $2 ${A[@]:2}
done
