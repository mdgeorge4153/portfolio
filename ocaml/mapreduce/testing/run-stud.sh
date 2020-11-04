#!/bin/bash

# $1 = student directory
# $2 = app name
# $3+ = args

if [ -z "$1" ]
then 
    echo "First argument is student directory"
    echo "Second  argument is app name"
    echo "Rest of arguments are arguments to app"
    exit 1
fi;

if [ -z "$2" ]
then 
    echo "First argument is student directory"
    echo "Second  argument is app name"
    echo "Rest of arguments are arguments to app"
    exit 1
fi;

A=( "$@" );
    
cd $1;
cs3110 compile controllerMain.ml
cs3110 run controllerMain.ml -l $2 ${A[@]:2}
cs3110 clean
