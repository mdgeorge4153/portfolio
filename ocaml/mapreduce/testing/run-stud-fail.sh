#!/bin/bash

# $1 = student directory
# $2 = app name
# $3 = args
# $4 = worker type
# $5 = job type

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

    
cd $1;
cp map_reduce/worker.ml map_reduce/worker_tmp.ml
cp map_reduce/worker$4.ml map_reduce/worker.ml
cs3110 compile workerMain.ml

cs3110 compile controllerMain.ml
cs3110 run controllerMain.ml $2 $3
cs3110 clean
cp map_reduce/worker_tmp.ml map_reduce/worker.ml
