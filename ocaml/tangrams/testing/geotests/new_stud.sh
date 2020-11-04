#!/bin/bash

#echo -e "Grader: akd54 + jd753 \n\nGame: 15/15\nGreat work!\n\nConvex Hull: 20/20\nWell done!\n\nMinkowski: 5/5\nNice.\n\n" > ../../Submissions/$1/$1_game.txt;

#gvim ../../Submissions/$1/$1_game.txt
cp ../../Submissions/$1/game.ml .
cp ../../Submissions/$1/geometry.ml .

make clean;
make;
./tangrams

make geotest;
./geotest
