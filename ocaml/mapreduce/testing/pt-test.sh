cd solution
cs3110 compile workerMain.ml
cs3110 run workerMain.ml 31100 &
cs3110 run workerMain.ml 31101 &
cd -

echo "" > Submissions/results.csv
./run-all-pt.sh Submissions pt 2

killall -9 cs3110
