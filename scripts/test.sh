#!/bin/sh

set -e

LOG=ghc.log

rm -f $LOG board_*.cnf *.o *.hi *.exe

# Some hints on selecting a filling percentage:
# * anything higher than 30% will cause current generator to frequently produce unsolvable boards
# * less than 20% (=17/81) is generally believed to be extra-difficult for human solver
fill_ratio=20

#for k in 3 4; do
for k in 3; do
  sz=$(($k * $k))
  nelts=$(($sz*$sz))
  ninits=$(($fill_ratio * $nelts / 100))
  echo "=== ${sz}x${sz} ($ninits/$nelts initialized) ==="
  bin/GenRand $sz $ninits 0 | tee -a $LOG
  (bin/GenRand $sz $ninits 0 | time -f 'Time: %e s' bin/Solve) 2>&1 | tee -a $LOG
  echo
done
#./build.sh
