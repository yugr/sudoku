#!/bin/sh

set -e

LOG=test.log

rm -f $LOG

if test -z "$1"; then
  kk='3 4'
else
  kk="$(seq 3 $1)"
fi

if test "$2" = obj; then
  ext=.exe
else
  ext=.sh
fi

# Some hints on selecting a filling percentage:
# * anything higher than 30% will cause current generator to frequently produce unsolvable boards
# * less than 20% (=17/81) is generally believed to be extra-difficult for human solver
fill_ratio=20

for k in $kk; do
  sz=$(($k * $k))
  nelts=$(($sz*$sz))
  ninits=$(($fill_ratio * $nelts / 100))
  echo "=== ${sz}x${sz} ($ninits/$nelts initialized) ==="
  bin/GenRand$ext $sz $ninits 0 | tee -a $LOG
  (bin/GenRand$ext $sz $ninits 0 | time -f 'Time: %e s' bin/Solve$ext) 2>&1 | tee -a $LOG
  echo
done

