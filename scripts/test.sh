#!/bin/sh

# Copyright 2015-2016 Yury Gribov
# 
# Use of this source code is governed by MIT license that can be
# found in the LICENSE.txt file.

set -e

ME=$(basename $0)
ROOT=$(dirname $0)/..
OBJDIR=$ROOT/bin

LOG=$ROOT/test.log
rm -f $LOG

export PATH=/cygdrive/c/cygwin64/home/Yura/src/minisat-2.2.0/core:$PATH

if test "$1" = obj; then
  ext=.exe
  shift
else
  ext=.sh
fi

if [ $# -eq 1 ]; then
  kk=$1
elif [ $# -eq 2 ]; then
  kk=$(seq $1 $2)
else
  echo >&2 "Syntax: $ME [obj] [from] to"
  exit 1
fi

# Avoid overflowing memory and killing machine
export GHCRTS="$GHCRTS -M1G"

trap 'rm -f board_*.cnf' EXIT

# Some hints on selecting a filling percentage:
# * anything higher than 30% will cause current generator
#   to frequently produce unsolvable boards
# * less than 20% (=17/81) is generally believed
#   to be extra-difficult for human solver
fill_ratio=20

for k in $kk; do
  sz=$(($k * $k))
  nelts=$(($sz*$sz))
  ninits=$(($fill_ratio * $nelts / 100))
  echo "=== ${sz}x${sz} ($ninits/$nelts initialized) ==="
  $OBJDIR/GenRand$ext $sz $ninits 0 | tee -a $LOG
  ($OBJDIR/GenRand$ext $sz $ninits 0 | time -f 'Time: %e s' $OBJDIR/Solve$ext) 2>&1 | tee -a $LOG
  echo
done

