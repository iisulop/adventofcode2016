#!/bin/bash

INPUTFILE=$1
CORRECT1=""
CORRECT2=""

for i in `seq 1 8` ; do
  REST1=`cut -c $i $INPUTFILE | sort | uniq -c | sort -r | head -n 1 | awk '{print $2}'`
  REST2=`cut -c $i $INPUTFILE | sort | uniq -c | sort | head -n 1 | awk '{print $2}'`
  CORRECT1=${CORRECT1}${REST1}
  CORRECT2=${CORRECT2}${REST2}
done

echo "Part 1: $CORRECT1"
echo "Part 2: $CORRECT2"
