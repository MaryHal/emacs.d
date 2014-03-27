#!/usr/bin/env bash

echo 0 > tmp.txt
echo "" | clang++ -v -E -x c++ - 1>>tmp.txt 2>>tmp.txt

OUTPUT_SIZE=`cat tmp.txt | wc -l`
UPPER_PATTERN='#include <...> search starts here:'
BOTTOM_PATTERN='End of search list'

cat tmp.txt | grep -A $OUTPUT_SIZE "$UPPER_PATTERN" | grep -B $OUTPUT_SIZE "$BOTTOM_PATTERN" | grep -v "$UPPER_PATTERN" | grep -v "$BOTTOM_PATTERN" | grep -vi "(framework directory)"
cat tmp.txt | grep -A $OUTPUT_SIZE "$UPPER_PATTERN" | grep -B $OUTPUT_SIZE "$BOTTOM_PATTERN" | grep -v "$UPPER_PATTERN" | grep -v "$BOTTOM_PATTERN" | grep -i "(framework directory)" | while read line ; do echo ${line:0:${#line}-22} ; done

rm tmp.txt
