#!/bin/sh

for i in `ls cool-examples/*.cl`
do
python main.py $i
./cool --out $i-ans --lex $i
diff -b -B -E -w $i-ans.cl-lex $i-lex
done
