#!/bin/sh

for i in `ls cool-examples/*.cl-lex`
do
node main.js $i
./cool --out $i".ans" --parse $i
done

for i in `ls cool-examples/*.cl`
do
diff -w -b -B $i".cl-ast" $i".cl-lex.ans.cl-ast"
done
