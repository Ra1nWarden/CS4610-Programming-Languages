#!/bin/sh
for EACH in `ls $1*.cl`
do
  ./cool $EACH > input1.txt
  ./a.out $EACH-type > input2.txt
  diff input1.txt input2.txt
done

for EACH in `ls $1inputs/*.cl`
do
  ./cool $EACH < $EACH-input > input1.txt
  ./a.out $EACH-type < $EACH-input > input2.txt
  diff input1.txt input2.txt
done
