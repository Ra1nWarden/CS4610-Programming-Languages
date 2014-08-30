#!/bin/sh

for each in `ls cool-examples/*.cl`
do
    diff -w -b -B -e $each-type $each+ans.cl-type
done
