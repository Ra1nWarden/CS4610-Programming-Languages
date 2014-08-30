#!/bin/sh
for EACH in `ls $1`
do
  ./cool --type $1$EACH
done
