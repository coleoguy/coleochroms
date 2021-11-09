#!/bin/bash
for j in Harpalus Cytronus Calathus Pimelia 
do
  cd /Users/hlb7922/Desktop/Dropbox/papers/chrom.num/data\ and\ analysis/analyses/chrom.evol/$j
  for i in {1..20}
  do
    chromEvol params$i.txt
  done
done
  