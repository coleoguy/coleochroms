#!/bin/bash
for j in Harpalus Cytronus Calathus Pimelia
do
  cd /Users/hlb7922/Desktop/Dropbox/papers/chrom.num/data\ and\ analysis/analyses/chrom.evol
  for i in {1..20}
  do
     for k in {1..5}
     do
       chromEvol simmed.data/$j.params.$i.$k.txt
    done
  done
done
  