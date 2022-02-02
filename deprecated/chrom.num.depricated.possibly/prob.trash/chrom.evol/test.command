#!/bin/bash
for j in Cerambycidae Chrysolina Chrysomelidae Timarcha Tenebrionidae Scarabaeidae Ips Elateridae Dytiscidae Diabrotica Dendroctonus Curculionidae Coccinellidae Lampyridae Pterostichus Bembidion Cicindela Carabidae 
do
  cd /Users/hlb7922/Desktop/Dropbox/papers/chrom.num/data\ and\ analysis/analyses/chrom.evol/$j
  for i in {1..20}
  do
    chromEvol params$i.txt
  done
done
  