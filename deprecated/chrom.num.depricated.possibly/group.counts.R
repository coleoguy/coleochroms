setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/analyses/chrom.evol/")
groups <- c('Cerambycidae', 'Chrysolina', 'Chrysomelidae', 'Timarcha', 
            'Tenebrionidae', 'Dendroctonus', 'Curculionidae','Scarabaeidae', 
            'Ips', 'Elateridae', 'Dytiscidae', 'Coccinellidae', 
            'Lampyridae', 'Pterostichus', 'Bembidion', 'Cicindela', 'Carabidae',
            'Harpalus', 'Cytronus', 'Calathus', 'Pimelia')

results<- vector()
library(ape)
for(i in 1:length(groups)){
  foo <- read.tree(paste(groups[i], "/", groups[i],"1.tree", sep=""))
  results[i] <- length(foo$tip.label)
}
names(results)<-groups

results2 <- vector()
for(i in 1:length(groups)){
  foo <- readLines(paste(groups[i], "/", groups[i],".txt", sep=""))
  results2[i] <- length(unique(foo[seq(from=2, to=length(foo), by=2)]))
}
names(results2) <-groups