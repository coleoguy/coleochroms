## This script will fit a BM model to the evoltuion of chromosome number in
## each genera being studied.  The full fit object is returned to be used in
## conjunction with arbutrus
## Heath Blackmon
## 28 April 2015
## coleoguy@gmail.com
library(geiger)
taxa   <- c('Bembidion', 'Calathus', 'Chrysolina', 
            'Cicindela', 'Cytronus', 'Dendroctonus', 
            'Diabrotica', 'Harpalus', 'Ips', 'Pimelia', 
            'Pterostichus', 'Timarcha')
results <- list()

for(j in 1:length(taxa)){
  data <- read.csv(paste("../data/genus.data.trees/",
                         taxa[j], 
                         "/data.csv", 
                         sep = ""))
  chrom <- as.numeric(data[,2])
  names(chrom) <- data[,1]
  chrom2 <- chrom ## a duplicate for sorting for arbutus
  print(taxa[j])
  group.results <- list()
  for(i in 1:100){
    cat(".")
    tree <- read.tree(paste("../data/genus.data.trees/", 
                            taxa[j], 
                            "/", 
                            taxa[j], 
                            i,
                            ".tree", 
                            sep=""))
    for(k in 1:length(tree$tip.label)){
      chrom2[k] <- chrom[which(names(chrom) == tree$tip.label[k])]
      names(chrom2)[k] <- names(chrom)[which(names(chrom) == tree$tip.label[k])]
    }
    group.results[[i]] <- fitContinuous(phy=tree, 
                            dat=chrom2, 
                            model="BM")
  }
  results[[j]] <- group.results
}
save(results, file="../results/genera.bm")
