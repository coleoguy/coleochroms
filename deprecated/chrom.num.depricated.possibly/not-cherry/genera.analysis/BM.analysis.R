## This script will fit a BM model to the evoltuion of chromosome number in each group
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
    group.results[[i]] <- fitContinuous(phy=tree, 
                            dat=chrom, 
                            model="BM")
  }
  results[[j]] <- group.results
}
save(results, file="../results/genera.bm")
