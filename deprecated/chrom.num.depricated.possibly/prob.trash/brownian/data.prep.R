setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/analyses/brownian")
library(ape)
library(geiger)

# Get the data in
trees <- read.nexus("../../data/trees.nexus")
groups <- read.csv('../../data/groups.csv', as.is = T)
data <- read.csv('../../data/chrom.nums.csv', as.is = T)
data.v <- data[,2]
names(data.v) <- data[,1]
rm(data)

# Create input data files fasta format for chrom numbers and all the trees
for(i in 1:ncol(groups)){
  dir.create(colnames(groups[i]))
  foo <- treedata(trees[[1]], data.v[names(data.v) %in% groups[,i]])
  phy <- foo[[1]]
  dat <- as.data.frame(foo[[2]])
  dat[,2]<-dat[,1]
  dat[,1]<- row.names(dat)
  colnames(dat) <- c('species', 'chrom.num')
  write.csv(dat, row.names = F, file = paste(colnames(groups[i]), "/data.csv", sep = ""))  
  for( k in 1:length(trees)){
    sub.tree <- treedata(trees[[k]], data.v[names(data.v) %in% groups[,i]])[[1]]
    write.tree(sub.tree, file= paste(colnames(groups[i]), "/", 
                                     colnames(groups)[i], k, 
                                     ".tree", sep = ""))
  }
}
results <- matrix(, 100, 23)
colnames(results) <- colnames(groups)

for(i in 1:ncol(groups)){
  chrom.nums <- read.csv(paste(colnames(groups[i]), "/data.csv", sep = ""))
  chrom.dat <- as.numeric(chrom.nums[,2])
  names(chrom.dat) <- chrom.nums[,1]
  for(j in 1:length(trees)){
    test.tree <- read.tree(paste(colnames(groups[i]), 
                                 "/", paste(colnames(groups[i])),
                                 j, ".tree", sep = ""))
    if(!is.nan(ace(chrom.dat, test.tree)[[3]][2])){
      results[j,i] <- ace(chrom.dat, test.tree)[[3]][1]
    }
  }
}