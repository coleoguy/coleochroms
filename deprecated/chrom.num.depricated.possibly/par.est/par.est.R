library(ape)
library(geiger)

## First we will pull in all of the data 
setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/analyses/brownian")

trees <- read.nexus("../../data/trees.nexus")
groups <- read.csv('../../data/groups.csv', as.is = T)
data <- read.csv('../../data/chrom.nums.csv', as.is = T)
data.v <- data[,2]
names(data.v) <- data[,1]
rm(data)
groups <- groups[,c(1,3,6:9,11,13,15,22:22)]

setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/figures/model.adequacy")
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
## a little clean up
rm(dat, data.v, foo, i, k, phy, sub.tree, trees)
groups <- colnames(groups)

## Fit the two alternative models 
## record AICs and likelihood of preffered model and parameters

results <- as.data.frame(matrix(,10,16))
colnames(results) <- c("group", "tree", 
                       "sigma2.lb","sigma2.ub","z.lb","z.ub", "lh","aicc",
                       "sigma2.lb","sigma2.ub", "alpha.lb","alpha.ub","z.lb","z.ub","lh","aicc")
counter <- 1
for(i in 1:length(groups)){
  data <- read.csv(paste(groups[i], "/data.csv", sep=""))
  foo.data <- data[,2]
  names(foo.data) <- data[,1]
  for(k in 1:100){
    tree <- read.tree(paste(groups[i], "/", groups[i], k, ".tree", sep=""))
    foo.bm <- fitContinuous(tree, foo.data, model="BM", control= list(hessian= T))
    foo.ou <- fitContinuous(tree, foo.data, model="OU", control= list(hessian= T, niter=500))
    if(!is.na(foo.bm$opt$CI)){
      flag1 <- T
      if(!is.na(foo.ou$opt$CI)){
        flag2 <- T
        results[counter,1:16] <- c(groups[i], k, foo.bm$opt$CI[,1], foo.bm$opt$CI[,2], 
                                   foo.bm$opt$lnL, foo.bm$opt$aicc,
                                   foo.ou$opt$CI[,1], foo.ou$opt$CI[,2], foo.ou$opt$CI[,3],
                                   foo.ou$opt$lnL, foo.ou$opt$aicc)
        counter <- counter + 1
      }
    }
    if(flag1 == F) print("BM Failed")
    if(flag2 == F) print("OU Failed")
    if(flag1 == T & flag2 == T) print(paste("Done with group:", i, groups[i], "Tree:", k))
    flag1 <- F
    flag2 <- F
  }
}

## lets save these results since they took soooo long
write.csv(results, file="par.est.csv")
data <- read.csv("par.est.csv", as.is=T)
        
## Simulate data under prefered model and calculate the likelihood
## of the ML point for all simulated datesets if observed likelihood
## is not an outlier then we fail to reject the chosen model as the 
## generating model for our observed data.


colSums(groups!="")


