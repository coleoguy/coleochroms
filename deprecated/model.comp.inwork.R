# This script evaluates support for models with
# 1, 2, or 3 parameters for chromosome evolution
# 1 = single parameter for fusions and fissions no polyploidy
# 2 = different parameters for fusion and fissions no polyploidy
# 3 = different parameters for fusion, fission, and polyploidy



library(chromePlus)
library(diversitree)
library(geiger)
library(doMC)
registerDoMC(14)


taxa   <- c('Bembidion', 'Calathus', 'Chrysolina',
            'Cicindela', 'Cytronus', 'Dendroctonus',
            'Diabrotica', 'Harpalus', 'Ips', 'Pimelia',
            'Pterostichus', 'Timarcha')
ranges <- matrix(NA, 12, 2)
row.names(ranges) <- taxa
for(j in 1:length(taxa)){
  data <- read.csv(paste("../data/genus.data.trees/",
                         taxa[j],
                         "/data.csv",
                         sep = ""))
  chrom <- ceiling(as.numeric(data[,2])/2)
  ranges[j, 1:2] <- range(chrom) + c(-2, 2)
}

results <- list()
for(j in 1:length(taxa)){
  data <- read.csv(paste("../data/genus.data.trees/",
                         taxa[j],
                         "/data.csv",
                         sep = ""))
  chrom <- ceiling(as.numeric(data[,2])/2)
  names(chrom) <- data[,1]
  print(taxa[j])
  Ntrees <- 100
  x <- foreach (i = 1:100, .combine=rbind) %dopar% { #Ntrees) %dopar% {
    tree <- read.tree(paste("../data/genus.data.trees/", taxa[j], "/", taxa[j],
                            i, ".tree", sep=""))
    treescale <- max(branching.times(tree))
    tree$edge.length <- tree$edge.length/treescale
    chrom.range <- range(chrom) + c(-2, 2)
    current.chroms <- data.frame(names(chrom), chrom, rep(1, length(chrom)))
    chrom.mat <- datatoMatrix(current.chroms,
                              range = chrom.range,
                              hyper = F)
    lk.mk <- make.mkn(tree, states=chrom.mat,
                      k=ncol(chrom.mat), strict=F,
                      control=list(method="ode"))
    con.lk1<-constrainMkn(data=chrom.mat,
                          lik=lk.mk, hyper=F,
                               polyploidy=F, verbose=F,
                               constrain=list(drop.demi=T, symmetric=T,
                                              drop.poly=T))
    con.lk2 <-constrainMkn(data=chrom.mat,
                                 lik=lk.mk, hyper=F,
                                 polyploidy=F, verbose=F,
                                 constrain=list(drop.demi=T, symmetric=F,
                                                drop.poly=T))
    con.lk3 <-constrainMkn(data=chrom.mat,
                                 lik=lk.mk, hyper=F,
                                 polyploidy=F, verbose=F,
                                 constrain=list(drop.demi=T, symmetric=F,
                                                drop.poly=F))
    fit1 <- find.mle(con.lk1, x.init = runif(1, 0, .1))
    fit2 <- find.mle(con.lk2, x.init = runif(2, 0, .1))
    #fit3 <- find.mle(con.lk3, x.init = runif(3, 0, .1))
    t(AIC(fit1, fit2))[2,]
  }
  results[[j]] <- x
}
hist(results[[1]][,1]-results[[1]][,2])
hist(results[[2]][,1]-results[[2]][,2])
hist(results[[3]][,1]-results[[3]][,2])
hist(results[[4]][,1]-results[[4]][,2])
hist(results[[5]][,1]-results[[5]][,2])
hist(results[[6]][,1]-results[[6]][,2])
hist(results[[7]][,1]-results[[7]][,2])
hist(results[[8]][,1]-results[[8]][,2])
hist(results[[9]][,1]-results[[9]][,2])
hist(results[[10]][,1]-results[[10]][,2])
hist(results[[11]][,1]-results[[11]][,2])
hist(results[[12]][,1]-results[[12]][,2])




bar <- list()
for(i in 1:50){
  bar[[i]] <- foo[[1]]
}
for(i in 51:100){
  bar[[i]] <- x[[(i-50)]]
}


