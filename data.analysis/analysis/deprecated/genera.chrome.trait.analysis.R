library(chromePlus)
library(diversitree)
library(geiger)
library(doMC)
registerDoMC(7)
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
  chrom <- ceiling(as.numeric(data[,2])/2)
  names(chrom) <- data[,1]
  print(taxa[j])
  Ntrees <- 100
  iter <- 500
  x <- foreach (i = 1:Ntrees) %dopar% {
    tree <- read.tree(paste("../data/genus.data.trees/", taxa[j], "/", taxa[j], 
                            i, ".tree", sep=""))
    treescale <- max(branching.times(tree))
    tree$edge.length <- tree$edge.length/treescale
    chrom.range <- range(chrom) + c(-3, 3)
    if(j == 8) chrom.range <- c(12, 24)
    current.chroms <- data.frame(names(chrom), chrom, rep(1, length(chrom)))
    chrom.mat <- datatoMatrix(current.chroms, 
                              range = chrom.range, 
                              hyper = F)
    lk.mk <- make.mkn(tree, states=chrom.mat,
                      k=ncol(chrom.mat), strict=F,
                      control=list(method="ode"))
    con.lk.mk<-constrainMkn(data=chrom.mat,
                             lik=lk.mk, hyper=F,
                             polyploidy=F, verbose=F,
                             constrain=list(drop.demi=T))
    prior <- make.prior.exponential(1)
    # this comes from a trial run of length 500
    W <- c(1.5,.8,3)
    samp <- diversitree::mcmc(con.lk.mk,
                                        x.init = runif(3, min = 0, max = 1),
                                        prior = prior, 
                                        upper = 40,
                                        w = W, print.every=100, 
                                        nsteps = iter)
    samp <- samp/treescale
    pburn <- (.2*nrow(samp)+1):nrow(samp)
    samp <- samp[sample(pburn, 100),2:4]
    samp
  }
  post <- x[[1]]
  for(ii in 2:Ntrees){
    post <- rbind(post, x[[ii]])
  }
  results[[j]] <- post
}


full.results <- vector(mode="list", length=3)
asc <- matrix(,10000, 12)
colnames(asc) <- taxa
for(i in 1:12){
  asc[,i] <- results[[i]][,1]
}
desc <- matrix(,10000, 12)
colnames(desc) <- taxa
for(i in 1:12){
  desc[,i] <- results[[i]][,2]
}
pol <- matrix(,10000, 12)
colnames(pol) <- taxa
for(i in 1:12){
  pol[,i] <- results[[i]][,3]
}
full.results[[1]] <- asc
full.results[[2]] <- desc
full.results[[3]] <- pol

# saved as genera.chromeplus.Rdata

