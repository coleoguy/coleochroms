library(chromePlus)
library(diversitree)
library(geiger)
library(doMC)
registerDoMC(9)
taxa   <- c('Bembidion', 'Calathus', 'Chrysolina',
            'Cicindela', 'Cytronus', 'Dendroctonus',
            'Diabrotica', 'Harpalus', 'Ips', 'Pimelia',
            'Pterostichus', 'Timarcha')
results <- list()
iter <- 500

for(j in 1:length(taxa)){
  data <- read.csv(paste("../data/genus.data.trees/",
                         taxa[j],
                         "/data.csv",
                         sep = ""))
  chrom <- ceiling(as.numeric(data[,2])/2)
  names(chrom) <- data[,1]
  print(taxa[j])
  Ntrees <- 100
  x <- foreach (i = 1:Ntrees) %dopar% {
    tree <- read.tree(paste("../data/genus.data.trees/", taxa[j], "/", taxa[j],
                            i, ".tree", sep=""))
    treescale <- max(branching.times(tree))
    tree$edge.length <- tree$edge.length/treescale
    chrom.range <- range(chrom) + c(-1, 1)
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
                            constrain=list(drop.demi=T, drop.poly=T))
    argnames(con.lk.mk)
    prior <- make.prior.exponential(1)
    # this comes from a trial run of length 500
    W <- 1
    samp <- diversitree::mcmc(con.lk.mk,
                              x.init = runif(2, min = 0, max = 1),
                              prior = prior,
                              upper = 40,
                              w = W,
                              nsteps = iter)
    # rescale to millions of years
    samp[,2:3] <- samp[,2:3]/treescale
    samp
  }
  post <- x[[1]][401:500,]
  for(ii in 2:Ntrees){
    post <- rbind(post, x[[ii]][401:500,])
  }
  results[[j]] <- post
}
names(results) <- taxa

save(results, file="../results/genera.wopoly.RData")
