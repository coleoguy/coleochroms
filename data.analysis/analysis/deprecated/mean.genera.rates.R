# Get time spent in states


# This function takes residence times from a stochastic map,
# a model from chromeplus and parameter estimates from MCMC
# and calculates the weighted average rate of chromosome numer
# mutations
getWeightedRates <- function(mod, pars, times = sim.vals[-length(sim.vals)]){
  summed.rate <- 0
  for(i in 1:length(times)){
    # check for ascending
    if(1 %in% mod[i,]) summed.rate <- summed.rate + times[i] * pars[1]
    if(2 %in% mod[i,]) summed.rate <- summed.rate + times[i] * pars[2]
    if(5 %in% mod[i,]) summed.rate <- summed.rate + times[i] * pars[3]
  }
  return(as.numeric(summed.rate))
}


library(phytools)
library(doMC)
library(chromePlus)
library(diversitree)
library(sfreemap)
registerDoMC(7)
taxa   <- c('Bembidion', 'Calathus', 'Chrysolina', 
            'Cicindela', 'Cytronus', 'Dendroctonus', 
            'Diabrotica', 'Harpalus', 'Ips', 'Pimelia', 
            'Pterostichus', 'Timarcha')
load("../results/genera.chromeplus.RData")
results <- vector(mode="list",length=12)
names(results) <- taxa
for(j in 1:length(taxa)){
  data <- read.csv(paste("../data/genus.data.trees/",
                         taxa[j], 
                         "/data.csv", 
                         sep = ""))
  chrom <- ceiling(as.numeric(data[,2])/2)
  names(chrom) <- data[,1]
  print(taxa[j])
  Ntrees <- 3
  spots <- seq(from=50, by=100, length.out=100)
  mean.rates <- c()
  for(i in 1:Ntrees){
    print(paste("tree", i))
    tree <- read.tree(paste("../data/genus.data.trees/", taxa[j], "/", taxa[j], 
                            i, ".tree", sep=""))
    chrom.range <- range(chrom) + c(-3, 3)
    if(j == 8) chrom.range <- c(12, 24)
    current.chroms <- data.frame(names(chrom), chrom, rep(1, length(chrom)))
    chrom.mat <- datatoMatrix(current.chroms, 
                              range = chrom.range, 
                              hyper = F)
    lk.mk <- make.mkn(tree, states=chrom.mat,
                      k=ncol(chrom.mat), strict=F,
                      control=list(method="ode"))
    mod <- constrainMkn(data=chrom.mat,
                        lik=lk.mk, hyper=F,
                        polyploidy=F, verbose=T,
                        constrain=list(drop.demi=T))[[2]]
    pars <- as.vector(c(full.results[[1]][spots[i],j],#asc
                        full.results[[2]][spots[i],j],#desc
                        full.results[[3]][spots[i],j]))#pol
    Q <- mod
    for(ii in 1:nrow(mod)){
      Q[ii, Q[ii, ]==1] <- pars[1]
      Q[ii, Q[ii, ]==2] <- pars[2]
      Q[ii, Q[ii, ]==5] <- pars[3]
    }
    tip.states <- current.chroms$chrom
    names(tip.states) <- current.chroms$names.chrom.
    sfreemap(tree=tree, tip_states = tip.states, Q=Q)
    
    foo <- make.simmap(tree=tree, model=mod, x=chrom.mat, pi="equal", Q=Q)
    sim.vals <- describe.simmap(foo)$times[2,]
    mean.rates[i] <- getWeightedRates(mod=mod, pars=pars)
  }
  results[[j]] <- mean.rates
}






