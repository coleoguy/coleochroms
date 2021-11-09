setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/figures/par.est")


## First lets just set up the results table for later fill in
results <- as.data.frame(matrix(,1,10))
colnames(results) <- c("group", "alpha", "sigma", "z", 
                       "lb.alpha", "lb.sigma", "lb.z", 
                       "ub.alpha", "ub.sigma", "ub.z")
                       


data <- read.csv("par.est.csv", as.is=T)
par.means <- read.csv("par.means.csv", as.is=T)
fams <- unique(par.means[,2])
for(i in 1:9){
  fam <- fams[i]
  results[i,1] <- fam
  results[i,2] <- mean(par.means[par.means[,2] == fam, "alpha"])
  results[i,3] <- mean(par.means[par.means[,2] == fam, "sigma"])
  results[i,4] <- mean(par.means[par.means[,2] == fam, "z"])
  results[i,5] <- min(data[data[,2] == fam, "alpha.lb"], na.rm = T)
  results[i,6] <- min(data[data[,2] == fam, "sigma2.lb"], na.rm = T)
  results[i,7] <- min(data[data[,2] == fam, "z.lb.1"], na.rm = T)
  results[i,8] <- max(data[data[,2] == fam, "alpha.ub"], na.rm = T)
  results[i,9] <- max(data[data[,2] == fam, "sigma2.ub"], na.rm = T)
  results[i,10] <- max(data[data[,2] == fam, "z.ub.1"], na.rm = T)
}

## now we want to create a graph that has a symbol for the means with error 
## bars representing the worst and mean 95% CI from across trees

plot(results[1:9,"ub.sigma"])
points(results[1:9,"lb.sigma"])
fams <- results$group
plot(density(par.means[par.means$group == fams[9],"sigma"]))


median(par.means[par.means$group == fams[9],"sigma"])