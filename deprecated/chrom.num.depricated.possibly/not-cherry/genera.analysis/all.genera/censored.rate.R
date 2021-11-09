setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/not-cherry/genera.analysis/all.genera")
library(geiger)
library(phytools)
# first adephaga
data <- read.csv("ade.only.csv", as.is=T)
row.names(data) <- data[, 1]
data <- data[, 2:3]
trees <- read.nexus("../../../data/trees.nexus")
new.trees <- vector("list", length=100)
for(i in 1:100){
  new.trees[[i]] <- treedata(trees[[i]], data)[[1]]
}
class(new.trees) <- "multiPhylo"
rm(i, trees)
tip.states <- data[, 2]
names(tip.states) <- row.names(data)
foo2 <- make.simmap(new.trees, tip.states, maxit=200000, model = matrix(c(0,0,1,0,0,2,0,0,0),3), nsim = 1, pi=c(1,1, 10000))
##### support for the ard model p.val = .0312#####
chrom.num <- as.numeric(data[, 1])
names(chrom.num) <- row.names(data)
result <- list()
for(i in 1:100){
  result[[i]] <- brownie.lite(foo2[[i]], chrom.num, maxit=200000, test="chisq", se=NULL)
  print(i)
}
model1 <- model2 <- vector()
for(i in 1:100){
  model1[i] <- result[[i]]$logL1
  model2[i] <- result[[i]]$logL.multiple
}
mean(model1)
#[1] -399.9665
mean(model2)
#[1] -305.7565
all.models <- vector()
for(i in 1:100){
  all.models[i] <- 1-pchisq((2*(-mean(model1[i]) + mean(model2[i]))), 1)
}                 
#pval to reject one rate < .01 for all stochastic mappings
#parameter estimates
bar1 <- bar2 <- matrix(,100,3)
bar3<- vector()
for(i in 1:100){
  bar1[i, ] <- result[[i]]$sig2.multiple
  bar2[i, ] <- sqrt(diag(result[[i]]$vcv.multiple))
  bar3[i] <- result[[i]]$convergence
}
colMeans(bar1[, ])
colMeans(bar2[, ])
##ADEPHAGA
# rate estimates for low, med, high pop size
# 24.4017  0.09073  0.36900
# se for these estimates
# 8.6278 0.034046 0.0576247
write.csv(bar1, file="ade.3rates.csv")
#####################
# second polyphaga  #
#####################

setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/not-cherry/genera.analysis/all.genera")
data <- read.csv("pol.only.csv", as.is=T)
row.names(data) <- data[, 1]
data <- data[, 2:3]
trees <- read.nexus("../../../data/trees.nexus")
new.trees <- vector("list", length=100)
for(i in 1:100){
  new.trees[[i]] <- treedata(trees[[i]], data)[[1]]
}
class(new.trees) <- "multiPhylo"
rm(i, trees)
tip.states <- data[, 2]
names(tip.states) <- row.names(data)
foo2 <- make.simmap(new.trees, tip.states, maxit=200000, model = matrix(c(0,0,1,0,0,2,0,0,0),3), nsim = 1, pi=c(1,1, 10000))
##### support for the ard model p.val = .0312#####
chrom.num <- as.numeric(data[, 1])
names(chrom.num) <- row.names(data)
result <- list()
for(i in 1:100){
  result[[i]] <- brownie.lite(foo2[[i]], chrom.num, maxit=200000, test="chisq", se=NULL)
  print(i)
}
model1 <- model2 <- vector()
for(i in 1:100){
  model1[i] <- result[[i]]$logL1
  model2[i] <- result[[i]]$logL.multiple
}
mean(model1)
#[1] -428.5342
mean(model2)
#[1] -384.6181
all.models <- vector()
for(i in 1:100){
  all.models[i] <- 1-pchisq((2*(-mean(model1[i]) + mean(model2[i]))), 1)
}                 
#pval to reject one rate < .01 for all stochastic mappings
#parameter estimates
bar1 <- bar2 <- matrix(,100,3)
for(i in 1:100){
  bar1[i, ] <- result[[i]]$sig2.multiple
  bar2[i, ] <- sqrt(diag(result[[i]]$vcv.multiple))
}
colMeans(bar1)
colMeans(bar2)
write.csv(bar1, file="pol.3rates.csv")
##POLYPHAGA
# rate estimates for low, med, high pop size
# 0.62442440 0.06518342 0.01774571
# se for these estimates
# 0.097959471 0.013405473 0.009306478