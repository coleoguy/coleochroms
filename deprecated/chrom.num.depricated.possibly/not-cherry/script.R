setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/not-cherry")
data <- read.csv("data.csv", as.is=T)

# remove NA data & add row names
data <- data[!is.na(data[, 3]), ]
row.names(data) <- data[, 1]

# read the tree
library(ape)
trees <- read.nexus("../data/trees.nexus")

# make a dataset in the make.simmap format for the focal taxa
library(geiger)
foo <- treedata(trees[[1]], data)
data <- foo[[2]]
regime.data <- data[, c(3,4)]
sim.data <- cbind(as.numeric(regime.data[,1]), as.numeric(regime.data[,2]))
row.names(sim.data) <- row.names(regime.data)
sim.data <- as.matrix(sim.data)
colnames(sim.data) <- c("winged", "wingless")

# now lets do a multiphylo object as well
new.trees <- vector("list", length=100)
for(i in 1:100){
  new.trees[[i]] <- treedata(trees[[i]], data)[[1]]
}
class(new.trees) <- "multiPhylo"

# now clean up
rm(foo, i, trees, regime.data)


# do the ancestral state reconstructions
library(phytools)
foo1 <- make.simmap(new.trees, sim.data, maxit=200000, model = "SYM", nsim = 5, pi = c(1, 0))
#foo2 <- make.simmap(new.trees, sim.data, maxit=200000, model = "ARD", nsim = 5, pi = c(1, 0))
chrom.num <- as.numeric(data[, 2])
names(chrom.num) <- row.names(sim.data)

##### lets compare the sym and ard model #####

#sym.log <- vector()
#for(i in 1:500){
#  sym.log[i] <- foo1[[i]]$logL
#}
#ard.log <- vector()
#for(i in 1:500){
#  ard.log[i] <- foo2[[i]]$logL
#}
#
#1-pchisq((2*(-mean(sym.log) + mean(ard.log))), 1)
##### no support for the ard model #####


result <- list()
for(i in 1:500){
  result[[i]] <- brownie.lite(foo1[[i]], chrom.num, maxit=200000, test="chisq", se=NULL)
  print(i)
}

model1 <- model2 <- vector()
for(i in 1:500){
  model1[i] <- result[[i]]$logL1
  model2[i] <- result[[i]]$logL.multiple
}
mean(model1)
#[1] -507.4481
mean(model2)
#[1] -465.9293

1-pchisq((2*(-mean(model1)+mean(model2))), 1)


wingless <- winged <- both <- vector()
for(i in 1:500){
  winged[i] <- result[[i]]$sig2.multiple[1]
  wingless[i] <- result[[i]]$sig2.multiple[2]
  both[i] <- result[[i]]$sig2.single
}
plot(winged, pch=19, col="blue", ylim=c(0,60))
points(wingless, pch=19, col="red")
results2 <- cbind(winged,wingless)
write.csv(results2, file="bm.rates.csv")




# tree for talk
plot(foo[[1]], type="fan", show.tip.label=F, edge.width=2)
picker <- colorRampPalette(c("red","blue"),space = "rgb")
picker <- picker(101)
tipcols <- vector()
for(i in 1:136){
  tipcols[i] <- picker[(sim.data[row.names(sim.data) == foo[[1]]$tip.label[i], 1] + .01) * 100]
}
tiplabels(, pch=19, cex=1.5,col=tipcols)
points(1,1,col=picker[1])
