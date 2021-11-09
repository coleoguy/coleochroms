## This script looks at the normality of 
## chromosome number data and whether it
## should be log transformed 

## does log transformation improve Sasr 
## in arbutus?
data <- read.csv("../data/carab.wingless.csv", as.is=T)

# remove NA data & add row names
data <- data[!is.na(data[, 3]), ]
row.names(data) <- data[, 1]

# read the tree
library(geiger)
trees <- read.nexus("../data/trees.nexus")

# make a dataset in the make.simmap format for the focal taxa
foo <- treedata(trees[[1]], data)
data <- foo[[2]]

# now lets do a multiphylo object as well
new.trees <- vector("list", length=100)
for(i in 1:100){
  new.trees[[i]] <- treedata(trees[[i]], data)[[1]]
}
class(new.trees) <- "multiPhylo"

# lets make names clear
chrom.data <- data

# now clean up

rm(foo, i, data, trees, regime.data)

# get chromosome number data into a vector for fitContinuous
chrom.num <- as.numeric(chrom.data[, 2])
names(chrom.num) <- row.names(wing.data)

rm(chrom.data)

result <- list()
for(i in 1:500){
  result[[i]] <- brownie.lite(mapped.trees1[[i]], chrom.num, maxit=200000, test="chisq", se=NULL)
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
write.csv(results2, file="../results/wing.bm.rates.csv", row.names=F)

