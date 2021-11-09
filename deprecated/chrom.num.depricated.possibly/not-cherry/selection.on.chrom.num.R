## lets make sure that we dont have a signal of different numbers
## of chromosomes in groups that have different states.
library(ape)
library(geiger)
# carabidae data
setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/not-cherry")
data <- read.csv("data.csv", row.names=1)
data <- data[complete.cases(data),]
tree <- read.nexus("../data/trees.nexus")
data <- treedata(tree[[1]], data)
tree <- data[[1]]
data <- data[[2]]
chrom.count <- data[,1]
pic.chrom <- pic(x=chrom.count, phy=tree)
flight.count <- data[,2]
pic.wing <- pic(x=flight.count, phy=tree)
cor.test(pic.chrom, pic.wing)
#pval for Carabidae = .6456
summary(lm(pic.chrom ~ pic.wing -1))

# genera data
tree <- read.nexus("../data/trees.nexus")
setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/not-cherry/genera.analysis/all.genera")
data <- read.csv("all.csv", row.names=1)
data <- treedata(tree[[1]], data)
tree <- data[[1]]
data <- data[[2]]
grp <- as.factor(data[,2])
names(grp) <- row.names(data)
resp <- data[,1]
names(resp) <- row.names(data)
aov.phylo(resp~grp, tree, nsim=1000)
#pval for anova of difference between groups = .8911