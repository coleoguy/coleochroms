library(ape)
## get the full tree
setwd("~/Desktop/Dropbox/papers/published/fragileY/analyses.and.data/misc/trees")
trees <- read.nexus("full.trees.nexus")
## get one to experiment with
tree <- trees[[25]]
## let get rid of all but one tip per genus
foo <- 1:1044
tip <- foo[!foo %in% c(1007,783,754,564,501,437,420,196,133,93,64,19)]
tree <- drop.tip(tree, tip)
## and check these to see if they are good
plot(tree)
## now lets enter the values
foo <- c(.148,2.68,.294,.355,.0602,.356,.89,.225,.00582,.0747,.00157,.0276)
ne <- c(2,0,2,2,1,0,0,0,2,1,1,0)
## now for a phylogenetically corrected anova
library(geiger)
## dependant variable
d1 <- c(.294, 2.68, .355, .06, .148, .356, .89, .276, .0058, .225, .0747, .0016)
names(d1) <- tree$tip.label
grp <- as.factor(c(2,0,2,1,2,0,0,0,2,0,1,1))
grp2 <- as.factor(c(2,0,2,2,2,0,0,0,2,0,2,2))
names(grp) <- tree$tip.label
names(grp2) <- tree$tip.label
aov.phylo(d1~grp, tree, nsim=100)
aov.phylo(d1~grp2, tree, nsim=100)
## how about if we split the groups
pol.tree <- drop.tip(tree, c(1,2,3,4,5))
pol.d1 <- d1[6:12]
pol.grp <- grp[6:12]
pol.grp2 <- grp2[6:12]

ade.tree <- drop.tip(tree, 6:12)
ade.d1 <- d1[1:5]
ade.grp <- grp[1:5]

aov.phylo(pol.d1 ~ pol.grp, pol.tree, nsim = 100)  # not significant
aov.phylo(pol.d1 ~ pol.grp2, pol.tree, nsim = 100)  # not significant pval = .059
aov.phylo(ade.d1 ~ ade.grp, ade.tree, nsim = 100)  # significant


## so no sig. with these test... perhaps we could try something like testing for 
## different rates? with brian omeara's approach.

