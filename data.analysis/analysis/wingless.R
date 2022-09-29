library(chromePlus)
library(coda)
library(diversitree)
trees <- read.nexus("../data/trees.nexus")
chroms <- read.csv("../data/hap.auto.csv", as.is=T)
# state 1 is winged
#lets prune trees
library(geiger)
row.names(chroms) <- chroms$name

# here we scale our trees to unit length and store there orginal depth
trees.p <- list()
scaler <- c()
for(i in 1:100){
  print(i)
  trees.p[[i]] <- treedata(trees[[i]], chroms)[[1]]
  scaler[i] <- max(branching.times(trees.p[[i]]))
  trees.p[[i]]$edge.length <- trees.p[[i]]$edge.length/scaler[i]
}

# here we match up our tip data and our
trees <- trees.p
class(trees) <- "multiPhylo"
x <- row.names(treedata(trees[[1]], chroms)[[2]])
chroms <- chroms[chroms$name%in%x,]
rm(trees.p,i,x)
range(chroms$chrom.num)


# we will work in parallele to speed things up
results <- vector(length=100, mode="list")
library(doMC)
# this is the number of cores to use
registerDoMC(12)
iter.full <- 200

# these values come from a sample of a run of 500 generations on each
# of 5 randomly chosen trees
# new.w <- diff(sapply(foo[2:6], quantile, c(.05, .95)))
# new.w <- diff(sapply(foo[2:8], quantile, c(.05, .95)))

# This stores the result for the w parameter for the run with no polyploidy
# new.w <- c(3.47425, 3.753301, 4.987116, 5.412818, 0.4839324)

# This stores the result for the w parameter for the run with polyploidy
new.w <- c(2.838092, 4.167098, 4.646785, 4.640986, 0.8953486, 2.263753, 0.5526955)

# now we set up our chrom matrix
wingstate <- as.numeric(chroms$wings.present>=.5)
current.chroms <- chroms
current.chroms$wings.present <- wingstate
chrom.mat <- datatoMatrix(current.chroms, range=c(8,30), hyper=T)

x <- foreach (i = 1:length(trees)) %dopar% {

  # here we just create our likelihood function
  lk.mk <- make.mkn(trees[[i]], states=chrom.mat,
                    k=46, strict=F,
                    control=list(method="ode"))
  # this version creates a one way model where wings can
  # not be regained and allows for polyploidy
  con.lk.mk<-constrainMkn(data=chrom.mat,
                          lik=lk.mk,
                          hyper=T,
                          polyploidy=F,
                          verbose=F,
                          oneway=T,
                          constrain=list(drop.demi=T, drop.poly=F))
  # This is the version of the model that does not allow for polyploidy
  # con.lk.mk<-constrainMkn(data=chrom.mat,
  #                         lik=lk.mk,
  #                         hyper=T,
  #                         polyploidy=F,
  #                         verbose=F,
  #                         oneway=T,
  #                         constrain=list(drop.demi=T, drop.poly=T))
  # we use a broad prior
  prior <- make.prior.exponential(2)
  argnames(con.lk.mk)
  print(paste("Working on dataset", i))
  results[[i]] <- diversitree::mcmc(con.lk.mk,
                                    x.init = runif(7, min = 0, max = 10),
                                    prior = prior, upper = 100, w = new.w,
                                    print.every=10, nsteps = iter.full)
}
# here we discard the first 100 generations as burnin
# we also convert this to rates in millions of years
post.sample <- x[[1]][101:200, 2:8]/scaler[1]
for(i in 2:Ntrees){
  post.sample <- rbind(post.sample,
                       x[[i]][101:200, 2:8]/scaler[i])
}
# save our results state 1 wings present

bar <- data.frame(c(post.sample[,3]-post.sample[,1],
                    post.sample[,4]-post.sample[,2],
                    post.sample[,6]-post.sample[,5],
                    rowMeans(post.sample[,c(3,4,6)])-rowMeans(post.sample[,c(1,2,5)])),
                  c(rep("fissions",10000),
                    rep("fusions",10000),
                    rep("wgd",10000),
                    rep("delta.mean.rate",10000)))

colnames(bar) <- c("rate","type")

write.csv(bar, "wingless.wpoly.chromeplus.csv", row.names=F)


