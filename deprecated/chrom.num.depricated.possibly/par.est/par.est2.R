## lets get the AICc scores even if we cant get the CI from the hessian

## first lets workout the trees we had trouble with
setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/figures/model.adequacy")
pars <- read.csv("par.est.csv", as.is=T)

need <- matrix(,100,9)
colnames(need) <- unique(pars[,2])[c(1:7,9:10)]

for(i in 1:9){
  foo <- length(which(!1:100 %in% pars[pars[, 2] == colnames(need)[i], 3]))
  need[1:foo,i] <- which(!1:100 %in% pars[pars[, 2] == colnames(need)[i], 3])
}

## ok so now we lists of the trees that we need to look at
setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/figures/model.adequacy")
new.results <- as.data.frame(matrix(,1,4))
colnames(new.results) <- c("group", "tree", "BM", "OU")
counter <- 1
for(j in 1:9){
  fam <- colnames(need)[j]
  data <- read.csv(paste(fam, "/data.csv", sep=""))
  chrom <- data[,2]
  names(chrom) <- data[,1]
  for(i in 1:100){
    if(!is.na(need[i, j])){
      new.results[counter,1] <- fam
      new.results[counter,2] <- need[i, j]
      tree <- read.tree(paste(fam, "/", fam, need[i, j], ".tree", sep=""))
      new.results[counter, 3] <- fitContinuous(tree, chrom, model="BM")$opt$aicc
      new.results[counter, 4] <- fitContinuous(tree, chrom, model="OU")$opt$aicc
      counter <- counter + 1
    }
  }
  print(fam)
}
fams <- unique(new.results[,1])
add.res <- matrix(,9,5)
colnames(add.res) <- c("group", "BM", "OU", "bm.aicc", "ou.aicc")
for(i in 1:9){
  bm <- new.results[new.results[, 1] == fams[i], 3]
  ou <- new.results[new.results[, 1] == fams[i], 4]
  add.res[i,1] <- fams[i]
  add.res[i,2] <- sum(bm < ou)
  add.res[i,3] <- sum(bm > ou)
  add.res[i,4] <- mean(bm)
  add.res[i,5] <- mean(ou)
}

### now we will combine the results to get the correct waited averages
### have to runt he code in the file parsing results for this to work

results1 <- pars[pars[,2] != "Pimelia",c("group","aicc", "aicc.1")]
results2 <- new.results[,c(1,3,4)]
colnames(results1) <- c("group", "BM", "OU")
test <- rbind(results1, results2)



fams <- unique(test[,1])
test[, 2] <- as.numeric(test[, 2])
test[, 3] <- as.numeric(test[, 3])
add.res <- matrix(,9,5)
colnames(add.res) <- c("group", "BM", "OU", "bm.aicc", "ou.aicc")
for(i in 1:9){
  bm <- test[test[, 1] == fams[i], 2]
  ou <- test[test[, 1] == fams[i], 3]
  add.res[i,1] <- fams[i]
  add.res[i,2] <- sum(bm < ou)
  add.res[i,3] <- sum(bm > ou)
  add.res[i,4] <- round(mean(bm), digits=2)
  add.res[i,5] <- round(mean(ou), digits=2)
}
## lets save the results of the model comparison
write.csv(add.res, file="model.com.csv")

