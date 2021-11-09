## Plot all
setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/figures/model.adequacy")
data <- read.csv("par.est.csv", as.is = T)

## first lets see if BM or OU worked better
results <- matrix(,10,5)
groups <- unique(data[, 2])
for(i in 1:10){
  bm.score <- data[data[,2] == groups[i],"aicc"]
  ou.score <- data[data[,2] == groups[i],"aicc.1"]
  results[i,2] <- sum(bm.score < ou.score)
  results[i,3] <- sum(bm.score > ou.score)
  results[i,1] <- groups[i]
  results[i,4] <- mean(bm.score)
  results[i,5] <- mean(ou.score)
}
colnames(results) <- c("group", "BM.better", "OU.better", "AVG.BM", "AVG.OU")