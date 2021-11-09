#This script calculates skew and kurtosis that goes into the main table



## redo this with all families greater than 100 species and all genera 
## with greater than 15  species



setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/data/fig1")
data <- read.csv("chrom.nums.csv", header=F, as.is=T)
Even <- function(x){
  if(x/2 == round(x/2)){
    return(T)
  }else{
    return(F)
  }
}

data<- data[,c(1:3,5)]
for(i in 1:4537){
  if(Even(data[i,4])){
    data[i,4] <- (data[i,4]-2)/2
  }else{
    data[i,4] <- (data[i,4]-1)/2
  }
}

fams <- unique(data[,1])
ade <- c("Dytiscidae", "Carabidae", "Gyrinidae", "Haliplidae", "Hygrobiidae", "Noteridae", "Trachypachidae")
pol <- fams[!fams %in% ade]
pol <- pol[c(1:4,6:37,40:52)]

ade.data <- data[data[,1] %in% ade,]
pol.data <- data[data[,1] %in% pol,]

foo <- as.data.frame(table(data[,1]))
families <- as.character(foo[foo[,2] > 99,1])

foo <- as.data.frame(table(data[,2]))
genera <- as.character(foo[foo[,2] > 14,1])
genera <- genera[c(1:13, 15:37, 39:52)]   # getting rid of clades with no variance

# a little cleaning
rm(data, ade,fams, i, pol, Even)

suborders <- c("Adephaga", "Polyphaga")

#so lets make a list of vectors with our counts
counts <- list()
counts[[1]]<- ade.data[,4]
counts[[2]]<- pol.data[,4]

temp <-rbind(ade.data,pol.data)
for(i in 1:length(families)) counts[[i+2]] <- temp[temp[,1] == families[i],4]
for(i in 1:length(genera)) counts[[i+13]] <- temp[temp[,2] == genera[i],4]

counts[[64]] <- c(ade.data[ade.data[,2] != "Bembidion",4],10, 11, 12, 15, 16, 17)
foo <- ade.data[ade.data[,2] != "Bembidion",]
counts[[65]] <- c(foo[foo[,1] == "Carabidae",4],10, 11, 12, 15, 16, 17)
post.hoc <- c("adephaga-bemb", "carabidae-bemb")

names(counts) <- c(suborders, families, genera, post.hoc)
    





# first test kurtosis in moments: value and p-value
# then test skew in e1071: value
library(moments)
library(e1071)
results <- as.data.frame(matrix(,1,6))
for(i in 1:length(counts)){
  foo <- anscombe.test(counts[[i]])
  results[i,1] <- names(counts)[i]
  results[i,2] <- length(counts[[i]])
  results[i,3] <- signif(unlist(foo[1])[1], digits=3)
  results[i,4] <- signif(unlist(foo[2])[1], digits=3)
  results[i,5] <- signif(skewness(counts[[i]], type=2), digits=3)
  boot.skew <- vector()
  for(j in 1:1000){
    foo <- rnorm(n=length(counts[[i]]), mean=mean(counts[[i]]), sd=sd(counts[[i]]))
    boot.skew[j] <- skewness(foo, type=2)
  }
  results[i,6] <- signif(sum(abs(boot.skew) > abs(results[i,5]))/1000, digits=3)
}

results2 <- results[1,]
results2[2,] <- results[64,]
results2[3:5,] <- results[2:4,] 
results2[6,] <- results[65,]
results2[7:65,] <- results[5:63,]
names(results) <- c("group", "n", "kurtosis", "p.val", "skew", "p.val")
# a little formatting for the paper
results2<-results
for(i in 1:nrow(results2)){
  if(as.numeric(results2[i,4])<.001) results2[i,4] <- "<.001"
  if(as.numeric(results2[i,6])<.001) results2[i,6] <- "<.001"
  
}
lepto <- sum(results[results[,3]>3,4] < .05)
platy <- sum(results[results[,3]<3,4] < .05)
meso <- sum(results[,4] > .05)

pos.skew <- sum(results[results[,5]>0,6] < .05)
neg.skew <- sum(results[results[,5]<0,6] < .05)
no.skew <- sum(results[,6] > .05)

foo <- results[results[,5]<0,]
foo[foo[,6]<.05,]
setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/figures/kurt.skew")
write.csv(results2, file="results.csv")
