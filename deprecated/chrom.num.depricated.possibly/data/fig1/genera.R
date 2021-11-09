setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/data/fig1")
data <- read.csv("chrom.nums.csv", header=F, as.is=T)
data5 <- read.csv("chrom.nums.csv", header=F, as.is=T)
as.data.frame(table(data5[,5]))->cindy

genera <- unique(data[,2])
new.data <- matrix(,1259,3)
new.data[,2] <- genera
for(i in 1:1259){
  new.data[i,1] <- data[which(data[,2] == new.data[i,2])[1],1]
}
for(i in 1:1259){
  new.data[i,3] <- mean(data[which(data[,2] == new.data[i,2]),5], na.rm = T)
}
write.csv(new.data, file="genera.csv")
## 30 minutes later after some work in excel lets read it back in
data <- read.csv("genera.csv", as.is=T)
ade <- data[data[, 1] == "Adephaga", 4]
poly <- data[data[, 1] == "Polyphaga", 4]

hist(ade)
hist(poly)
ade2 <- as.data.frame(table(round(ade)))
poly2 <- as.data.frame(table(round(poly)))
barplot(c(table(ade),table(poly)), beside=F)

## 30 minutes later

data <- read.csv("genera2.csv")[,1:3]
data2 <- t(data)
barplot(as.matrix(data2[2:3,]), beside=F, names.arg=data2[1,], 
        col=c("lightblue", "red"), cex.names=.6)
points(48, 230, pch=15, col='red', cex=1.6)
points(48, 250, pch=15, col='lightblue', cex=1.6)
text(48, 230, "Adephaga", pos=4)
text(48, 250, "Polyphga", pos=4)

sum(data2[2,])
sum(data2[3,])
