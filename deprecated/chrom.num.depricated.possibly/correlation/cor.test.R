setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/figures/correlation")
data <- read.csv("temp.csv", as.is=T)
cor.test(x=log(data[,2]), y=log(data[,3]),method="k", alternative="greater")
cor.test(x=log(data[,2]), y=log(data[,3]),method="k", alternative="greater")
cor.test(x=data[,2], y=data[,3],method="k"), alternative="greater")
cor.test(x=data[,2], y=data[,3],method="s"), alternative="greater")
data2<- data[c(1:17),]
cor.test(x=data2[,2], y=data2[,3],method="p")
cor.test(x=data2[,2], y=data2[,3],method="k")
cor.test(x=data2[,2], y=data2[,3],method="s")
data2<- data[c(1,3:18),]
cor.test(x=data2[,2], y=data2[,3],method="p")
cor.test(x=data2[,2], y=data2[,3],method="k")
cor.test(x=data2[,2], y=data2[,3],method="s")
