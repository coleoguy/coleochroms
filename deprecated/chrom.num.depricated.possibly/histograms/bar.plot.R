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
ade.foo <- as.data.frame(table(ade.data[,4]))
pol.foo <- as.data.frame(table(pol.data[,4]))
#first switch levels to numerics
ade.foo[,1] <- as.numeric(levels(ade.foo[,1]))
pol.foo[,1] <- as.numeric(levels(pol.foo[,1]))


finals <- matrix(,34,3)
finals[,1] <- 1:34
for(i in 1:34){
  if(i %in% ade.foo[,1]) finals[i,2] <- ade.foo[ade.foo[,1] == i,2]
  if(i %in% pol.foo[,1]) finals[i,3] <- pol.foo[pol.foo[,1] == i,2]
}

colnames(finals) <- c("haploid.num", "ade", "pol")
foo <- rbind(finals[,2], finals[,3])
foo[2,3] <- 4
foo[1,31] <- 3
foo[2,31] <- 2
foo <- foo[,3:31]
# lets get rid of any NA values
foo[1, which(is.na(foo[1,]))] <- 0
foo[2, which(is.na(foo[2,]))] <- 0
finals[3,1] <- "1-3"
finals[31,1] <- "31+"
finals <- finals[3:31,1]
par(mar=c(1,3,1,1))
foo2 <- t(foo)
spots <- seq(from=2, to=86, length.out=29)
barplot(foo, ylim=c(0,1000), cex.axis=.6, cex.names=.7, border="black", 
        las=2, col=c("black", "white"), ylab="", beside=T,
         cex.lab=.7)
mtext(text="Chromosome Number", side=1, line=1, cex=.7)
mtext(text="Count", side=2,line=2.3,cex=.7)
axis(1, at=spots, labels=finals, tick = F, cex.axis=.45, line=-1)
points(x=rep(75,2), y=c(900,850),pch=c(0,15), cex=.9)
text(x=rep(75,2), y=c(900,850),labels=c("Polyphaga", "Adephaga"), pos=4, cex=.7)
