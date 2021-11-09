setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/analyses/chrom.evol")
data <- read.csv("final.params.csv", as.is=T)[,c(1,3:11)]
# lets make a table for our combined results
results <- data

#lets convert the sd in data.ph.u to CI values
results[3:4,2:10] <- data[3:4,2:10]/sqrt(20-1)*1.96
results[5:6,2:10] <- data[5:6,2:10]/sqrt(100-1)*1.96

results <- results[,c(1,2,10,4,9,5,7,3,8,6)]

par(mar=c(4,6,1,1))
plot(0,0, col="white", xlim=c(0,.7), ylim=c(.5,9.5), yaxt="n",
     ylab="", xlab="Transition Rate", cex.axis=.6,main="",cex.lab=.8)
axis(side=2,at=1:9+.15, labels=colnames(results)[2:10], las=2, cex.axis=.6, tck=0)
lo <- results[1,2:10]-results[5,2:10]
hi <- results[1,2:10]+results[5,2:10]
for(i in 1:9){
  lines(y=c(i,i), x=c(lo[i],hi[i]), lwd=2, col="gray")
  lines(y=c(i-.1,i+.1), x=rep(lo[i], each=2), lwd=2, col="gray")
  lines(y=c(i-.1,i+.1), x=rep(hi[i], each=2), lwd=2, col="gray")
}
points(y=1:9, x=results[1,2:10], pch=0, cex=.7)
lo <- results[2,2:10]-results[6,2:10]
hi <- results[2,2:10]+results[6,2:10]
for(i in 1:9){
  j<-i+.3
  lines(y=c(j,j), x=c(lo[i],hi[i]), lwd = 2, col = "gray")
  lines(y=c(j-.1,j +.1), x=rep(lo[i], each = 2), lwd = 2, col = "gray")
  lines(y=c(j-.1,j +.1), x=rep(hi[i], each = 2), lwd = 2, col = "gray")
}
points(y=1:9+.3, x=results[2,2:10], pch=15, cex=.7)

points(x=rep(.6,2), y=c(9,8.5),pch=c(15,0), cex=.7)
text(x=rep(.6,2), y=c(9,8.5),labels=c("Gains", "Losses"), pos=4, cex=.7)
