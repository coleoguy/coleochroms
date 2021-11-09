setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/analyses/chrom.evol")
data <- read.csv("final.params3.csv", as.is=T)[,c(1,3:23)]
# lets make a table for our combined results
results <- data

#lets convert the sd in data.ph.u to CI values
results[3:4,2:22] <- data[3:4,2:22]/sqrt(20-1)*1.96
results[5:6,2:22] <- data[5:6,2:22]/sqrt(100-1)*1.96


foo <- results[1:2,1:22]
foo <- colMeans(foo[,2:22])
bar <- order(foo, decreasing=T)
bar <- bar+1

results <- results[,c(1,bar)]

results <- results[,c(1,3:22)]
nums <- ncol(results)

par(mar=c(4,6,1,1))
plot(0,0, col="white", xlim=c(0,.9), ylim=c(.5,{nums-.5}), yaxt="n",
     ylab="", xlab="", cex.axis=.6,main="",cex.lab=.8)
axis(side=2,at=1:{nums-1}+.15, labels=colnames(results)[2:nums], las=2, cex.axis=.6, tck=0)
axis(side=1,at=.35, line=1,labels="Transition Rate", las=1, cex.axis=.8, tck=0)
lo <- results[1,2:nums]-results[5,2:nums]
hi <- results[1,2:nums]+results[5,2:nums]
for(i in 1:{nums-1}){
  lines(y=c(i,i), x=c(lo[i],hi[i]), lwd=2, col="gray")
  lines(y=c(i-.15,i+.15), x=rep(lo[i], each=2), lwd=2, col="gray")
  lines(y=c(i-.15,i+.15), x=rep(hi[i], each=2), lwd=2, col="gray")
}
points(y=1:{nums-1}, x=results[1,2:nums], pch=0, cex=1)
lo <- results[2,2:nums]-results[6,2:nums]
hi <- results[2,2:nums]+results[6,2:nums]
for(i in 1:{nums-1}){
  j<-i+.3
  lines(y=c(j,j), x=c(lo[i],hi[i]), lwd = 2, col = "gray")
  lines(y=c(j-.15,j +.15), x=rep(lo[i], each = 2), lwd = 2, col = "gray")
  lines(y=c(j-.15,j +.15), x=rep(hi[i], each = 2), lwd = 2, col = "gray")
}
points(y=1:{nums-1}+.3, x=results[2,2:nums], pch=15, cex=1)

points(x=rep(.6,2), y=c(16,15.5),pch=c(15,0), cex=1)
text(x=rep(.6,2), y=c(16,15.5),labels=c("Gains", "Losses"), pos=4, cex=.7)
## lets figure out the right order based on mean

#output at 500x700