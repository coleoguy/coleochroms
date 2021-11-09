setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/analyses/chrom.evol")
data <- read.csv("final.params3.csv", as.is=T)[,c(1,3:23)]
# lets make a table for our combined results
results <- data

#lets convert the sd in data.ph.u to CI values
results[3:4,2:22] <- data[3:4,2:22]/sqrt(20-1)*1.96
results[5:6,2:22] <- data[5:6,2:22]/sqrt(100-1)*1.96


foo <- results[1:2,1:21]
foo <- colMeans(foo[,2:21])
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
text(x=rep(.6,2), y=c(16,15.5),labels=c("Increase", "Decrease"), pos=4, cex=.7)
## lets figure out the right order based on mean

#output at 500x700









## Lets see a plot with just Polyphaga
data <- read.csv("final.params3.csv", as.is=T)[,c(1,3:23)]
results <- data
results[3:4,2:22] <- data[3:4,2:22]/sqrt(20-1)*1.96
results[5:6,2:22] <- data[5:6,2:22]/sqrt(100-1)*1.96
pol.cols <- c(2:11,13,14,20)
pol.results <- results[,c(1, pol.cols)]
results <- pol.results
nums <- ncol(results)
foo <- results[1:2,1:nums]
foo <- colMeans(foo[,2:nums])
bar <- order(foo, decreasing=T)
bar <- bar+1
results <- results[,c(1,bar)]
par(mar=c(4,6,1,1))
plot(0,0, col="white", xlim=c(0,{max(foo)+.1}), ylim=c(.5,{nums-.5}), yaxt="n",
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
points(x=rep(.5,2), y=c(nums-1, nums-1.5),pch=c(15,0), cex=1)
text(x=rep(.5,2), y=c(nums-1, nums-1.5),labels=c("Increase", "Decrease"), pos=4, cex=.7)


## save at 550x550

### plotting of ade resutls

data <- read.csv("final.params3.csv", as.is=T)[,c(1,3:23)]
results <- data
results[3:4,2:22] <- data[3:4,2:22]/sqrt(20-1)*1.96
results[5:6,2:22] <- data[5:6,2:22]/sqrt(100-1)*1.96
ade.cols <- c(12,15,16,17,18,19,21)
ade.results <- results[,c(1, ade.cols)]
results <- ade.results
nums <- ncol(results)
foo <- results[1:2,1:nums]
foo <- colMeans(foo[,2:nums])
bar <- order(foo, decreasing=T)
bar <- bar+1
results <- results[,c(1,bar)]
par(mar=c(4,6,2,1))
plot(0,0, col="white", xlim=c(0,{max(foo[1:6])+.4}), ylim=c(.5,{nums-.5}), yaxt="n",
     ylab="", xlab="", cex.axis=.6,main="",cex.lab=.6, xaxt="n")

mtext(side=3,at=c(0,.2,.4,.6,.8,1), line=.3,text=c(0,.2,.4,.6,.8,1), cex=.8)
axis(side=3, las=0, cex.axis=.6, tck=-.02, labels=F)



axis(side=2,at=1:{nums-1}+.15, labels=colnames(results)[2:nums], las=2, cex.axis=.6, tck=0)
axis(side=1,at=.55, line=1,labels="Transition Rate", las=1, cex.axis=.8, tck=0)
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
points(x=rep(.8,2), y=c(nums-1, nums-1.5),pch=c(15,0), cex=1)
text(x=rep(.8,2), y=c(nums-1, nums-1.5),labels=c("Increase", "Decrease"), pos=4, cex=.7)
par(new=T)
plot(x=results[1:2, 2], y=c(.5,.8), xlim=c(0,7.7),ylim=c(0,8), pch=c(0,15), yaxt="n",
     xaxt="n", xlab="", ylab="")
lo <- results[1,2:nums]-results[5,2:nums]
hi <- results[1,2:nums]+results[5,2:nums]
lines(y=c(.5,.5), x=c(lo[1],hi[1]), lwd = 2, col = "gray")
lines(y=c(.5-.15,.5 +.15), x=rep(lo[1], each = 2), lwd = 2, col = "gray")
lines(y=c(.5-.15,.5 +.15), x=rep(hi[1], each = 2), lwd = 2, col = "gray")
lo <- results[2,2:nums]-results[6,2:nums]
hi <- results[2,2:nums]+results[6,2:nums]
lines(y=c(.8,.8), x=c(lo[1],hi[1]), lwd = 2, col = "gray")
lines(y=c(.8-.15,.8 +.15), x=rep(lo[1], each = 2), lwd = 2, col = "gray")
lines(y=c(.8-.15,.8 +.15), x=rep(hi[1], each = 2), lwd = 2, col = "gray")
points(x=results[1:2, 2], y=c(.5,.8), pch=c(0,15))
abline(h=1.25, lty=2, col="gray")
mtext(side=1,at=c(0,2,4,6,8), line=.3,text=c(0,2,4,6,8), cex=.8)
axis(side=1, las=0, cex.axis=.6, tck=-.02, labels=F)