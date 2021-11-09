library(plotrix)
setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/not-cherry/genera.analysis")
results <- read.csv("genera.sigsq.csv", header=T, as.is=T, row.names=1)
colnames(results) <- c('Bembidion', 'Calathus', 'Chrysolina', 
                       'Cicindela', 'Cytronus', 'Dendroctonus', 
                       'Diabrotica', 'Harpalus', 'Ips', 'Pimelia', 
                       'Pterostichus', 'Timarcha')
results.lb <- read.csv("genera.sigsq.lb.csv", header=T, as.is=T, row.names=1)
colnames(results.lb) <- c('Bembidion', 'Calathus', 'Chrysolina', 
                          'Cicindela', 'Cytronus', 'Dendroctonus', 
                          'Diabrotica', 'Harpalus', 'Ips', 'Pimelia', 
                          'Pterostichus', 'Timarcha')
results.ub <- read.csv("genera.sigsq.ub.csv", header=T, as.is=T, row.names=1)
colnames(results.lb) <- c('Bembidion', 'Calathus', 'Chrysolina', 
                          'Cicindela', 'Cytronus', 'Dendroctonus', 
                          'Diabrotica', 'Harpalus', 'Ips', 'Pimelia', 
                          'Pterostichus', 'Timarcha')
gen.means <- colMeans(results)
gen.highs <- colMeans(results.ub)
gen.lows <- colMeans(results.lb)
gen.order <- c(7,9,10,6,12,3,5,1,4,8,11,2)
par(mar=c(10,5,5,10))
cols <- c("red","#7a0177","blue")
cats <- c(3,2,2,1,1,1,1,3,3,3,2,1)
plot(x=1:12, y=gen.means[gen.order], ylim=c(0,3), xaxt='n',xlab='', 
     col=cols[cats], pch=19, xlim=c(1,12), ylab="Rate of Karyotype Evolution") 
for(i in 1:11){
  #vertical lines
  lines(x=rep(i, 2), y=c(gen.highs[gen.order][i], gen.lows[gen.order][i]))
  #high caps
  lines(x=c(i-.1, i+.1), y=c(rep(gen.highs[gen.order][i], 2)))
  #low caps
  lines(x=c(i-.1, i+.1), y=c(rep(gen.lows[gen.order][i], 2)))
}
axis.break(axis=1,breakpos=11.5,style="slash")
axis.break(axis=3,breakpos=11.5,style="slash")
par(new=TRUE)
plot(x=12, y=gen.means[2], xlim=c(1,12), ylim=c(0,50), col="red",
     xaxt="n",yaxt="n",xlab="",ylab="", pch=19)
axis(4)

axis(side=1, lwd=.1, at=1:12, labels=names(gen.means[gen.order]), las=2, cex.axis=.8, tck=0)

i <- 12
#vertical lines
lines(x=rep(i, 2), y=c(gen.highs[gen.order][i], gen.lows[gen.order][i]))
#high caps
lines(x=c(i-.1, i+.1), y=c(rep(gen.highs[gen.order][i], 2)))
#low caps
lines(x=c(i-.1, i+.1), y=c(rep(gen.lows[gen.order][i], 2)))

lines(x=c(7.5,7.5), y=c(0,50), lty=2)
text(x=4, y=48, labels="Polyphaga", cex=.8)
text(x=10, y=48, labels="Adephaga", cex=.8)
par(xpd=TRUE)
points(x=15,y=50,pch=19, col="blue")
points(x=15,y=47,pch=19, col="#7a0177")
points(x=15,y=44,pch=19, col="red")
text(x=15, y=50, labels=expression(High~N[e]),pos=4, cex=.8)
text(x=15, y=47, labels=expression(Medium~N[e]),pos=4, cex=.8)
text(x=15, y=44, labels=expression(Low~N[e]),pos=4, cex=.8)
## export eps width=575