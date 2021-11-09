# lets look at burnin across datasets
posterior <- x[[1]][sample(251:1000,50), ]
for(i in 2:100){
  posterior <- rbind(posterior, x[[i]][sample(251:1000, 50), ])
}
library(coda)
effectiveSize(posterior)

plot(posterior$asc1~posterior$desc1)
plot(density(posterior$asc1),xlim=c(0,1))
plot(density(posterior$desc2))
plot(density(posterior$pol1))
plot(density(posterior$pol2))
plot(as.mcmc(posterior))

HPDinterval(as.mcmc(posterior$desc2))
plot(posterior$asc1~posterior$p,pch=16, cex=.2)
plot(posterior$asc2~posterior$p,pch=16, cex=.2)
plot(posterior$desc1~posterior$p,pch=16, cex=.2)
plot(posterior$pol1~posterior$p,pch=16, cex=.2)
plot(posterior$pol2~posterior$p,pch=16, cex=.2)
plot(posterior$tran12~posterior$p,pch=16, cex=.2)
plot(posterior$tran21~posterior$p,pch=16, cex=.2)

plot(log(posterior$asc2)~log(posterior$asc1), 
     ylim=c(-13,3), xlim=c(-13,3),
     ylab="Wingless ascending",
     xlab="Winged ascending",
     pch=16, cex=.2,
     xaxt="n",yaxt="n")
lines(x=c(-13,3),y=c(-13,3),col="red")
tick.spot <- c(log(.00001), log(.0001), log(.001), log(.01), log(.1),log(1), log(10))
axis(side=1, at=tick.spot,labels=c(.00001, .0001, .001, .01, .1,1, 10),cex.axis=.7)
axis(side=2, at=tick.spot,labels=c(.00001, .0001, .001, .01, .1,1, 10),cex.axis=.7)

plot(log(posterior$desc2)~log(posterior$desc1), 
     #ylim=c(-14,15), xlim=c(-14,15),
     ylab="Wingless ascending",
     xlab="Winged ascending",
     pch=16, cex=.2,
     xaxt="n",yaxt="n")
lines(x=c(-17,3),y=c(-13,3),col="red")
tick.spot <- c(log(.00001), log(.0001), log(.001), log(.01), log(.1),log(1), log(10))
axis(side=1, at=tick.spot,labels=c(.00001, .0001, .001, .01, .1,1, 10),cex.axis=.7)
axis(side=2, at=tick.spot,labels=c(.00001, .0001, .001, .01, .1,1, 10),cex.axis=.7)
