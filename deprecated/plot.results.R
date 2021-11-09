load("~/Desktop/beetles.w.poly.RData")
plot(results[[1]]$p, ylim=c(-450,-350), type="l", pch=16, cex=.5)
for(i in 2:100) points(results[[i]]$p, type="l", pch=16, cex=.5, col=rainbow(100)[i])
post.dist <- results[[1]][200:250,]
for(i in 2:100) post.dist <- rbind(post.dist, results[[i]][200:250,])

# state 2 is wingless
z <- density(post.dist$pol2-post.dist$pol1)
plot(x=z$x, y=z$y/25,xlim=c(0,2),ylim=c(0,3),type="l",col="darkgreen",
     lwd=2, ylab="density", xlab="rate parameter")
lines(density(post.dist$desc2-post.dist$desc1),xlim=c(0,2),col="red",lwd=2)
lines(density(post.dist$asc2-post.dist$asc1), col="blue",lwd=2)
points(x=rep(1.5,3), y=c(2.5,2.3,2.1), pch=15, col=c("darkgreen","blue","red"))
text(x=rep(1.5,3), y=c(2.5,2.3,2.1), labels=c("polyploidy","gain","loss"), pos=4)
library(coda)
apd<-HPDinterval(as.mcmc(post.dist$asc2-post.dist$asc1), prob=.90)
dpd<-HPDinterval(as.mcmc(post.dist$desc2-post.dist$desc1), prob=.90)
ppd<-HPDinterval(as.mcmc(post.dist$pol2-post.dist$pol1), prob=.90)





plot(density(post.dist$tran21-post.dist$tran12),ylim=c(0,90))



WhiskerLine <- function(x1, x2, y, col){
  # horizontal
  lines(x=c(x[1],x[2])y=rep)
}

