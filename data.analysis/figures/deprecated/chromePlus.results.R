load("../results/run with.04.RData")

library(viridis)
for(i in 1:100) print(nrow(x[[1]]))


for(i in 1:100){
  plot(x[[i]]$p,type="l",
       col=viridis(100)[1])
  readline(prompt="Press [enter] to continue")
  
}

# looks like a run length of 500 is conservative
# and gets us into the posterior on these
# lets extract posterior and rescale to 
# MY units
posterior <- x[[1]][500:1000,]/scaler[1]
for(i in 2:100){
  posterior <- rbind(x[[i]][250:1000,]/scaler[i])
}

dif.asc <- posterior$asc2-posterior$asc1
dif.dsc <- posterior$desc2-posterior$desc1
dif.pol <- posterior$pol2-posterior$pol1

pdf("rates.pdf", width=4, height=4)
plot(density(dif.asc),xlim=c(-.3,.8), ylim=c(0,6),
     main="",xlab="Rate difference (wingless-winged)")
lines(density(dif.dsc))
lines(density(dif.pol,bw=.07))
polygon(density(dif.asc), col=rgb(1,0,0,.5))
polygon(density(dif.dsc), col=rgb(0,0,1,.5))
polygon(density(dif.pol,bw=.07), col=rgb(.2,.6,.2,.5))
cols <-c(rgb(1,0,0,.5), 
         rgb(0,0,1,.5), 
         rgb(.2,.6,.2,.5))
points(x=rep(.55,3),y=c(6,5.5,5),pch=22, bg=cols)
text(x=rep(.55,3),y=c(6,5.5,5),
     c("ascending","descending","polyploidy"),
     pos=4,cex=.6)
dev.off()

