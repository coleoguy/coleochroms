library(viridis)
library(coda)
library(ape)

load("../../../data.analysis/results/genera.wopoly.RData")

results.wopoly <- results[c(7, 10, 9, 6, 12, 3, 5,
                            1, 8, 4, 11, 2)]

load("../../../data.analysis/results/genera.wpoly.RData")

pruned.results <- list()
for(i in 1:12){
  result <- matrix(NA, 0, 5)
  colnames(result) <- c("tree", colnames(results[[1]][[1]])[-1])
  for(j in 1:100){
    # append all the last 100s
    foo <- results[[i]][[j]]
    foo$i <- j
    result <- rbind(result, foo[901:1000,])
  }
  pruned.results[[i]] <- result
}
names(pruned.results) <- names(results)

results.wpoly <- pruned.results[c(7, 10, 9, 3, 6, 12, 5,
                                  1, 4, 8, 11, 2)]

rm(foo,pruned.results,result,results, i, j)
nes <- c(1,2,2,3,3,3,3,1,1,1,2,3)

trees <- read.nexus("../../../data.analysis/data/coleo.trees.nexus")
tipstokeep <- c("POTEPI_Pimelia_elevata", "POCUSC_Ips_paraconfusus",
                "POCUSC_Dendroctonus_ponderosae","POCHGA_Diabrotica_balteata",
                "POCHCH_Timarcha_intricata", "POCHCH_Cyrtonus_contractus",
                "POCHCH_Chrysolina_marginata","ADCATR_Bembidion_punctulatum",
                "ADCAHA_Pterostichus_globosus", "ADCAHA_Harpalus_aesculanus",
                "ADCAHA_Calathus_asturiensis", "ADCACI_Cicindela_dorsalis")

cols <- viridis(3, begin=.3, end=.8,alpha =.2)[nes]


xlims <- c(.7,12.3)
plot(x=0,y=0,ylim=c(0,.25),
     xlim=xlims,col="white",
     ylab="rate",xlab="",xaxt="n")
axis(side=1,at=1:12,labels=names(pruned.results),las=2,cex.axis=.7)
for(i in 1:7){
  y<- rowSums(pruned.results[[i]][,2:3])
  x<-jitter(rep(i,10000),amount=.25)
  points(x=x, y=y,
         cex=.2,col=cols[i],pch=16)
  points(x=mean(x),y=mean(y),pch=16)
  inter <- HPDinterval(as.mcmc(y))
  lines(x=c(i,i), y=inter, lwd=2)
}
for(i in 8:12){
par(new = TRUE) 
# One run of calathus didn't mix well
  
y2<- rowSums(pruned.results[[i]][,2:3])
if(i == 12){
  y2 <- y2[-c(8501:8600)]
}
x2 <- jitter(rep(i,length(y2)),amount=.25)
plot(x=x2, 
     y=y2, ylim=c(0,2.4),
     cex=.2,xlim=xlims,
     col=cols[i],pch=16, axes = FALSE, 
     xlab = "", ylab = "")
points(x=mean(x2),y=mean(y2),pch=16)
inter <- HPDinterval(as.mcmc(y2))
lines(x=c(i,i), y=inter, lwd=2)
}

axis(side = 4, at = c(0,.5,1,1.5))
mtext("rate", side = 4, line = 3)    
abline(v=7.5, lty=2)
text(x=c(4,10),y=rep(2.4,2),c("Polyphaga", "Adephaga"))

