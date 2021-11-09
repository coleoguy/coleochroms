setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/analyses/chrom.evol/results")
groups <- c('Cerambycidae', 'Chrysolina', 'Chrysomelidae', 'Timarcha', 
            'Tenebrionidae', 'Dendroctonus', 'Curculionidae','Scarabaeidae', 
            'Ips', 'Elateridae', 'Dytiscidae', 'Coccinellidae', 
            'Lampyridae', 'Pterostichus', 'Bembidion', 'Cicindela', 'Carabidae')

# OK the goal here is to collect all the data from the boot strap analysis
gain <- matrix(, 100, length(groups))
loss <- matrix(, 100, length(groups))
colnames(gain) <- groups
colnames(loss) <- groups
for(k in 1:length(groups)){ #length(groups)
  counter <- 1
  print(groups[k])
  for(k2 in 1:20){
    for(k3 in 1:5){ 
      print(paste(groups[k], ": ", counter))
      ## Read in the results of the chromevol program
      if(file.exists(paste(groups[k], ".", k2, ".", k3, "/chromEvol.res", sep=""))){
        data <- readLines(paste(groups[k], ".", k2, ".", k3, "/chromEvol.res", sep=""))[c(13,16,17)]
        ## now we will extract the parameter estimates and scale as appropriate
        ## (necessary because Itay's program scales tree to help in optimization
        ## so we have to multiply the rates to get them back into the tree units)  
        rate <- as.numeric(strsplit(data[1], 'by ')[[1]][2])
        gain[counter, k] <- as.numeric(strsplit(data[2], '\t')[[1]][2]) * rate
        loss[counter, k] <- as.numeric(strsplit(data[3], '\t')[[1]][2]) * rate
      }else{
        gain[counter, k] <- "file.missing"
        loss[counter, k] <- "file.missing"
      }
      counter <- counter + 1
    }
  }
}

## Lets look at these results
for(i in 1:100){
  gain[i,] <- as.numeric(gain[i,])
  loss[i,] <- as.numeric(loss[i,])
}

# now I need to get the quantiles on these estimates
quantile(as.numeric(gain[,1]), c(.025, .975),na.rm=T)[1]

sd.gain <- sd.loss <- vector()
sd.gain[1] <- "sd.gain"
sd.loss[1] <- "sd.loss"

for(i in 1:17){
  sd.gain[i+1] <- sd(as.numeric(gain[,i]),na.rm=T)
  sd.loss[i+1] <- sd(as.numeric(loss[,i]),na.rm=T)
}
MLE <- read.csv("../MLE2.csv", as.is=T)
parameters <- rbind(MLE, sd.gain, sd.loss)

rownames(parameters) <- c("gain", "loss", "sd.gain.p", "sd.loss.p", "sd.gain.t", "sd.loss.t")

write.csv(parameters, file="../final.params2.csv")


### This code creates a larger data set with all 17 groups
colMeans(as.numeric(gain), na.rm=T)
