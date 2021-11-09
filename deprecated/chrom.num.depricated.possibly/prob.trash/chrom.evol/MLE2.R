setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/analyses/chrom.evol")
data<- matrix(, 4, 17)
groups <- c('Cerambycidae', 'Chrysolina', 'Chrysomelidae', 'Timarcha', 
            'Tenebrionidae', 'Dendroctonus', 'Curculionidae','Scarabaeidae', 
            'Ips', 'Elateridae', 'Dytiscidae', 'Coccinellidae', 
            'Lampyridae', 'Pterostichus', 'Bembidion', 'Cicindela', 'Carabidae')
colnames(data) <- groups
rownames(data) <- c("gain", "loss", "sd.gain", "sd.loss")

for(i in 1:length(groups)){
  gain <- loss <- vector()
  for(j in 1:20){
    data.temp <- readLines(paste(groups[i],
                                 "/result", j, "/chromEvol.res", 
                                 sep = ""))[c(13,16,17)]
    rate <- as.numeric(strsplit(data.temp[1], 'by ')[[1]][2])
    gain[j] <- as.numeric(strsplit(data.temp[2], '\t')[[1]][2]) * rate
    loss[j] <- as.numeric(strsplit(data.temp[3], '\t')[[1]][2]) * rate
  }
  data[1,i] <- mean(gain, na.rm=T)
  data[2,i] <- mean(loss, na.rm=T)
  data[3,i] <- sd(gain, na.rm=T)
  data[4,i] <- sd(loss, na.rm=T)  
}
write.csv(data, file="MLE2.csv")