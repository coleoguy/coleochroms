substrRight <- function(x, n){
  sapply(x, function(xx)
    substr(xx, (nchar(xx)-n+1), nchar(xx))
  )
}

setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/analyses")

groups <- c('Cerambycidae', 'Chrysolina', 'Chrysomelidae', 'Timarcha', 'Tenebrionidae', 
'Scarabaeidae', 'Ips', 'Elateridae', 'Dytiscidae', 'Diabrotica', 'Dendroctonus', 
'Curculionidae', 'Coccinellidae', 'Lampyridae', 'Pterostichus', 'Bembidion', 
'Cicindela', 'Carabidae')
data<- matrix(,18,6)
row.names(data) <- groups
colnames(data) <- c("Norm.CV", "Gain", "Loss", "Dupl", "Scale", "lnL")

foo <- read.csv('correlations/cv.and.age.csv', as.is = T)
foo <- foo[foo[,1] %in% row.names(data),1:3]
for(i in 1:18){
  if(rownames(data)[i] %in% foo[,1]){
    bar <- which(foo[,1] == rownames(data)[i])
    data[i,1] <- foo[bar, 2] / (foo[bar, 3] / 100)
  }
}
for(j in 1:6){
  for(i in 1:18){
    temp.res <- readLines(paste("chrom.evol/",
                                rownames(data)[i],
                                "/result", j, "/chromEvol.res", 
                                sep = ""))[c(13,16,17,18,20)]
    data[i,5] <- substrRight(temp.res[1], 10)
    data[i,3] <- substrRight(temp.res[2], 7)
    data[i,2] <- substrRight(temp.res[3], 7)
    data[i,4] <- substrRight(temp.res[4], 8)
    data[i,6] <- substrRight(temp.res[5], 8)
  }
  write.csv(data, file=paste("results", j, ".csv", sep = ""))
}
temp.data1 <- read.csv("results1.csv", as.is=T)
temp.data2 <- read.csv("results2.csv", as.is=T)
temp.data3 <- read.csv("results3.csv", as.is=T)
temp.data4 <- read.csv("results4.csv", as.is=T)
temp.data5 <- read.csv("results5.csv", as.is=T)
temp.data6 <- read.csv("results6.csv", as.is=T)

temp2 <- rbind(temp.data1, 
               temp.data2, 
               temp.data3, 
               temp.data4, 
               temp.data5, 
               temp.data6)
write.csv(temp2, file='sum.table.csv')

data2<- read.csv("sum.table.csv")
data3<- matrix(,18,18)
row.names(data3) <- row.names(data)
colnames(data3) <- c(rep("gains", 6), rep("loss", 6), rep("dupl", 6))
for(i in 1:18){
  data3[i,1:6] <- data2[data2[,1] == row.names(data3)[i],2]
  data3[i,7:12] <- data2[data2[,1] == row.names(data3)[i],3]
  data3[i,13:18] <- data2[data2[,1] == row.names(data3)[i],4]
}
plot(data3[,1])
for(i in 2:6){
  points(data3[,i])
  
}
test <- t(data3[,1:6])
gains <- as.vector(data3[,1:6])

counter <- 1
foobar <-matrix(,108,2)
for(i in 1:18){
  foobar[counter:{counter+5},1] <- test[1:6, i]
  foobar[counter:{counter+5},2] <- rep(colnames(test)[i], 6)
  counter <- counter + 6
}

foobish <- vector()
number.order <- colnames(test)[c(18,2:4,12,1,17,13,9,5:8,10:11,14:16)]
number.info <- read.csv("groups.csv", as.is=T)
for(i in 1:18){
  foobish[i] <- sum(number.info[,number.order[i]] != "")
}
names(foobish) <- number.order

  

par(mar=c(10,4.1,4.1,2.1))
boxplot(test[,c(18,2:4,12,1,17,13,9,5:8,10:11,14:16)]/2, use.cols=T, las=2,
        main="Rate of Chromosome Number Evolution (MK model)",
        ylab="Gains Per MY", ylim=c(0,.26), xlim = c(-1,18))
for(i in 1:18){
  text(i,.25,foobish[i], cex=.8)
}
text(0,.25, "N =", cex=.8)
    