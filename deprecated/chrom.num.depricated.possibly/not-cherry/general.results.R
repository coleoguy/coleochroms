

setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/data/fig1")
data <- read.csv("chrom.nums.csv", header=F, as.is=T)
data<- data[,c(1:3,5)]


fams <- unique(data[,1])
ade <- c("Dytiscidae", "Carabidae", "Gyrinidae", "Haliplidae", "Hygrobiidae", "Noteridae", "Trachypachidae")
pol <- fams[!fams %in% ade]
pol <- pol[c(1:4,6:37,40:52)]

ade.data <- data[data[,1] %in% ade,]
pol.data <- data[data[,1] %in% pol,]
rm(data,ade,fams,pol)


min(ade.data[,4])
max(ade.data[,4])
mean(ade.data[,4])

min(pol.data[,4])
max(pol.data[,4])
mean(pol.data[,4])
