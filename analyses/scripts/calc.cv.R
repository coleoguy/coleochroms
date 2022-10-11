dat <-read.csv("../data/scv-autosome.counts.csv")
scvtab <- read.csv("cv.and.age.csv")
cvs <- c()
for(i in 1:ncol(dat)){
  cvs[i] <- sd(dat[,i], na.rm=T)/mean(dat[,i], na.rm=T)
}
names(cvs) <- colnames(dat)
cvs
for(i in 1:length(cvs)){
  hit <- which(scvtab$name == names(cvs)[i])
  print(i)
  scvtab$cv[hit] <- cvs[i]
}
write.csv(scvtab,file="cv.and.age.csv",row.names = F)
