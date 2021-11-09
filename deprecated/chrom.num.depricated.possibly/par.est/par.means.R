setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/figures/par.est")
library(geiger)
par.means <- as.data.frame(matrix(,1,4))
colnames(par.means) <- c("group", "sigma", "alpha", "z")
fams <- c("Bembidion", "Carabidae", "Chrysolina",
          "Chrysomelidae", "Cicindela", "Curculionidae",
          "Ips", "Scarabaeidae", "Timarcha")
counter <- 1
for(j in 1:9){
  fam <- fams[j]
  data <- read.csv(paste(fam, "/data.csv", sep=""))
  chrom <- data[,2]
  names(chrom) <- data[,1]
  for(i in 1:100){
    par.means[counter,1] <- fam
    tree <- read.tree(paste(fam, "/", fam, i, ".tree", sep=""))
    foo <- fitContinuous(tree, chrom, model="OU")$opt
    par.means[counter, 2:4] <- c(foo$sigsq, foo$alpha, foo$z0)
    counter <- counter + 1
    print(paste(j, "Done with:", fam, i))
    
  }
}
write.csv(par.means, file="par.means.csv")