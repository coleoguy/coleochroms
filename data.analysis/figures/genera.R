dat <- read.csv("../data/genera.chromeplus.csv", row.names = 1)
genera <- unique(dat$Genus)
dat$suborder[dat$Genus %in% genera[c(3,5,6,7,9,10,12)]] <- "Polyphaga"
dat$suborder[dat$Genus %in% genera[c(1,2,4,8,11)]] <- "Adephaga"
library(ggraptR)
ggraptR(dat)

boxplot(dat$rate[dat$suborder=="Polyphaga"]~dat$genus[dat$suborder=="Polyphaga"])
for(i in 1:12){
  x <- mean(dat$rate[dat$genus==genera[i] & dat$type=="wgd"])
  print(paste(genera[i],x))
}

