# First lets get the mean rates estimated in each of our genera
load("../results/genera.wpoly.RData")

# get a list of the genera
taxa <- names(results)

# Now lets get the cvs and ages of genera
scvar <- read.csv("../data/cv.and.age.csv")

# lets find the overlap
hits <- taxa %in% scvar$name
taxa <- taxa[hits]

#mean rate for genera calculated using phylogenies
mr <- c()
for(i in 1:length(taxa)){
  curgenus <- results[names(results) == taxa[i]][[1]]
  rates <- curgenus[[1]][901:1000, 2:4]
  for(j in 2:100){
    print(paste(taxa[i], "tree", j))  
    rates <- rbind(rates, curgenus[[j]][901:1000, 2:4])
  }
  mr[i] <- mean(colMeans(rates))
}
names(mr) <- taxa

# now lets getting matching scaled cvs using all taxa and fossil dates
nmr <- c()
for(i in 1:length(taxa)){
  hit <- which(scvar$name == taxa[i])
  print(paste(scvar$name[hit], 
              round(scvar$cv[hit],3),
              round((scvar$cv[hit]/scvar$age[hit])*100, 3),
              round(scvar$age[hit],3)))
  nmr[i] <- scvar$cv[hit]/scvar$age[hit]
}
names(nmr) <- taxa
plot(nmr~mr)

#result
cor.test(mr,nmr, method="kendall")

# remove data points that are far outside all others
cor.test(mr[-2:-3],nmr[-2:-3], method="kendall")

sort(nmr)[9]/sort(nmr)[8]
sort(mr)[9]/sort(mr)[8]
sort(nmr)
sort(mr)

tableS1 <- scvar[scvar$name %in% taxa,]
write.csv(tableS1, "../results/scaled.variance.csv")
