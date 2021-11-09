## This script will analyze the fit of a
## BM model to the evoltuion of chromosome number in
## each genera being studied using the Arbutrus approach. 
## Heath Blackmon
## 28 April 2015
## coleoguy@gmail.com
library(arbutus)
load("../results/genera.bm")
taxa   <- c('Bembidion', 'Calathus', 'Chrysolina', 
            'Cicindela', 'Cytronus', 'Dendroctonus', 
            'Diabrotica', 'Harpalus', 'Ips', 'Pimelia', 
            'Pterostichus', 'Timarcha')


arb.results <- list()
# now lets loop through and combine 
# all the arbutus results across trees
for(i in 1:12){
  cat(paste("\nworking on", taxa[i]))
  group.results <- list()
  for(j in 1:100){
    group.results[[j]] <- arbutus(results[[i]][[j]], nsim = 10)
    cat(".")
  }
  arb.results[[i]] <- group.results
}

# ok so now we extract the arbutus results from all trees in a group 
# and combine so we have a null and observed distribution



# look at geiger 

