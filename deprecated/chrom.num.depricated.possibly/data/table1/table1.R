setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis")
data <- read.csv('beetle.data.csv', as.is = T)
big.gens <- c('Bembidion', 'Cicindela', 'Carabus', 'Chrysolina',
              'Pterostichus', 'Otiorhynchus', 'Pimelia', 'Onthophagus',
              'Aphodius', 'Timarcha', 'Calathus', 'Diabrotica',
              'Ips', 'Cassida', 'Chlaenius', 'Cryptocephalus',
              'Harpalus', 'Pissodes', 'Colymbetes', 'Alagoasa',
              'Altica', 'Curculio', 'Longitarsus', 'Zabrus',
              'Agonum', 'Chilocorus', 'Myllocerus', 'Epilachna',
              'Hylobius', 'Phyllobius', 'Poecilus', 'Amara', 'Lema',
              'Nebrioporus', 'Brachinus',
              'Ctenicera', 'Passalus', 'Aphidecta', 'Dichotomius',
              'Leptinotarsa', 'Asphaera', 'Exochomus', 'Notonomus',
              'Omophoita', 'Calligrapha', 'Cyrtonus', 'Dendroctonus',
              'Hyperaspis', 'Themognatha', 'Anomala', 'Anthonomus',
              'Blaps', 'Platynus', 'Hyperaspis', 'Dendroctonus')
big.fams <- c('Carabidae', 'Chrysomelidae', 'Curculionidae', 
              'Scarabaeidae', 'Tenebrionidae', 'Coccinellidae', 
              'Cerambycidae', 'Dytiscidae', 'Scolytidae', 'Buprestidae',
              'Elateridae', 'Passalidae')


family.results <- matrix(,length(big.fams), 5)
colnames(family.results) <- c('group', 'N', 'mean', 'sd', 'cv')
for(i in 1:length(big.fams)){
  foo <- data[data[,1] == big.fams[i],6]
  family.results[i, 1] <- big.fams[i]
  family.results[i, 4] <- sd(foo)
  family.results[i, 3] <- mean(foo)
  family.results[i, 5] <- sd(foo)/mean(foo)
  family.results[i, 2] <- length(foo)
}


genus.results <- matrix(,length(big.gens), 5)
colnames(genus.results) <- c('group', 'N', 'mean', 'sd', 'cv')
for(i in 1:length(big.gens)){
  foo <- data[data[,2] == big.gens[i],6]
  genus.results[i, 1] <- big.gens[i]
  genus.results[i, 4] <- sd(foo)
  genus.results[i, 3] <- mean(foo)
  genus.results[i, 5] <- sd(foo)/mean(foo)
  genus.results[i, 2] <- length(foo)
}

results <- rbind(family.results, genus.results)

