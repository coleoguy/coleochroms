setwd("~/Desktop/Dropbox/papers/fragileY/analyses.and.data/misc/trees")
library(ape)
trees <- read.nexus("full.trees.nexus")

setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/not-cherry/genera.analysis")
taxa <- c("ADCACI_Cicindela_dorsalis", "ADCAHA_Calathus_abaxoides" ,
          "ADCAHA_Harpalus_anxius", "ADCAHA_Pterostichus_anthracinus", 
          "ADCATR_Bembidion_decorum", "POCHCH_Chrysolina_aurichalcea",
          "POCHCH_Cyrtonus_cupreovirens", "POCUSC_Dendroctonus_pseudotsugae",
          "POCHGA_Diabrotica_porracea", "POTEPI_Pimelia_integra", 
          "POCUSC_Ips_montanus", "POCHCH_Timarcha_pimelioides")
alltaxa <- trees[[1]]$tip.label
droptaxa <- alltaxa[!alltaxa %in% taxa]

pruned.trees <- list()
for(i in 1:100){
  pruned.trees[[i]] <- drop.tip(trees[[i]], tip=droptaxa) 
}
class(pruned.trees) <- "multiPhylo"
rm(alltaxa,droptaxa,i)

ne.class <- c(1,3,1,2,1,3,3,3,1,2,2,3)
names(ne.class) <- taxa
karyo.rate <- c(2.94E-01, 2.68E+00, 3.55E-01, 6.02E-02,
                1.48E-01, 3.56E-01, 8.90E-01, 2.25E-01,
                5.82E-03, 1.57E-03, 7.47E-02, 2.76E-01)
names(karyo.rate) <- taxa
data <- cbind(taxa,ne.class,karyo.rate)

library(phytools)
phylANOVA(tree=pruned.trees[[1]], x=ne.class, y=karyo.rate, nsim=1000, posthoc=F, p.adj="holm")









# just using polyphaga
setwd("~/Desktop/Dropbox/papers/fragileY/analyses.and.data/misc/trees")
library(ape)
trees <- read.nexus("full.trees.nexus")

setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/not-cherry/genera.analysis")
taxa <- c("POCHCH_Chrysolina_aurichalcea",
          "POCHCH_Cyrtonus_cupreovirens", "POCUSC_Dendroctonus_pseudotsugae",
          "POCHGA_Diabrotica_porracea", "POTEPI_Pimelia_integra", 
          "POCUSC_Ips_montanus", "POCHCH_Timarcha_pimelioides")
alltaxa <- trees[[1]]$tip.label
droptaxa <- alltaxa[!alltaxa %in% taxa]

pruned.trees <- list()
for(i in 1:100){
  pruned.trees[[i]] <- drop.tip(trees[[i]], tip=droptaxa) 
}
class(pruned.trees) <- "multiPhylo"
rm(alltaxa,droptaxa,i)

ne.class <- c("fast","fast","fast","slow","med","med","fast")
names(ne.class) <- taxa
karyo.rate <- c(3.56E-01, 8.90E-01, 2.25E-01,
                5.82E-03, 1.57E-03, 7.47E-02, 2.76E-01)
names(karyo.rate) <- taxa
library(phytools)
phylANOVA(tree=pruned.trees[[1]], x=ne.class, y=karyo.rate, nsim=100, posthoc=T)


library(geiger)
d1 <- karyo.rate
grp <- as.factor(ne.class)
names(grp) <- names(d1) <- taxa
aov.phylo(d1~grp, pruned.trees[[1]], nsim=50)
