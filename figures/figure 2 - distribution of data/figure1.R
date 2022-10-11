###Title: Heatmap of Coleoptera Chromosome Counts
###Author: Jamie Alfieri
###Date: 20 June 2022
###Description: This script produces a heatmap of coleoptera chromosome counts.
###There are some manual tip pruning steps, transformations, etc. that are
###specific to this data. Additionally, the figure produced by this plot is
###then edited in powerpoint to adjust the scale bar values.
###Necessary files:
###coleo_chrom.csv: a csv file containing chromosome counts. This was obtained
###from Heath.
###nameindex.csv: a csv file containing a matched list of old names and new names.
###this file is used to correct the names on the treefile and datafile.
###coleo_mesquiteedits.phy: a simple phylip file, edited by Jamie in mesquite.
###the edits were to increase the node depth of the Archostemata/Myxophaga node.
###the base tree was obtained from Heath.

##Install packages and attach libraries
library(ape)
library(phytools)
library(viridis)

#read in coleo chrom number data
coleo <- read.csv("coleo_chrom.csv", h=T, sep = ",")

#read in tree edited from mesquite
coleotree <- read.tree(file = "coleo_mesquiteedits.phy")
#we are using the first tree in coleotree multiphylo object because it has more
#Adephaga species
coleotree <- coleotree[[1]]

#read in name index data
nameindex <- read.csv("nameindex.csv", h=T, sep = ",")

#######
####Clean Data
#remove rows where Haploid.Autosome.Number is blank
coleo <- coleo[!is.na(coleo$Haploid.Autosome.Number), ]
#remove rows where Family is Stylopidae (Strepsiptera)
coleo <- coleo[!coleo$Family == "Stylopidae",]
#remove rows where Family is Myrmecolacidae (Strepsiptera)
coleo <- coleo[!coleo$Family == "Myrmecolacidae",]
#change names based on name index
coleo$Family <- as.character(coleo$Family)
nameindex$oldname <- as.character(nameindex$oldname)
nameindex$newname <- as.character(nameindex$newname)
i <- 0
for (i in 1:nrow(nameindex)){
  coleo$Family[coleo$Family == nameindex$oldname[i]] <- nameindex$newname[i]
}

###Remove Geotrupidae & Hydradephaga from tree
coleotree <- drop.tip(coleotree, c("Geotrupidae", "Hydradephaga"))
#######check data#######

#change family to factor
coleo$Family<-as.factor(coleo$Family)
#count how many levels there are (there should be 47)
coleofam<-levels(coleo$Family)

####generate count data for each of the 47 families####

outcoleo <- matrix(,nrow=47, ncol = 35)
rownames(outcoleo) <- coleofam
colnames(outcoleo) <- c(paste(as.character(1:35)))
i <- 0
for(i in 1:length(coleofam)){
  test<-coleo$Haploid.Autosome.Number[coleo$Family == coleofam[i]]
  k<-1
  for(k in 1:35){
    outcoleo[i,k]<-sum(test == k)
  }
}


#transform data to log +1 scaled
transformedoutput <- log1p(outcoleo)

####make plot####

#make a palette adding white to the viridis palette (0's are white (NA's of
#non transformed data), everything else is viridis)
new_palette <- c("white", viridis(n = 58, begin = 0, end = 1))

phylo.heatmap(coleotree, labels = TRUE, transformedoutput,
              fsize = c(0.6, 0.6, 1), colors = new_palette, grid = TRUE,
              split = c(0.4, 0.6), legend = TRUE)

