library(ggplot2)
library(ape)
library(grid)

Even <- function(x){
  if(x/2 == round(x/2)){
    return(T)
  }else{
    return(F)
  }
}

# This script is for making a plot to go with the tree of chromosome numbers

## This script is used to produce the pieces of figure 1
## First we get the family level tree this is just a cladogram
## It has been updated to include more subdivisions of Adephaga
## than the version that was submitted to PNAS.
setwd("~/Desktop/Dropbox/papers/published/fragileY/analyses.and.data/fig1")
tree <- read.tree('new2.phy')[[1]]
tree$edge.length <-rep(1,96)
tree <- chronoMPL(tree)
## Just realized that I have a tip for which there is no data lets drop it
tree <- ape:::drop.tip(tree, 'Hydradephaga')

## here it is just plain
plot(tree, edge.width=5, cex=.8, show.tip.label=F)

## I am going to use the numbers from the most recent version of the database
## subtracting only the data that Ben Normark gave us on parthenogens since 
## I haven't discussed its use in this paper
data <- read.csv('all.inverts.csv', as.is=T)
data <- subset(data, data$Order == 'Coleoptera',)
data <- subset(data, data$entry.name == 'Blackmon',c(3,4,5,7,9,11,12,13,15))
data <- data[,c(1:3,5)]
data <- data[!is.na(data[,4]),]
data <- data[,c(1,4)]
colnames(data) <- c("family", "chroms")

## at this point we have a dataframe that contains the basic data that we want

foo <- as.data.frame(unique(data$family))
bar <- tree$tip.label
baq <- foo[1:nrow(foo),1] %in% bar
foo <- cbind(foo, baq)
colnames(foo) <- c('family', 'present')

## now i have the table foo that tells me which families are on the tree

foo <- subset(foo, foo[,2] == F,1:2)

## now we subset this down to just the groups that we need to find an 
## assignment for

## Lets create a replacement table to fix the data for parsing

colnames(foo)[2] <- 'h.level'
foo[c(1:3, 5, 10), 2] <- 'Curculionoidea'
foo[c(4, 7), 2] <- 'Archostemata'
foo[14,2] <- 'Myxophaga'
foo[9,2] <- 'Scarabaeidae'
foo[c(6,8,11:13,15),2] <- 'Tenebrionoidea'

## Now lets go through and do the replacement process.

for(i in 1:nrow(data)){
  if(data[i, 'family'] %in% foo[,1])
    data[i, 'family'] <- foo[which(data[i, 'family'] == foo[,1]),2]
}
## Lets cleanup a little bit
rm(baq); rm(bar); rm(foo); rm(i)

## so now rather than having counts for each state we will calculate the percent
## in each staet and create new scaled counts so that we can produce a 
## graph that is a heat map based on percentage of taxa in each chromosome
## count level


## step one lets make a table for all the families
bar <- as.data.frame(matrix(,max(sort(table(data[,1]))), length(tree$tip.label)))
colnames(bar) <- tree$tip.label
for(i in 1:length(tree$tip.label)){
  temp <- data[data[, 1] == tree$tip.label[i],]
  bar[1:nrow(temp),i] <- data[data[, 1] == tree$tip.label[i], 2]
}

## So right here we can calculate the corrlation between variance in chromosome
## number and the number of species for which we have samples.
colVars <- function(x, na.rm=FALSE, dims=1, unbiased=TRUE, SumSquares=FALSE,
                    twopass=FALSE) {
  if (SumSquares) return(colSums(x^2, na.rm, dims))
  N <- colSums(!is.na(x), FALSE, dims)
  Nm1 <- if (unbiased) N-1 else N
  if (twopass) {x <- if (dims==length(dim(x))) x - mean(x, na.rm=na.rm) else
    sweep(x, (dims+1):length(dim(x)), colMeans(x,na.rm,dims))}
  (colSums(x^2, na.rm, dims) - colSums(x, na.rm, dims)^2/N) / Nm1
}
var.vals <- colVars(bar, na.rm=T)
rec.vals <- vector()
for(i in 1:ncol(bar)){
  rec.vals[i] <- nrow(bar) - sum(is.na(bar[,i]))
}
covar.vals <- matrix(,2,48)
covar.vals[1,] <- var.vals
covar.vals[2,] <- rec.vals

covar.vals <- covar.vals[,c(1,3:4,6:11,15:45,48)]
cor.test(covar.vals[1,], covar.vals[2,])
colnames(covar.vals) <- names(var.vals[c(1,3:4,6:11,15:45,48)])
grp.cnt <- colSums(!is.na(bar))
new.results <- as.data.frame(matrix(,1,2))
counter <- 1
for(j in 1:48){
  temp.counts <- as.data.frame(table(bar[,j])/grp.cnt[j])
  #temp.counts[,2] <- round(temp.counts[,2] * 100)
  temp.counts[,2] <- round(temp.counts[,2] * 1000)
  temp.counts[,1] <- as.numeric(as.character(temp.counts[,1]))
  new.counts <- vector()
  for(i in 1:nrow(temp.counts)){
    new.counts <- c(new.counts, rep(temp.counts[i,1], temp.counts[i,2]))
  }
  new.results[1:length(new.counts),j] <- new.counts
}
colnames(new.results) <- tree$tip.label

## now lets switch the big table into a 2 column dataframe for graphing
foo <- as.data.frame(matrix(,1,2))
counter <- 1
for(i in 1:48){
  print(i)
  for(j in 1:nrow(new.results)){
    foo[counter,1] <- colnames(new.results)[i]
    foo[counter,2] <- new.results[j,i]
    counter <- counter + 1
  }
}

data <- foo

## and a little cleaning now
rm(bar); rm(foo); rm(new.results); rm(temp); rm(temp.counts); rm(counter)
rm(grp.cnt); rm(i); rm(j); rm(new.counts)

## lets make another version of the dataset that has all values in it
data3 <- data
data3[4945:{4945+59-1},1] <- "holder"
data3[4945:{4945+59-1},2] <- 4:62

## lets name our columns and switch family to a factor
colnames(data) <- c("family", "chrom")
data$family <- as.character(data$family)
data$family <- factor(data$family, levels=tree$tip.label, ordered=TRUE)

## lets name our columns and switch family to a factor
colnames(data3) <- c("family", "chrom")
data3$family <- as.character(data3$family)
data3$family <- factor(data3$family, levels=tree$tip.label, ordered=TRUE)

data$chrom <- factor(data$chrom)














library(plyr)
data3 <- ddply(data3, .(family, chrom), summarise, freq = length(chrom))
data3 <- data3[!is.na(data3[,2]),]
theme_set(theme_bw(24))
ggplot(data3, aes(y = family, x = chrom, fill = freq)) + geom_tile() + 
  scale_fill_gradient(low = "blue", high = "red", name="Percent", space="rgb") +
  xlab("Diploid Number") +
  scale_x_discrete(breaks=seq(0,70, by=5)) +
  theme(axis.line = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour="blue"),
        panel.border = element_blank(),
        #panel.background = element_blank(),
        axis.title.x = element_text(size = rel(.6)),
        axis.text.x = element_text(size = rel(.6), colour = "black"),
        legend.key.size = unit(.35, "cm"),
        legend.text = element_text(size = 6),
        legend.title = element_text(size =7),
        legend.position = c(.95, .87))



# lets switch from diploid to haploid counts
for(i in 1:339){
  if(Even(data3[i,2])){
    data3[i,2] <- (data3[i,2]-2)/2
  }else{
    data3[i,2] <- (data3[i,2]-1)/2
  }
}


ggplot(data3, aes(y = family, x = chrom, fill = freq)) + geom_tile() + 
  scale_fill_gradient(low = "blue", high = "red", name="Percent", space="rgb") +
  xlab("Autosomes") + scale_x_discrete(breaks=seq(0,35, by=5)) +
  theme(axis.line = element_blank(),
        panel.grid.minor = element_line(colour="blue"),
        panel.border = element_blank(),
        axis.title.x = element_text(size = rel(.45)),
        axis.text.x = element_text(size = rel(.45), colour = "black"),
        axis.title.y = element_text(size = rel(.3), colour = "black"),
        axis.text.y = element_text(size = rel(.3), colour = "black"),
        legend.key.size = unit(.45, "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.position = c(.95, .88))
