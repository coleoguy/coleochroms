## this is a version of figure one that uses raw counts instead of percents
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

setwd("~/Desktop/Dropbox/papers/fragileY/analyses.and.data/fig1")
tree <- read.tree('new2.phy')[[1]]
tree <- ape:::drop.tip(tree, 'Hydradephaga')

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

data[4537:{4537+59-1},1] <- "holder"
data[4537:{4537+59-1},2] <- 4:62
rownames(data)<- 1:4595
## lets name our columns and switch family to a factor
colnames(data) <- c("family", "chrom")
data$family <- as.character(data$family)
data$family <- factor(data$family, levels=tree$tip.label, ordered=TRUE)


library(plyr)
data <- ddply(data, .(family, chrom), summarise, freq = length(chrom))
data <- data[!is.na(data[,2]),]

# lets switch from diploid to haploid counts
for(i in 1:nrow(data)){
  if(Even(data[i,2])){
    data[i,2] <- (data[i,2]-2)/2
  }else{
    data[i,2] <- (data[i,2]-1)/2
  }
}
data$chrom <- factor(data$chrom)

theme_set(theme_bw(24))

data <- data[1:346,]
ggplot(data, aes(y = family, x = chrom, fill = log10(freq))) + geom_tile() + 
  scale_fill_gradient(low = "blue", high = "red", name="Records", space="rgb",
                      labels=(c("1","","10","","100","300"))) +
  xlab("Autosomes") +
  scale_x_discrete(breaks=seq(0,35, by=5)) +
  theme(axis.line = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour="blue"),
        panel.border = element_blank(),
        #panel.background = element_blank(),
        axis.title.x = element_text(size = rel(.45)),
        axis.text.x = element_text(size = rel(.45), colour = "black"),
        axis.title.y = element_text(size = rel(.3), colour = "black"),
        axis.text.y = element_text(size = rel(.3), colour = "black"),
        legend.key.size = unit(.45, "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.position = c(.99, .88))



## Lets try to add some rows for the sum of adephaga and sum of polyphaga
poly <- unique(data[94:346,1])
ade <- unique(data[4:93,1])

foo <- vector()
foo[1:34] <- 0
for(i in 1:34){
  temp <- data[data[, 2] == i, ]
  foo[i] <- sum(temp[temp$family %in% ade,]$freq)
}
ade.foo <- foo

foo <- vector()
foo[1:34] <- 0
for(i in 1:34){
  temp <- data[data[, 2] == i, ]
  foo[i] <- sum(temp[temp$family %in% poly,]$freq)
}
poly.foo <- foo

newlines <- matrix(,68,3)
newlines[1:34, 1] <- "Polyphaga"
newlines[35:68, 1] <- "Adephaga"
newlines[1:34,2] <- 1:34
newlines[35:68,2] <- 1:34
newlines[,3] <- c(poly.foo, ade.foo)
newlines <- newlines[newlines[, 3] > 0, ]
newlines <- as.data.frame(newlines)
newlines[, 2] <- as.factor(newlines[,2])
newlines[, 1] <- as.factor(newlines[,1])
newlines[, 3] <- as.numeric(as.character(newlines[,3]))
colnames(newlines) <- colnames(data)
data <- rbind(newlines, data)




ggplot(data, aes(y = family, x = chrom, fill = log10(freq))) + geom_tile() + 
  scale_fill_gradient(low = "blue", high = "red", name="Records", space="rgb",
                      labels=(c("1","","10","","100","300"))) +
  xlab("Autosomes") +
  scale_x_discrete(breaks=seq(0,35, by=5)) +
  theme(axis.line = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour="blue"),
        panel.border = element_blank(),
        #panel.background = element_blank(),
        axis.title.x = element_text(size = rel(.45)),
        axis.text.x = element_text(size = rel(.45), colour = "black"),
        axis.title.y = element_text(size = rel(.3), colour = "black"),
        axis.text.y = element_text(size = rel(.3), colour = "black"),
        legend.key.size = unit(.45, "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.position = c(.99, .88))



