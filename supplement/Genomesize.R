trees<-read.nexus('coleo.trees.nexus')

plot(trees[[1]], cex=.1)


foo <- read.csv("genome_size_data_060722_08_29_50.csv")
foo$Ne.Class <- as.factor(foo$Ne.Class)
foo$Ne.Class <- factor(foo$Ne.Class, levels=c('Low', 'Medium', 'High'))





boxplot(foo$Size~foo$Ne.Class)
library(beeswarm)
beeswarm(foo$Size~foo$Ne.Class, xlab= 'Effective Population Size Class', ylab= 'Genome Size (C-value)')
