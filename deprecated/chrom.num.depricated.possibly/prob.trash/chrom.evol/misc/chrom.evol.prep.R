setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/analyses")
library(ape)
library(geiger)

# Get the data in
trees <- read.nexus("../data/trees.nexus")
groups <- read.csv('../data/groups.csv', as.is = T)
data <- read.csv('../data/chrom.nums.csv', as.is = T)
data.v <- data[,2]
names(data.v) <- data[,1]
rm(data)
setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/analyses/chrom.evol")
# Create input data files fasta format for chrom numbers and all the trees
for(i in 1:ncol(groups)){
  dir.create(colnames(groups[i]))
  foo <- treedata(trees[[1]], data.v[names(data.v) %in% groups[,i]])
  phy <- foo[[1]]
  dat <- foo[[2]]
  ids <- paste(rep(">", length(dat)),row.names(dat), sep = "")
  vals <- as.character(round(dat))  # I have to round to make it work for itays program
  final <- vector()
  for(j in 1:length(dat)){
    final <- paste(final, ids[j], "\n", vals[j], "\n", sep = "")    
  }
  cat(final, file= paste(colnames(groups[i]), "/",
                         colnames(groups)[i], ".txt", sep = ""))
  for( k in 1:20){  #takes to long to look at all trees
    sub.tree <- treedata(trees[[sample(1:100, 1)]], data.v[names(data.v) %in% groups[,i]])[[1]]
    write.tree(sub.tree, file= paste(colnames(groups[i]), "/", 
                                     colnames(groups)[i], k, 
                                     ".tree", sep = ""))
  }
}

# Now lets create all the params files that we will need
for(i in 1:ncol(groups)){
  for(j in 1:20){
    params <- paste('_mainType Optimize_Model\n',
                    '_outDir result', j, '\n', '_dataFile ', colnames(groups[i]),
                    '.txt\n',
                    '_treeFile ', colnames(groups[i]),
                    j, '.tree\n', '_maxChrNum -10\n',
                    '_minChrNum -1\n',
                    '_simulationsNum 0\n', '_gainConstR = 1\n',
                    '_lossConstR = 1\n', '_duplConstR = 1\n', sep = '')
    cat(params, file= paste(colnames(groups[i]), "/",
                           'params', j, ".txt", sep = ""))
  }
}


# getting counts for groups
foo <- vector(length=23)
names(foo) <- colnames(groups)
for(i in 1:23){
  foo[i] <- sum(groups[,i]!="")
}