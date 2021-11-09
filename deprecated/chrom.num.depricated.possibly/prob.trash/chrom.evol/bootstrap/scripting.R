library(geiger)
## This little function just lets me pull the end of a line of text off
substrRight <- function(x, n){
  sapply(x, function(xx)
    substr(xx, (nchar(xx)-n+1), nchar(xx))
  )
}

setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/analyses/chrom.evol")
groups <- c("Pimelia", "Harpalus", "Cytronus", "Calathus")
  
#'Dendroctonus', 'Curculionidae',
#            'Coccinellidae', 'Lampyridae', 'Pterostichus', 'Bembidion',
#            'Cicindela', 'Carabidae')

# these are pulled out because they worked on the first try
# Cerambycidae', 'Chrysolina', 'Chrysomelidae', 'Timarcha', 'Tenebrionidae', 

# ok and these worked on the second try
#'Scarabaeidae', 'Ips', 'Elateridae', 'Dytiscidae', 

for(k in 1:length(groups)){ #length(groups)
  for(k2 in 1:20){
    print(paste(groups[k], ' tree: ', k2, sep=""))
    ## Read in the results of the chromevol program
    data <- readLines(paste(groups[k], "/result", k2, "/chromEvol.res", sep=""))[c(13,16,17, 8, 10)]

    ## now we will extract the parameter estimates and scale as appropriate
    ## (necessary because Itay's program scales tree to help in optimization
    ## so we have to multiply the rates to get them back into the tree units)

    rate <- as.numeric(strsplit(data[1], 'by ')[[1]][2])
    g <- as.numeric(strsplit(data[2], '\t')[[1]][2]) * rate
    l <- as.numeric(strsplit(data[3], '\t')[[1]][2]) * rate
    minchrom <- as.numeric(strsplit(data[4], ': ')[[1]][2])
    maxchrom <- as.numeric(strsplit(data[5], ': ')[[1]][2])

    ## now we need to simulate new dataset based on these parameters
    ## geiger has fairly straight forward simulation function.

    ## get the tree
    tree <- read.tree(paste(groups[k], "/", groups[k], k2, ".tree", sep=""))


    ## make the q-matrix given by row to do this we need to expand out the rate
    ## estimates for all possible states on our tree four purposes we will construct
    ## a matrix that has rows and columns equal to our max-min+1 chrom.num
    side <- maxchrom - minchrom + 1
    q <- matrix(0,side,side)
    for(i in 1:{side-1}){
      q[i, i + 1] <- g        # this fills in the chromosome gain rates
      q[i + 1, i] <- l        # this fills in the chromosome loss rates
    }
    diag(q) <- -rowSums(q)    # this fills in the diagonal

    ## set the root to the ML estimate from orig model parametization
    rt <- 1

    ## this is the actual simulation. The 17 being added at the end just gets us back on the same part of the scale 
    ## I've run some little tests it doesnt seem to matter either way
    sim.data <- as.data.frame(sim.char(tree, q, model="discrete", root=2, nsim=5)+minchrom)

    ## lets save the results of the data simulation into the format that we will
    ## need for chromEvol
    foo <- vector()
    for(j in 1: ncol(sim.data)){
      for(i in 1:nrow(sim.data)){
        foo <- paste(foo, ">", rownames(sim.data)[i], "\n", sim.data[i, j], "\n", sep="")
      }
      cat(foo, file= paste("simmed.data/", groups[k], ".", k2, ".", j, ".txt", sep=""))
    }
  }
}




# Now lets create all the params files that we will need
for(i in 1:length(groups)){#
  for(j in 1:20){#20
    for(j2 in 1:5){#5
    params <- paste('_mainType Optimize_Model\n',
                    '_outDir simmed.data/results/', groups[i], ".", j, ".", j2, '\n', 
                    '_dataFile ', 'simmed.data/', groups[i], '.', j, '.', j2, '.txt\n',
                    '_treeFile ', groups[i], '/', groups[i], j, '.tree\n', 
                    '_maxChrNum -1\n',
                    '_minChrNum -1\n',
                    '_simulationsNum 0\n', 
                    '_gainConstR = 1\n',
                    '_lossConstR = 1\n',
                    sep = '')
    cat(params, file= paste('simmed.data/', groups[i], '.params', '.', j, ".", j2,".txt", sep = ""))
    }
  }
}