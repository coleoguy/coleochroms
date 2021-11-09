load("~/Desktop/Dropbox/projects/coleo.chrom.num/data.analysis/results/wingless.analysis.RData")

results <- x
for(i in 1:100){
  results[[i]] <- results[[i]]/scaler[i]
}

# here are the likelihoods
plot(results[[1]]$p, type="l",ylim=c(-540,-360))
for(i in 1:100) lines(results[[i]]$p, col=rainbow(100)[i])

# here are the parameter estimates
post.burn <- results[[1]][sample(251:500, 100),]
for(i in 2:100) post.burn <- rbind(post.burn, 
                                   results[[i]][sample(251:500, 100), ])


rates <- c(post.burn$asc2 - post.burn$asc1,
           post.burn$desc2 - post.burn$desc1,
           post.burn$pol2 - post.burn$pol1)

tran.type <- rep(c("ascending","descending","polyploidy"), each=10000)
dat <- data.frame(rates, tran.type)
library(ggraptR)
ggraptR(dat)

ggplot(dat, aes(x=~rates)) + geom_density(aes(fill=~as.factor(tran.type), y=~..density..), stat="density", position="identity", alpha=0.5) + facet_grid(tran.type ~ .) + theme_bw() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + guides(fill=guide_legend(title="tran.type")) + xlab("Rate Wingless-Winged (per myr)") + ylab("density")

library(coda)
asc <- post.burn$asc2-post.burn$asc1
desc <- post.burn$desc2-post.burn$desc1
pol <- post.burn$pol2-post.burn$pol1

# values for paper

HPDinterval(as.mcmc(asc))
HPDinterval(as.mcmc(desc))
HPDinterval(as.mcmc(pol))
sum(asc>0)/length(asc)
sum(desc>0)/length(asc)
sum(pol>0)/length(asc)

