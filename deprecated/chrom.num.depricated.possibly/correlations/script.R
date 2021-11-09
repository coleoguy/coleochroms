setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/analyses/correlations")
new.old <- read.csv('new.and.old.csv')
cv.age <- read.csv('cv.and.age.csv')
cv.age[,1:2] <- cv.age[,2:1]
colnames(cv.age) <- c("Age", "CV")

par(mfcol=c(1,2))
foo <- cor.test(new.old[,2], new.old[,3])
plot(new.old[,2:3], pch=19, xlab = "New CV Estimate", ylab= "Old CV Estimate")
# 95% CI for Correlation Coefficient = .91 - .98
# pvalue << .0001

foo <- cor.test(cv.age[,2], cv.age[,3])
fam.col <- vector()
fam.col[1:40] <- "black"
fam.col[c(18,26,27,28,29,30,33,35,37,38,40)] <- "blue"
plot(cv.age[3:2], col= fam.col, pch=19, xlab= "Min. Age", ylab="CV")
# 95% CI for Correlation Coefficient = .19-.69
# pvalue = .002
