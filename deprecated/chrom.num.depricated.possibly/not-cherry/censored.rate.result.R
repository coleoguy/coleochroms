setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/not-cherry")
# Carabidae analysis
data <- read.csv("bm.rates.csv", as.is=T, header=T)
1-pchisq((2*(507.4481-465.9293)), 1)
boxplot(data[,c("wingless", "winged")],
        outline=F, col="gray",
        names=c("wingless", "winged"), ylab="Rate of Karyotype Evolution")
#SE of winged 0.08183
sd(data[,2])/sqrt(nrow(data))
#SE of wingless 0.4132
sd(data[,3])/sqrt(nrow(data))


# Genera analysis
setwd("~/Desktop/Dropbox/papers/chrom.num/data and analysis/not-cherry/genera.analysis/all.genera")
data.poly <- read.csv("pol.3rates.csv", as.is=T, header=T)
data.ade <- read.csv("ade.3rates.csv", as.is=T, header=T)
boxplot(data.poly[,2:4],
        outline=F, col="gray",
        names=c("low","medium", "high"), ylab="Rate of Karyotype Evolution")
boxplot(data.ade[,2:4],
        outline=F, col="gray",
        names=c("Low","Medium", "High"), ylab="Rate of Karyotype Evolution")

## Now for the figure
par(mfcol=c(2,2))
boxplot(data[,c("wingless", "winged")],
        outline=F, col="gray", main="Carabidae",
        names=c("wingless", "winged"), ylab="Rate of Karyotype Evolution")
text(x=-0.28, y=27, label="A. Carabidae", pos=4, font=2)
boxplot(data.poly[,2:4],
        outline=F, col="gray", main="Polyphaga",
        names=c("low","medium", "high"), ylab="Rate of Karyotype Evolution")
plot.new()
boxplot(data.ade[,2:4],
        outline=F, col="gray", main="Adephaga",
        names=c("Low","Medium", "High"), ylab="Rate of Karyotype Evolution")

