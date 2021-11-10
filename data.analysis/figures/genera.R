#attached libraries
library(ggplot2)
library(ggridges)
library(dplyr)
library(coda)

dat <- read.csv("../results/genera.chromeplus.csv", row.names = 1)
genera <- unique(dat$Genus)
dat$suborder[dat$Genus %in% genera[c(3,5,6,7,9,10,12)]] <- "Polyphaga"
dat$suborder[dat$Genus %in% genera[c(1,2,4,8,11)]] <- "Adephaga"
#add ne index variable
dat$expectedne[dat$Genus %in% genera[c(2,3,5,6,12)]] <- 2
dat$expectedne[dat$Genus %in% genera[c(11,10,9)]] <- 3
dat$expectedne[dat$Genus %in% genera[c(1,4,8,7)]] <- 4
#add ne index variable as factor
dat$indexgroup[dat$Genus %in% genera[c(2,3,5,6,12)]] <- "low"
dat$indexgroup[dat$Genus %in% genera[c(11,10,9)]] <- "medium"
dat$indexgroup[dat$Genus %in% genera[c(1,4,8,7)]] <- "high"
#get maximum values for axis limits
dat %>%
  group_by(suborder) %>%
  summarize(max(value))
#order genera by expectedne
dat$Genus <- factor(dat$Genus, levels = unique(dat$Genus[order(dat$expectedne)]))
#make a polyphaga dataset
polyphaga <- dat[dat$suborder == "Polyphaga",]
#make adephaga dataset
adephaga <- dat[dat$suborder == "Adephaga",]

####Make polyphaga datasets
#jitter
jitterfigure <- ggplot(polyphaga) +
  geom_jitter(aes(Genus, value)) +
  ylim(0,0.25)

jitterfigure

jitterfigure2 <- ggplot(polyphaga) +
  geom_jitter(aes(expectedne, value)) +
  ylim(0,0.25)
jitterfigure2

#boxplot
boxplotfigure <- ggplot(polyphaga) +
  geom_boxplot(aes(Genus, value))

boxplotfigure

#density
# TODO where is geom_density_ridges() from can't find it
densityfigure <- ggplot(polyphaga, aes(x = value, y = Genus)) +
  geom_density_ridges(aes(fill = name)) +
  xlim(0,0.06)

densityfigure


# TODO example of calcing HPD interval

foo <- dat$value[dat$Genus=="Bembidion" & dat$name == "asc1"]
HPDinterval(as.mcmc(foo))[1,]
