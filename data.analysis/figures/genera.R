#attached libraries
library(ggplot2)
library(ggridges)
library(dplyr)
library(coda)
library(tidyr)

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


##Calculate HPD intervals for asc, desc, and pol for each genus

hpd_polyphaga <- polyphaga %>%
  group_by_(.dots=c("Genus", "name", "indexgroup")) %>%
  do(data.frame(HPDinterval(as.mcmc(.$value))))

hpd_polyphaga_long <- hpd_polyphaga %>%
  pivot_longer(c("lower", "upper"), names_to = "interval")

polyphagainterval <- ggplot(hpd_polyphaga_long, aes(x = value, y = Genus)) +
  geom_line(aes(color = indexgroup)) +
  geom_point(aes(color = indexgroup)) +
  facet_wrap(facets = hpd_polyphaga_long$name)

hpd_adephaga <- adephaga %>%
  group_by_(.dots=c("Genus", "name", "indexgroup")) %>%
  do(data.frame(HPDinterval(as.mcmc(.$value))))
hpd_adephaga_long <- hpd_adephaga %>%
  pivot_longer(c("lower", "upper"), names_to = "interval")

adephagainterval <- ggplot(hpd_adephaga_long, aes(x = value, y = Genus)) +
  geom_line(aes(color = indexgroup)) +
  geom_point(aes(color = indexgroup)) +
  facet_wrap(facets = hpd_adephaga_long$name)

hpd_all <- dat %>%
  group_by_(.dots=c("Genus", "name", "indexgroup")) %>%
  do(data.frame(HPDinterval(as.mcmc(.$value))))
hpd_all_long <- hpd_all %>%
  pivot_longer(c("lower", "upper"), names_to = "interval")

allinterval <- ggplot(hpd_all_long, aes(x = value, y = Genus)) +
  geom_line(aes(color = indexgroup)) +
  geom_point(aes(color = indexgroup)) +
  facet_wrap(facets = hpd_all_long$name)

allinterval 
