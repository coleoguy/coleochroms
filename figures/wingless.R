# Heath Blackmon
# 10 November
# This code makes the figure for the analysis of chromosome number in
# carabidae
library(coda)
library(ggplot2)
library(ggpubr)
library(viridis)
# this sets up a theme for ggplot
ggtheme <- theme_bw() + theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(),
                              panel.border=element_blank(),
                              axis.line = element_line(colour="grey30"),
                              axis.title = element_text(colour="grey20"),
                              axis.text = (element_text(colour="grey30")),
                              legend.title = element_text(colour="grey20"),
                              legend.text = element_text(colour="grey30"))

# This reads in the data without polyploidy

dat <- read.csv("../results/wingless.chromeplus.csv")
dat <- dat[dat$type %in% c("fission","fusion"),]
hpd1 <- data.frame(X = c(HPDinterval(as.mcmc(dat$rate[dat$type=="fission"]))[1,],
                         HPDinterval(as.mcmc(dat$rate[dat$type=="fusion"]))[1,]),
                   Y = c(-1, -1, -2, -2),
                   types = rep(c("fission", "fusion"), each = 2))

# This reads in the data with polyploidy
dat2 <- read.csv("../results/wingless.wpoly.chromeplus.csv")
dat2 <- dat2[dat2$type %in% c("fission","fusion", "wgd"),]
hpd2 <- data.frame(X = c(HPDinterval(as.mcmc(dat2$rate[dat2$type=="fission"]))[1,],
                         HPDinterval(as.mcmc(dat2$rate[dat2$type=="fusion"]))[1,],
                         HPDinterval(as.mcmc(dat2$rate[dat2$type=="wgd"]))[1,]),
                   Y = c(-2, -2, -4, -4, -6, -6),
                   types = rep(c("fission", "fusion", "wgd"), each = 2))

m1 <- ggplot(dat, aes(x=rate)) +
  geom_density(aes(fill=as.factor(type),y=..density..),
               stat="density", position="identity", alpha=0.35) +
  geom_line(data=hpd1, aes(x=X, y=Y, color=types),
            alpha=0.45, size=1.4, lineend="round") +
  geom_vline(xintercept=0, linetype="dashed", color="grey40") +
  scale_fill_viridis_d(option="D", end=.6)+
  scale_color_viridis_d(option="D", end=.6)+
  guides(fill=guide_legend(title="parameter"),
         color="none") +
  xlab("Rate difference (per my)\n wingless-winged") +
  ggtheme

m2 <- ggplot(dat2, aes(x=rate)) +
  geom_density(aes(fill=as.factor(type),y=..density..),
               stat="density", position="identity", alpha=0.35) +
  geom_line(data=hpd2, aes(x=X, y=Y, color=types),
            alpha=0.45, size=1.4, lineend="round") +
  geom_vline(xintercept=0, linetype="dashed", color="grey40", size=.5) +
  scale_fill_viridis_d(option="D", end=.9)+
  scale_color_viridis_d(option="D",end=.9)+
  guides(fill=guide_legend(title="parameter"),
         color="none") +
  xlab("Rate difference (per my)\n wingless-winged") +
  ggtheme

ggarrange(m1,m2)
# exported 9.75x4.5
