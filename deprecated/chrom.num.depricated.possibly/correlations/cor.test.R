#mean rate for genera calculated using phylogenies
mr <- c(0.243,24.33,.462,.515,.088,.558,.323)
#scaled covariance using all taxa and fossil dates
scv <- c(.12,.42,.07,.08,.24,3.87,.69)
#result
cor.test(mr,scv, method="kendall")
