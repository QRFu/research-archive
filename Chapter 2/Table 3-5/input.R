# R code for the two-sided t-test when the effect size d=0.2, BFthresh=3, eta=0.80, and the number of simulations is 10000.
library(SSDbain)
SSDttest(type='equal',Population_mean=c(0.2,0),var=NULL,BFthresh=3,eta=0.80,Hypothesis='two-sided',T=10000)
