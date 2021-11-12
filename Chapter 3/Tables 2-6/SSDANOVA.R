library(SSDbain)
#f=0.25 eta=0.8
#when the variances are equal 
SSDANOVA(hyp1="mu1=mu2=mu3",hyp2="Ha",type="equal",f1=0,f2=0.25,var=NULL,BFthresh=3,eta=0.80,T=10000,seed=10)
#when the variances are unequal 
SSDANOVA(hyp1="mu1=mu2=mu3",hyp2="Ha",type="unequal",f1=0,f2=0.25,var=c(1.5,0.75,0.75),BFthresh=3,eta=0.80,T=10000,seed=10)
#when the variances are unequal and want to obtain the robust result
SSDANOVA_robust(hyp1="mu1=mu2=mu3",hyp2="Ha",f1=0,f2=0.25,skews=c(0,0,0),kurts=c(0,0,0),var=c(1.5,0.75,0.75),BFthresh=3,eta=0.8,T=10000,seed=10)
