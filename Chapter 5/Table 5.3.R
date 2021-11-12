library(SSDbain)
SSDANOVA(hyp1="mu1=mu2=mu3",hyp2="Ha",type="equal",f1=0,f2=0.25,var=NULL,BFthresh=3,eta=0.80,T=10000,seed=10)
SSDANOVA(hyp1="mu1=mu2=mu3",hyp2="mu1>mu2>mu3",type="equal",f1=0,f2=0.25,var=NULL,BFthresh=3,eta=0.80,T=10000,seed=10)
SSDANOVA(hyp1="mu1>mu2>mu3",hyp2="Hc",type="equal",f1=0.25,f2=0.25,var=NULL,BFthresh=3,eta=0.80,T=10000,seed=10)