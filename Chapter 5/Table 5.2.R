library(SSDbain)
SSDttest(type='equal',Population_mean=c(0.5,0),var=NULL,BFthresh=3,eta=0.8,Hypothesis='two-sided',T=10000)
SSDttest(type='equal',Population_mean=c(0.5,0),var=NULL,BFthresh=3,eta=0.8,Hypothesis='one-sided',T=10000)
SSDANOVA(hyp1="mu1=mu2=mu3",hyp2="Ha",type="equal",f1=0,f2=0.25,var=NULL,BFthresh=3,eta=0.80,T=10000,seed=10)
SSDANOVA(hyp1="mu1=mu2=mu3",hyp2="mu1>mu2>mu3",type="equal",f1=0,f2=0.25,var=NULL,BFthresh=3,eta=0.80,T=10000,seed=10)
SSDRegression(Hyp1='beta1=beta2=0',Hyp2='Ha',k=2,rho=matrix(c(1,0,0,1),nrow=2),R_square1=0,R_square2=0.13,T_sim=10000,BFthresh=3,eta=0.8,seed=10,standardize=FALSE,ratio=c(1,1))
SSDRegression(Hyp1='beta1=beta2=0',Hyp2='beta1>0&beta2>0',k=2,rho=matrix(c(1,0,0,1),nrow=2),R_square1=0,R_square2=0.13,T_sim=10000,BFthresh=3,eta=0.8,seed=10,standardize=FALSE,ratio=c(1,1))

