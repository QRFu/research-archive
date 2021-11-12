#For two-sample t-test,
#Classical: 
library(pwr)
pwr.t.test(d = 0.5, power = 0.80, sig.level = 0.05)
#Bayesian:
library(SSDbain)
SSDttest(type='equal',Population_mean=c(0.5,0),var=NULL,BFthresh=3,eta=0.8,Hypothesis='two-sided',T=10000)
SSDttest(type='equal',Population_mean=c(0.5,0),var=NULL,BFthresh=3,eta=0.8,Hypothesis='one-sided',T=10000)

#For three groups ANOVA,
#Classical
library(pwr)
pwr.anova.test(k = 3, f = 0.25, sig.level = 0.05, power = 0.8)
#Bayesian
library(SSDbain)
SSDANOVA(hyp1="mu1=mu2=mu3",hyp2="Ha",type="equal",f1=0,f2=0.25,var=NULL,BFthresh=3,eta=0.80,T=10000,seed=10)
SSDANOVA(hyp1="mu1=mu2=mu3",hyp2="mu1>mu2>mu3",type="equal",f1=0,f2=0.25,var=NULL,BFthresh=3,eta=0.80,T=10000,seed=10)

#For three predictors regression:
#Classical
library(pwr)
pwr.f2.test(u = 3, f2 = 0.13/(1 - 0.13), sig.level = 0.05, power = 0.8)  
#Bayesian
library(SSDbain)
SSDRegression(Hyp1='beta1=beta2=beta3=0',Hyp2='Ha',k=3,rho=matrix(c(1,0,0,0,1,0,0,0,1),nrow=3),
R_square1=0,R_square2=0.13,T_sim=10000,BFthresh=3,eta=0.8,seed=10,standardize=FALSE,ratio=c(1,1,1))
SSDRegression(Hyp1='beta1=beta2=beta3=0',Hyp2='beta1>0&beta2>0&beta3>0',k=3,rho=matrix(c(1,0,0,0,1,0,0,0,1),nrow=3),
R_square1=0,R_square2=0.13,T_sim=10000,BFthresh=3,eta=0.8,seed=10,standardize=FALSE,ratio=c(1,1,1))
