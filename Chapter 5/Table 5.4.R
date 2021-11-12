library(SSDbain)
SSDRegression(Hyp1='beta1=beta2=beta3=0',Hyp2='beta1>0&beta2>0&beta3>0',
k=3,rho=matrix(c(1,0,0,0,1,0,0,0,1),nrow=3),R_square1=0,R_square2=0.13,
T_sim=10000,BFthresh=3,eta=0.8,seed=10,standardize=FALSE,ratio=c(1,1,1))