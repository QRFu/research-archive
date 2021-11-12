#Table 5, rho=0.2, H0:beta1=beta2=beta3=0 vs Ha

library(SSDbain)
#change l for different correlation matrix
l=0.2
k<-3
rho<-matrix(runif(k*k),nrow=k)
for (i in 1:k){
  for (j in 1:k){
    if(i==j){
      rho[i,j]=1
    }else
      rho[i,j]=l
  }
}
SSDRegression(Hyp1='beta1=beta2=beta3=0',Hyp2='Ha',k=3,rho,R_square1=0,R_square2=0.13,T_sim=10000,BFthresh=3,eta=0.8,seed=10,standardize=FALSE,ratio=c(1,1,1))
