#J=1
#ROW 1 for Table 1:
N1<-N2<-N<-100
samph<-c(N1,N2)
ngroup<-samph/J
J<-1
m1<-0
m2<-0
sd1<-sd2<-1
#simulate data
set.seed(10)

X<-rnorm(N1,m1,sd1)
Y<-rnorm(N2,m2,sd2)
estimate<-c(mean(X), mean(Y))
variance<-c(var(X)/N1, var(Y)/N2) 
library(bain)
bf<-c()
names(estimate) <- c("mu1","mu2")
variance_sp<-(variance[1]+variance[2])/2
covlist<-list(matrix(variance_sp),matrix(variance_sp))
res<-bain(estimate, n=ngroup, "mu1 = mu2",Sigma=covlist,group_parameters=1,joint_parameters = 0)
fit<-res$fit$Fit[1]
com<-res$fit$Com[1]
bf<-res$fit$BF[1]
fit 
com  
bf

#ROW 2 for Table 1:
N1<-N2<-N<-100
samph<-c(N1,N2)
ngroup<-samph/J
J<-1
m1<-0.5
m2<-0
sd1<-sd2<-1
#simulate data
set.seed(10)

X<-rnorm(N1,m1,sd1)
Y<-rnorm(N2,m2,sd2)
estimate<-c(mean(X), mean(Y))
variance<-c(var(X)/N1, var(Y)/N2) 
library(bain)
bf<-c()
names(estimate) <- c("mu1","mu2")
variance_sp<-(variance[1]+variance[2])/2
covlist<-list(matrix(variance_sp),matrix(variance_sp))
res<-bain(estimate, n=ngroup, "mu1 = mu2",Sigma=covlist,group_parameters=1,joint_parameters = 0)
fit<-res$fit$Fit[1]
com<-res$fit$Com[1]
bf<-res$fit$BF[1]
fit 
com  
bf




