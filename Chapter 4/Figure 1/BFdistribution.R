#Loading/Attaching and Listing of Packages
library(stringr)
library(conflicted)
library(pkgmaker, warn.conflicts = FALSE)
library(foreach)
library(doParallel)
library(doRNG)
library(pkgmaker, warn.conflicts = FALSE)
#Detect the Number of CPU Cores
no_cores <- detectCores() - 1
#Initiate cluster
cl <- makeCluster(no_cores)
# register the parallel backend with the foreach package
registerDoParallel(cl)
#this is needed to see the package
clusterEvalQ(cl, .libPaths())
J=1
N_min<-10
N_max<-1000
T_sim<-100000
T<-T_sim
#R_square<-0.13#effect size
#number of predictors
k<-3
l=0
rho<-matrix(c(1,l,l,l,1,l,l,l,1),nrow=k)
Hyp1<-'beta1=beta2=beta3=0'
Hyp2<-'beta1>0&beta2>0&beta3>0'
ratio<-c(1,1,1)
N<-23
bf<-numeric(T)
samph<-rep(N,k)
sampm<-samph/J
R_square1<-0
R_square2<-0.13
# beta1<-c(0,0,0)
# beta2<-c(0.208,0.208,0.208)
#calculate beta
beta<-cal_beta(k,R_square1,R_square2,Hyp1,Hyp2,ratio,rho)
beta1<-beta[[1]]
beta2<-beta[[2]]
flag_Hyp<-1
seed<-10
set.seed(seed,kind="Mersenne-Twister",normal.kind="Inversion")
standardize=FALSE
beta<-beta1
#calculate BF
BF<- foreach(ii = 1:T, .packages = c("bain")) %dorng% {
  #  for (ii in 1:T){
  res<- list()
  library(MASS)
#simulate data
  X<-mvrnorm(n=N,mu=numeric(k),Sigma=rho)
  if(flag_Hyp){
    sd<-sqrt(1-R_square1)
  }else{
    sd<-sqrt(1-R_square2)
  }
  epsilon<-rnorm(n=N,mean=0,sd=sd)
  Y<-  epsilon
  for (i in 1:k)
  {
    Y<-Y+beta[i]*X[,i]
  }
  library(fungible)
  library(stats)


  if(standardize==TRUE){
    # Obtain standardized regression coefficients
    capture.output(int <-fungible:: seBeta(X = X, y = Y, Nobs = N,
                                           estimator = "Normal"), file='NUL')
    # Store the parameter estimates
    estimate <- int$CIs[, 2]
    for (i in 1:k)
    {
      names(estimate)[i]<-paste('beta',i,sep='')
    }

    # Store the variance covariance matrix for the parameters
    # of interest
    cov <- list(int$cov.mat)
    cov<-matrix(unlist(cov),nrow = k)
  }else{
    # Obtain standardized regression coefficients for the unstandardize case
    M=cbind(X,Y)
    data<-as.list(as.data.frame(M))
    for (i in 1:k)
    {
      names(data)[i]<-paste('beta',i,sep='')
    }
    Equ<-'Y ~ beta1'
    for (i in 2:k){
      Equ<-paste(Equ,' + beta',i,sep='')
    }
    Equ<-paste('lm(',Equ,'-1',',data)',sep='')
    # execute a multiple regression using lm()
    regr<-eval(parse(text=Equ))
    # take a look at the estimated regression coefficients and their names
    coef(regr)
    estimate <- coef(regr)
    cov<-vcov(regr)
  }
    # test hypotheses with bain. Note that standardized = FALSE denotes that the
  # hypotheses are in terms of unstandardized regression coefficients
  capture.output(res<-bain::bain(estimate,Hyp1,fraction = J,n=N,Sigma=cov,group_parameters=0,joint_parameters=k), file='NUL')

    bf<-res$fit$Fit[[1]]/res$fit$Com[[1]]

  return(bf)
}
bf0u<-unlist(BF)

set.seed(seed,kind="Mersenne-Twister",normal.kind="Inversion")
bf<-numeric(T)
samph<-rep(N,k)
sampm<-samph/J
beta<-beta1
flag_Hyp<-0
BF<- foreach(ii = 1:T, .packages = c("bain")) %dorng% {
  #  for (ii in 1:T){
  res<- list()
  library(MASS)
  X<-mvrnorm(n=N,mu=numeric(k),Sigma=rho)
  if(flag_Hyp){
    sd<-sqrt(1-R_square1)
  }else{
    sd<-sqrt(1-R_square2)
  }
  epsilon<-rnorm(n=N,mean=0,sd=sd)
  #beta1<-beta2<-beta3<-d
  Y<-  epsilon
  for (i in 1:k)
  {
    Y<-Y+beta[i]*X[,i]
  }
  library(fungible)
  library(stats)


  if(standardize==TRUE){
    # Obtain standardized regression coefficients
    capture.output(int <-fungible:: seBeta(X = X, y = Y, Nobs = N,
                                           estimator = "Normal"), file='NUL')
    # Store the parameter estimates
    estimate <- int$CIs[, 2]
    for (i in 1:k)
    {
      names(estimate)[i]<-paste('beta',i,sep='')
    }
    #names(estimate)<-c('beta1','beta2','beta3')
    # Store the variance covariance matrix for the parameters
    # of interest
    cov <- list(int$cov.mat)
    cov<-matrix(unlist(cov),nrow = k)
  }else{
    M=cbind(X,Y)
    data<-as.list(as.data.frame(M))
    for (i in 1:k)
    {
      names(data)[i]<-paste('beta',i,sep='')
    }
    Equ<-'Y ~ beta1'
    for (i in 2:k){
      Equ<-paste(Equ,' + beta',i,sep='')
    }
    Equ<-paste('lm(',Equ,'-1',',data)',sep='')
    # execute a multiple regression using lm()
    regr<-eval(parse(text=Equ))
    # take a look at the estimated regression coefficients and their names
    coef(regr)
    estimate <- coef(regr)
    cov<-vcov(regr)
  }
  # test hypotheses with bain. Note that standardized = FALSE denotes that the
  # hypotheses are in terms of unstandardized regression coefficients

  capture.output(res<-bain::bain(estimate,Hyp2,fraction = J,n=N,Sigma=cov,group_parameters=0,joint_parameters=k), file='NUL')


  bf<-res$fit$Fit[[1]]/res$fit$Com[[1]]

  return(bf)
}
bf1u<-unlist(BF)

bf11<-bf0u/bf1u


N<-100
flag_Hyp<-1
seed<-10
set.seed(seed,kind="Mersenne-Twister",normal.kind="Inversion")
standardize=FALSE
beta<-beta2
BF<- foreach(ii = 1:T, .packages = c("bain")) %dorng% {
  #  for (ii in 1:T){
  res<- list()
  library(MASS)
  X<-mvrnorm(n=N,mu=numeric(k),Sigma=rho)
  if(flag_Hyp){
    sd<-sqrt(1-R_square1)
  }else{
    sd<-sqrt(1-R_square2)
  }
  epsilon<-rnorm(n=N,mean=0,sd=sd)
  #beta1<-beta2<-beta3<-d
  Y<-  epsilon
  for (i in 1:k)
  {
    Y<-Y+beta[i]*X[,i]
  }
  library(fungible)
  library(stats)


  if(standardize==TRUE){
    # Obtain standardized regression coefficients
    capture.output(int <-fungible:: seBeta(X = X, y = Y, Nobs = N,
                                           estimator = "Normal"), file='NUL')
    # Store the parameter estimates
    estimate <- int$CIs[, 2]
    for (i in 1:k)
    {
      names(estimate)[i]<-paste('beta',i,sep='')
    }
    #names(estimate)<-c('beta1','beta2','beta3')
    # Store the variance covariance matrix for the parameters
    # of interest
    cov <- list(int$cov.mat)
    cov<-matrix(unlist(cov),nrow = k)
  }else{
    M=cbind(X,Y)
    data<-as.list(as.data.frame(M))
    for (i in 1:k)
    {
      names(data)[i]<-paste('beta',i,sep='')
    }
    Equ<-'Y ~ beta1'
    for (i in 2:k){
      Equ<-paste(Equ,' + beta',i,sep='')
    }
    Equ<-paste('lm(',Equ,'-1',',data)',sep='')
    # execute a multiple regression using lm()
    regr<-eval(parse(text=Equ))
    # take a look at the estimated regression coefficients and their names
    coef(regr)
    estimate <- coef(regr)
    cov<-vcov(regr)
  }
  # test hypotheses with bain. Note that standardized = FALSE denotes that the
  # hypotheses are in terms of unstandardized regression coefficients
  capture.output(res<-bain::bain(estimate,Hyp1,fraction = J,n=N,Sigma=cov,group_parameters=0,joint_parameters=k), file='NUL')

  bf<-res$fit$Fit[[1]]/res$fit$Com[[1]]

  return(bf)
}
bf2u<-unlist(BF)




set.seed(seed,kind="Mersenne-Twister",normal.kind="Inversion")
#beta<-c(d,d,d)
bf<-numeric(T)
samph<-rep(N,k)
sampm<-samph/J
beta<-beta2

flag_Hyp<-0
BF<- foreach(ii = 1:T, .packages = c("bain")) %dorng% {
  #  for (ii in 1:T){
  res<- list()
  library(MASS)
  X<-mvrnorm(n=N,mu=numeric(k),Sigma=rho)
  if(flag_Hyp){
    sd<-sqrt(1-R_square1)
  }else{
    sd<-sqrt(1-R_square2)
  }
  epsilon<-rnorm(n=N,mean=0,sd=sd)
  #beta1<-beta2<-beta3<-d
  Y<-  epsilon
  for (i in 1:k)
  {
    Y<-Y+beta[i]*X[,i]
  }
  library(fungible)
  library(stats)


  if(standardize==TRUE){
    # Obtain standardized regression coefficients
    capture.output(int <-fungible:: seBeta(X = X, y = Y, Nobs = N,
                                           estimator = "Normal"), file='NUL')
    # Store the parameter estimates
    estimate <- int$CIs[, 2]
    for (i in 1:k)
    {
      names(estimate)[i]<-paste('beta',i,sep='')
    }
    #names(estimate)<-c('beta1','beta2','beta3')
    # Store the variance covariance matrix for the parameters
    # of interest
    cov <- list(int$cov.mat)
    cov<-matrix(unlist(cov),nrow = k)
  }else{
    M=cbind(X,Y)
    data<-as.list(as.data.frame(M))
    for (i in 1:k)
    {
      names(data)[i]<-paste('beta',i,sep='')
    }
    Equ<-'Y ~ beta1'
    for (i in 2:k){
      Equ<-paste(Equ,' + beta',i,sep='')
    }
    Equ<-paste('lm(',Equ,'-1',',data)',sep='')
    # execute a multiple regression using lm()
    regr<-eval(parse(text=Equ))
    # take a look at the estimated regression coefficients and their names
    coef(regr)
    estimate <- coef(regr)
    cov<-vcov(regr)
  }
  # test hypotheses with bain. Note that standardized = FALSE denotes that the
  # hypotheses are in terms of unstandardized regression coefficients

    capture.output(res<-bain::bain(estimate,Hyp2,fraction = J,n=N,Sigma=cov,group_parameters=0,joint_parameters=k), file='NUL')


  bf<-res$fit$Fit[[1]]/res$fit$Com[[1]]

  return(bf)
}
bfju<-unlist(BF)

bf22<-bfju/bf2u



#output Figure 1
pdf('densityofBF01.pdf',width=8.8/2.54,height=6/2.54)
x<-1:10000
y<-bf11
df<-data.frame(x,y)
library(ggplot2)
p <- ggplot(df, aes(x=y))+geom_density()+geom_vline(aes(xintercept=3),
                                                   linetype="dashed")+labs(x=expression("BF" ["01"]), y = "Density")+
  scale_x_continuous(limits=c(0, 30),breaks=c(0,3,5,10,15,20,25,30))+
  scale_y_continuous(limits=c(0, 0.1),breaks=c(0,0.05,0.1))

d <- ggplot_build(p)$data[[1]]
p+geom_area(data = subset(d, x > 3), aes(x=x, y=y), fill="gray")+ theme_grey(base_size = 10)
dev.off()


pdf('densityofBF10.pdf',width=8.8/2.54,height=6/2.54)
x<-1:10000
y<-bf22
df<-data.frame(x,y)
library(ggplot2)
p <- ggplot(df, aes(x=y))+geom_density()+geom_vline(aes(xintercept=3),
                                                    linetype="dashed")+labs(x=expression("BF" ["10"]), y = "Density")+
  scale_x_continuous(limits=c(0, 30),breaks=c(0,3,5,10,15,20,25,30))+
  scale_y_continuous(limits=c(0, 0.2),breaks=c(0,0.05,0.1,0.15,0.2))

d <- ggplot_build(p)$data[[1]]
p+geom_area(data = subset(d, x > 3), aes(x=x, y=y), fill="gray")+ theme_grey(base_size = 10)
dev.off()






