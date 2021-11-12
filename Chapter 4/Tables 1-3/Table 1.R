
##H0: beta1=beta2=0 vs Ha for rho=0 (rho=0.2, rho=0.5)
k=2
R_square1=0
R_square2=0.13
Hyp1<-'beta1= beta2=0'
Hyp2<-'Ha'
ratio<-c(1,1)
rho<-matrix(data=c(1,0,0,1),nrow=2)
#rho<-matrix(data=c(1,0.2,0.2,1),nrow=2)
#rho<-matrix(data=c(1,0.5,0.5,1),nrow=2)
beta<-cal_beta(k,R_square1,R_square2,Hyp1,Hyp2,ratio,rho)
beta1<-beta[[1]]
beta2<-beta[[2]]
print(beta1)
print(beta2)

##H0: beta1=beta2=0 vs beta1>0&beta2>0 for rho=0 (rho=0.2, rho=0.5)
k=2
R_square1=0
R_square2=0.13
Hyp1<-'beta1=beta2=0'
Hyp2<-'beta1>0&beta2>0'
ratio<-c(1,1)
rho<-matrix(data=c(1,0,0,1),nrow=2)
#calculate beta
beta<-cal_beta(k,R_square1,R_square2,Hyp1,Hyp2,ratio,rho)
beta1<-beta[[1]]
beta2<-beta[[2]]
print(beta1)
print(beta2)

##H0: beta1=beta2 vs beta1>beta2 for rho=0 (rho=0.2, rho=0.5)
k=2
R_square1=0.13
R_square2=0.13
Hyp1<-'beta1=beta2'
Hyp2<-'beta1>beta2'
ratio<-c(2,1)
rho<-matrix(data=c(1,0,0,1),nrow=2)
beta<-cal_beta(k,R_square1,R_square2,Hyp1,Hyp2,ratio,rho)
beta1<-beta[[1]]
beta2<-beta[[2]]
print(beta1)
print(beta2)

#beta1, beta2 for population in which the complement hypothesis
#is true can be determined based on the representative hypothesis. (See APPENDIX B)



