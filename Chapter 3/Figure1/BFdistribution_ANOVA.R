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
varnames<-c('mu1','mu2','mu3')
hyp1<-"mu1=mu2=mu3"
hyp2<-"mu1>mu2>mu3"
BFthresh<-3
type<-'equal'
T=100000
f1<-0
f2<-0.25
EI_matrix1<-list()
EI_matrix1<-matrix_trans_anova(varnames,hyp1)
ERr1<-EI_matrix1[[1]]
IRr1<-EI_matrix1[[2]]
EI_matrix2<-list()
EI_matrix2<-matrix_trans_anova(varnames,hyp2)
ERr2<-EI_matrix2[[1]]
IRr2<-EI_matrix2[[2]]
k<-3
#calculate mu based on the effect size f
# mu<-cal_mu_anova(k,f1,f2,hyp1,hyp2,ERr1,ERr2)
# m1<-mu[[1]]
# m2<-mu[[2]]
m1<-c(0,0,0)
m2<-c(0.6124, 0.3062, 0.0000)
sd<-c(1,1,1)
J<-1
m<-m1
flag_output=0
flag_fast=0
seed=10
N<-18
#when H0 is true
#calculate the Bayes factor BF0u
bf0u_H0<-cal_bf_anova(N,m,sd,T,J,varnames,hyp1,flag_output,flag_fast,type,hyp2,seed)
#calculate the Bayes factor BF1u
bfju_H0<-cal_bf_anova(N,m,sd,T,J,varnames,hyp2,flag_output=0,flag_fast,type,hyp2,seed)
#calculate the Bayes factor BF01
bf0j_H0<-bf0u_H0/bfju_H0

N<-93
m<-m2
#when H1 is true
#calculate the Bayes factor BF0u
bf0u_Hj<-cal_bf_anova(N,m,sd,T,J,varnames,hyp1,flag_output=0,flag_fast,type,hyp2,seed)
#calculate the Bayes factor BF1u
bfju_Hj<-cal_bf_anova(N,m,sd,T,J,varnames,hyp2,flag_output=0,flag_fast,type,hyp2,seed)
#calculate the Bayes factor BF10
bfj0_Hj<-bfju_Hj/bf0u_Hj

#output the figure
pdf('densityofBF01.pdf',width=8.8/2.54,height=6/2.54)
x<-1:100000
y<-bf0j_H0
df<-data.frame(x,y)
library(ggplot2)
p <- ggplot(df, aes(x=y))+geom_density()+geom_vline(aes(xintercept=3),
                                                   linetype="dashed")+labs(x=expression("BF" ["01"]), y = "Density")+
  scale_x_continuous(limits=c(0, 100),breaks=c(0,3,10,20,30,40,50,60,70,80,90,100))+
  scale_y_continuous(limits=c(0, 0.05),breaks=c(0,0.01,0.02,0.03,0.04,0.05))

d <- ggplot_build(p)$data[[1]]
p+geom_area(data = subset(d, x > 3), aes(x=x, y=y), fill="gray")+ theme_grey(base_size = 10)
dev.off()


pdf('densityofBF10.pdf',width=8.8/2.54,height=6/2.54)
x<-1:100000
y<-bfj0_Hj
df<-data.frame(x,y)
library(ggplot2)
p <- ggplot(df, aes(x=y))+geom_density()+geom_vline(aes(xintercept=3),
                                                    linetype="dashed")+labs(x=expression("BF" ["10"]), y = "Density")+
  scale_x_continuous(limits=c(0, 30),breaks=c(0,3,5,10,15,20,25,30))+
  scale_y_continuous(limits=c(0, 0.2),breaks=c(0,0.05,0.1,0.15,0.2))

d <- ggplot_build(p)$data[[1]]
p+geom_area(data = subset(d, x > 3), aes(x=x, y=y), fill="gray")+ theme_grey(base_size = 10)
dev.off()






