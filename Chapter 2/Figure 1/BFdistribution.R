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

m1<-0
m2<-0
sd1<-1
sd2<-1
N<-26
J<-1
N_SIM<-10000
Variances<-'equal'
power<-0.8


N1<-N2<-N
samph<-c(N1,N2)
ngroup<-samph/J
Error<-list()
#simulate data
set.seed(10)
datalist<-list()
for(i in 1:N_SIM){
  datalist[[i]]<-cbind(rnorm(N1,m1,sd1),rnorm(N2,m2,sd2))
}

##caculate estimate and variance for group mean
estimate<-lapply(datalist, function(x) apply(x,2,mean))
variance<-lapply(datalist, function(x) apply(x,2,function(x) as.matrix(var(x)/N1) ) )


##caculate bf
library(bain)
bf<-c()

#parallel execution,execute those repeated operations on multiple processors/cores on your computer.
BF<- foreach(i = 1:N_SIM) %dorng% {
  library(bain)
  names(estimate[[i]]) <- c("mu1","mu2")
  variance_sp<-(variance[[i]][1]+variance[[i]][2])/2
  covlist<-list(matrix(variance_sp),matrix(variance_sp))
  res<-bain(estimate[[i]], n=ngroup, "mu1 = mu2",Sigma=covlist,group_parameters=1,joint_parameters = 0)
  bf11<-res$fit$BF[1]
  return(bf11)
}
bf11<-unlist(BF)







##simulate data
##Repeat the same simulation results

m1<-0.5
m2<-0
sd1<-1
sd2<-1
N<-104
J<-1
N_SIM<-10000
Variances<-'equal'
power<-0.8
N1<-N2<-N
samph<-c(N1,N2)
ngroup<-samph/J
Error<-list()
#simulate data
set.seed(10)
datalist<-list()
for(i in 1:N_SIM){
  datalist[[i]]<-cbind(rnorm(N1,m1,sd1),rnorm(N2,m2,sd2))
}

##caculate estimate and variance for group mean
estimate<-lapply(datalist, function(x) apply(x,2,mean))
variance<-lapply(datalist, function(x) apply(x,2,function(x) as.matrix(var(x)/N1) ) )


##caculate bf
library(bain)
bf<-c()

# for(i in 1:N_SIM){
BF<- foreach(i = 1:N_SIM) %dorng% {
  library(bain)
  names(estimate[[i]]) <- c("mu1","mu2")
  variance_sp<-(variance[[i]][1]+variance[[i]][2])/2
  covlist<-list(matrix(variance_sp),matrix(variance_sp))
  res<-bain(estimate[[i]], n=ngroup, "mu1 = mu2",Sigma=covlist,group_parameters=1,joint_parameters = 0)
  bf22<-1/res$fit$BF[1]
  return(bf22)
}
bf22<-unlist(BF)


#output the figure
pdf('densityofBF01.pdf',width=8.8/2.54,height=6/2.54)
x<-1:10000
y<-bf11
df<-data.frame(x,y)
library(ggplot2)
p <- ggplot(df, aes(x=y))+geom_density()+geom_vline(aes(xintercept=3),
                                                   linetype="dashed")+labs(x=expression("BF" ["01"]), y = "Density")+
  scale_x_continuous(limits=c(0, 18),breaks=c(0,3,5,10,15,18))+
  scale_y_continuous(limits=c(0, 0.4),breaks=c(0,0.1,0.2,0.3,0.4))

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
  scale_y_continuous(limits=c(0, 0.15),breaks=c(0,0.05,0.1,0.15))

d <- ggplot_build(p)$data[[1]]
p+geom_area(data = subset(d, x > 3), aes(x=x, y=y), fill="gray")+ theme_grey(base_size = 10) +
  geom_curve(aes(x = 10, y = 0.02, xend = 15, yend = 0.05),
             colour = "#FF0000",
             size=0.5,
             curvature = -0.2,
             arrow = arrow(length = unit(0.03, "npc"))) +
  geom_label(aes(x = 10, y = 0.02, label = "Here is the\nUnicode symbol"),
             hjust = 0,
             vjust = 0.5,
             colour = "#FAAB18",
             fill = "white",
             label.size = NA,
             family="Helvetica",
             size = 6)
dev.off()






