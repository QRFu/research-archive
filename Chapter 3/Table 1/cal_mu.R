
#calculate means for population in which Ha is true.
varnames<-c('mu1','mu2','mu3')
hyp1<-"mu1=mu2=mu3"
hyp2<-"Ha"
BFthresh<-3
type<-'equal'
T=10000
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
#calculate mu
mu<-cal_mu_anova(k,f1,f2,hyp1,hyp2,ERr1,ERr2)
m1<-mu[[1]]
m2<-mu[[2]]

#calculate means for population in which H1: mu1>mu2>mu3 is true.
varnames<-c('mu1','mu2','mu3')
hyp1<-"mu1=mu2=mu3"
hyp2<-"mu1>mu2>mu3"
BFthresh<-3
type<-'equal'
T=10000
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
#calculate mu
mu<-cal_mu_anova(k,f1,f2,hyp1,hyp2,ERr1,ERr2)
m1<-mu[[1]]
m2<-mu[[2]]

#calculate means for population in which H2: mu2>mu3>mu1 is true.
varnames<-c('mu1','mu2','mu3')
hyp1<-"mu1=mu2=mu3"
hyp2<-"mu1>mu2>mu3"
BFthresh<-3
type<-'equal'
T=10000
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
#calculate mu
mu<-cal_mu_anova(k,f1,f2,hyp1,hyp2,ERr1,ERr2)
m1<-mu[[1]]
m2<-mu[[2]]

#the means for population in which the complement hypothesis of H1 is true, 
#can be determined based on the order of the representative hypothesis (see Appendix C)