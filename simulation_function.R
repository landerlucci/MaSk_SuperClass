# Simulation Study ####
#
# n: sample size [2000]
# p: no. of features [10]
# asym: TRUE/FALSE (asymmetric Exp-Gaussian, default=FALSE)
# homoscedasticity: TRUE/FALSE (default=TRUE)
# pi1: the fraction of units from the minority class, pi1=n1/n [0.25, 0.10, 0.05]
# delta: separation shift [0.50, 0.25, 0.10]
# seed: seed for the random generation process

library(mvtnorm)
library(clusterGeneration) # for the generation of the correlation matrix

sim_generation<-function(n=2000,p=10,asym=FALSE,homoscedasticity=TRUE,pi1=0.05,delta=0.50,seed=2405){
  set.seed(seed)
  sigma<-rcorrmatrix(p)
  
  n1<-n*pi1
  n0<-n*(1-pi1)
  
  if (homoscedasticity=TRUE) {
  set.seed(seed)
  x0<-rmvnorm(n0,mean=rep(0,p),sigma = sigma)
  x1<-rmvnorm(n1,mean = rep(delta,p),sigma = sigma)
  } else {
    set.seed(seed)
    x0<-rmvnorm(n0,mean=rep(0,p))
    x1<-rmvnorm(n1,mean = rep(delta,p),sigma = sigma)
    }
  
  if (asym=TRUE){
    x0<-exp(x0)
    x1<-exp(x1)+delta
  }
  
  x<-rbind(x0,x1)
  y<-rep(c(0,1),c(n0,n1))
  
  return(list(x=x,y=y))
}

