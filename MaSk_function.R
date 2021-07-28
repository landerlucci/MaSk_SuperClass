# Main function ####
# x: data matrix.
# y: vector of class labels, coded as 0 for majority class and as 1 for minority class.
# N: desired overall sample size; by default is the original sample size.
# pi1: desired fraction of units from the minority class; by default is 0.5 (perfect balance).
# type: type of matrix sketching; one among "Gaussian" (default), "CW" (Clarkson-Woodruff), "Hadamard".

# Returned output: a list
## x: the sketched data matrix;
## y: the vector of class labels for the sketched data;
## N: sample size of the sketched data; 
## pi1: desired fraction of units from the minority class;
## type: type of matrix sketching.
MaSk<-function(x,y,N=NULL,pi1=0.5,type="Gaussian"){
  x0<-x[y==0,]
  x1<-x[y==1,]
  X0<-scale(x0,T,F)
  X1<-scale(x1,T,F)
  mean.x0<-colMeans(x0)
  mean.x1<-colMeans(x1)
  n0<-nrow(x0)
  n1<-nrow(x1)
  p<-ncol(x)
  if (is.null(N)) N<-length(y)
  k1<-round(pi1*N)
  k0<-N-k1
  if (type=='Gaussian'){
    S0<-matrix(rnorm(prod(n0,k0),sd=1/sqrt(k0)),k0,n0)
    S1<-matrix(rnorm(prod(n1,k1),sd=1/sqrt(k1)),k1,n1)
  }  else if (type=='CW'){
    S0<-clark.sk(n0,k0)
    S1<-clark.sk(n1,k1)
  } else if (type=="Hadamard"){
    S0<-hada.sk(n0,k0)
    S1<-hada.sk(n1,k1)
    X0<-rbind(X0,matrix(0,nrow=zero.row(n0),ncol=p))
    X1<-rbind(X1,matrix(0,nrow=zero.row(n1),ncol=p))
  }
  x0.tilde.star=sqrt(k0/n0)*(S0%*%X0)+matrix(mean.x0,k0,ncol=p,byrow=T)
  x1.tilde.star=sqrt(k1/n1)*(S1%*%X1)+matrix(mean.x1,k1,ncol=p,byrow=T)
    
  x.tilde.star<-as.data.frame(rbind(x0.tilde.star,x1.tilde.star))
  y.star<-c(rep(0,k0),rep(1,k1))
    
return(list(x=x.tilde.star,y=y.star,pi1=pi1,N=N,type=type))    
}

clark.sk<-function(n,k){
  require(extraDistr)
  I<-matrix(0,k,n)
  indice<-sample(1:k,n,replace=ifelse(n<k,FALSE,TRUE))
  for (i in 1:n) I[indice[i],i]<-rsign(1)
  return(I)
}


hada.sk<-function(n,k){
  require(extraDistr)
  require(pracma)
  if (zero.row(n)!=0) n<-n+zero.row(n)
  D<-rsign(n)
  quali.neg<-which(D<0)
  quali<-sample(1:n,k,replace=ifelse(n<k,TRUE,FALSE))
  IHD<-hadamard(n)[quali,]
  IHD[,quali.neg]<--IHD[,quali.neg]
  return(1/sqrt(k)*IHD)
}

# Function that returns the no. of zero rows to add in order to complete the Hadamard matrix
zero.row<-function(n){  
    if (n %in% c(1,3,5,6,9,10))
    return(12-n)
  esponente=ceiling(log2(c(n,n/12,n/20)))
  xx=c(1,12,20)*2^esponente
  return(min(xx-n))
}
