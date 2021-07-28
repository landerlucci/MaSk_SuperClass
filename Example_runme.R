source("MaSk_function.R")

x<-iris[,-5]
y<-ifelse(iris[,5]=="setosa",1,0)


# Under-Sketching (with Hadamard matrices) ####
out<-MaSk(x,y,N=2*sum(y==1),pi1=0.5,type="Hadamard")

table(out$y)
out$type


# Balanced-Sketching, with each class being twice the minority one (with Hadamard matrices) ####
out<-MaSk(x,y,N=4*sum(y==1),pi1=0.5,type="Hadamard")

table(out$y)
out$type


# Over-Sketching (with Hadamard matrices) ####
out<-MaSk(x,y,N=2*sum(y==0),pi1=0.5,type="Hadamard")

table(out$y)
out$type


# Graphical representation of the sketched data ####
library(ggplot2)

df.plot<-data.frame(X=rbind(x,out$x),Data=factor(rep(c('Original','Rebalanced'),c(nrow(x),nrow(out$x))),levels=c('Original','Rebalanced'),labels=c('Original','Rebalanced')),Class=factor(c(y,out$y),levels=c(0,1),labels=c("Setosa","Vers+Virg")))

ggplot(df.plot, aes(x=X.Petal.Length, y=X.Petal.Width, group=Class,shape=Data)) +  
  geom_point(aes(col = Class))+   
  theme_bw() + scale_color_manual(values=c("black", "red"))+ 
  scale_shape_manual(values=c(18,6)) + 
  labs(x="Petal Length",y="Petal Width",title ="Iris Data",
       subtitle = "'Setosa' vs. 'Versicolor + Virginica'")

