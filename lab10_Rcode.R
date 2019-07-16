#Stat 372, Lab 10, March 21, 2016

################################################################################
#Topics covered:
#  1. Multidimentional Scaling
#  2. Calculate canonical variates and their correlations
################################################################################
#  1. Multidimentional Scaling
#Classical (Metric) Multidimensional Scaling
loc=cmdscale(eurodist) 
x=loc[,1]
y=-loc[,2]
plot(x,y,type="n",xlab="",ylab="",asp=1)
text(x,y,rownames(loc),cex=0.8)

#Kruskal's Non-metric Multidimensional Scaling
library(MASS)
fit=isoMDS(eurodist,k=2)
x=fit$points[,1]
y=-fit$points[,2]
plot(x,y,type="n",xlab="",ylab="")
text(x,y,rownames(loc),cex=0.8)

#  2. Calculate canonical variates and their correlations
library(MASS)
s11=matrix(c(100,0,0,1),2,2)
s22=matrix(c(1,0,0,100),2,2)
s12=matrix(c(0,0.95,0,0),2,2)
s21=t(s12)

A=solve(s11)%*%s12%*%solve(s22)%*%s21

obj1=eigen(A)
ea=obj1$vector[,1]
p=sqrt(max(obj1$values))
ca=t(ea)%*%s11%*%ea
a=ea/sqrt(ca)
eb=p*ginv(s12)%*%s11%*%a
cb=t(eb)%*%s22%*%eb
b=eb/sqrt(cb[1,1])

# Example 10.1 in Johnson and Wichern 
s11=matrix(c(1,0.4,0.4,1),2,2)
s12=matrix(c(0.5,0.3,0.6,0.4),2,2)
s22=matrix(c(1,0.2,0.2,1),2,2)
s21=t(s12)

A=solve(s11)%*%s12%*%solve(s22)%*%s21

obj1=eigen(A)
ea=obj1$vector[,1]
p=sqrt(max(obj1$values))
ca=t(ea)%*%s11%*%ea
a=ea/sqrt(ca)
eb=p*ginv(s12)%*%s11%*%a
cb=t(eb)%*%s22%*%eb
b=eb/sqrt(cb[1,1])
cbind(a,b)

#we can used another method presented in Johnson and Wichern
s1obj=eigen(s11)
s1root=s1obj$vectors%*%sqrt(diag(s1obj$values))%*%t(s1obj$vectors)

s2obj=eigen(s22)
s2root=s2obj$vectors%*%sqrt(diag(s2obj$values))%*%t(s2obj$vectors)

mat1=solve(s1root)%*%s12%*%solve(s22)%*%s21%*%solve(s1root)
m1obj=eigen(mat1)
atilde=solve(s1root)%*%m1obj$vectors[,1]

mat2=solve(s2root)%*%s21%*%solve(s11)%*%s12%*%solve(s2root)
m2obj=eigen(mat2)
btilde=solve(s2root)%*%m2obj$vectors[,1]
cbind(atilde, btilde)


