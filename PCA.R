###Work for question 4 and supported calculation in written assignment

#Question 1
#built-in function
x1=matrix(c(6,6,18,8,11,34,28,71,43,33,20,27,23,64,44,30,75,26,124,54,30,14),11,2,byrow=F)
x2=matrix(c(25,28,36,35,15,44,42,54,34,29,39,15,13,22,29,31,64,30,64,56,20,21),11,2,byrow=F)
dif=x1-x2
dif
library(ICSNP)
HotellingsT2(dif, mu = c(0,0))

#do it directly (calculation for written assignment)
p=ncol(dif)
n=nrow(dif)
(mud=matrix(apply(dif,2,mean),p,1))
(mu0=matrix(c(0,0),p,1))
(coma=cov(dif))
(t0=n*(n-p)/(p*(n-1))*t(mud-mu0)%*%solve(coma)%*%(mud-mu0))
(pvalue=pf(t0,p,n-p,lower.tail=F))

#Question 2
#built-in function
(x1=matrix(c(6,5,8,4,7,7,9,6,9,9),5,2,byrow=F))
(x2=matrix(c(3,1,3,3,6,3),3,2,byrow=F))
(HotellingsT2(x1,x2))

#do it directly (calculation for written assignment)
(mu1=matrix(apply(x1,2,mean),2,1))
(mu2=matrix(apply(x2,2,mean),2,1))
(n1=nrow(x1))
(n2=nrow(x2))
(S1=cov(x1))
(S2=cov(x2))
(Sp=((n1-1)*S1+(n2-1)*S2)/(n1+n2-2))
(z=((1/n1)+(1/n2))*Sp)
(z1=solve(z))
(T2=t(mu1-mu2)%*%z1%*%(mu1-mu2))
(t0=((n1+n2-2-1)/((n1+n2-2)*2))*T2)
(pval=pf(t0,2,5,lower.tail=F))

#Question 3
#built-in function
(x1=matrix(c(6,5,8,4,7,7,9,6,9,9),5,2,byrow=F))
(x2=matrix(c(3,1,3,3,6,3),3,2,byrow=F))
(x3=matrix(c(2,5,3,2,3,1,1,3),4,2,byrow=F))
(x=rbind(x1,x2,x3))
n1=nrow(x1)
n2=nrow(x2)
n3=nrow(x3)
library(rrcov)
gr=as.factor(rep(c(1,2,3),c(n1,n2,n3)))
Wilks.test(x,gr)

#do it directly (calculation for the written assignment)
(xbar=matrix(apply(x,2,mean),2,1))
(x1bar=matrix(apply(x1,2,mean),2,1))
(x2bar=matrix(apply(x2,2,mean),2,1))
(x3bar=matrix(apply(x3,2,mean),2,1))
s1=cov(x1)
s2=cov(x2)
s3=cov(x3)

(B=n1*(x1bar-xbar)%*%t(x1bar-xbar)+n2*(x2bar-xbar)%*%t(x2bar-xbar)+n3*(x3bar-xbar)%*%t(x3bar-xbar))
(W=(n1-1)*s1+(n2-1)*s2+(n3-1)*s3)

(lambda=(det(W))/(det(B+W)))

n=n1+n2+n3
p=2
k=3
(chisq=-(n-1-((p+k)/2))*log(lambda,base=exp(1)))

###Question 5
#firstly, I did import dataset name "mutagen"
(obj=prcomp(mutagen,center=T,scale=T,tol=0.001))
(evalue=obj$sdev^2)
(pvec=cumsum(evalue)/sum(evalue))
#in order to capture 90% of variation, we should take up to 15 PCs

#b - perform a scatter plot based on the first 2 principal components
pc=obj$x
plot(pc[,1],pc[,2],main="Scatter Plot based on the first two PCs",xlab="First PC ",ylab="Second PC",cex.lab=1.3)

###Question 6
#import stock-price data
library(psych)
##(a)
#Principal component solution and Maximum likelikhood solution without rotation
(m1=fa(T8.4, nfactors=1,rotate="none",fm="pa"))
(m2=fa(T8.4, nfactors=1,rotate="none",fm="ml"))
cbind(m1$loadings,m2$loadings)

#Principal component solution and Maximum likelihood solution with rotation
(m3=fa(T8.4, nfactors=1,rotate="varimax",fm="pa"))
(m4=fa(T8.4, nfactors=1,rotate="varimax",fm="ml"))
cbind(m3$loadings,m4$loadings)

##(b)
#Principal component solution and Maximum likelikhood solution without rotation
(m1=fa(T8.4, nfactors=2,rotate="none",fm="pa"))
(m2=fa(T8.4, nfactors=2,rotate="none",fm="ml"))
cbind(m1$loadings,m2$loadings)

#Principal component solution and Maximum likelihood solution with rotation
(m3=fa(T8.4, nfactors=2,rotate="varimax",fm="pa"))
(m4=fa(T8.4, nfactors=2,rotate="varimax",fm="ml"))
cbind(m3$loadings,m4$loadings)

##(c)
#do the standardized cov
(eobj=eigen(cor(T8.4)))
#the most proper number of common factors is 2
#the first and second value are greater than 1
#the third one is not even closed to 1
#so we repeat the function in (b) and get the outcome

###Question 7
mymvector=function(mat){
  n=nrow(mat)
  mvector=(1/n)*t(mat)%*%matrix(1,n,1)
  return(mvector)
}


mycov=function(mat){
  n=nrow(mat)
  u=mat-(1/n)*matrix(1,n,n)%*%mat
  covmat=(1/(n-1))*t(u)%*%u
  return(covmat)
}

