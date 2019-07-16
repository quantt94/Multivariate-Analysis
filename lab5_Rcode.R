#Stat 372, Lab 5, Feb.8, 2016

################################################################################
#Topics covered:
#  1. Principal Component Analysis
#  2. Factor Analysis
################################################################################

#1. PCA for the Birds data.
bird=read.table("/Users/wanhuasu/Documents/MacEwan/stat372/winter2016/lab/bumpus.txt",header=T)
?prcomp #check the built-in function

(obj=prcomp(birddata[],center=T,scale=T,tol=0.001))
#center=T: transform the data to have mean 0; scale=T: to have standard deviation 1. 
#Conduct PCA on the correlation matrix

(names(obj)) #values are given as an object
(obj$sdev^2) #will give you the eigenvalues, because lambda= variance of Y1

#confirm the first PC is the unit eigenvector corresponding to the largest eigenvalue.
(eobj=eigen(cor(birddata)))
#Note: signs might be different given by different function or software

(pc=obj$x) #the loading on each PC for all observations

#verify the loadings
(pcmat=obj$rotation) #get the coefficients
(cormat=scale(birddata)) #standardize the data to have 0 (means x-mean/stdv) and standard deviation 1
((mypc1=cormat%*%pcmat[,1])) #the first PC, should be the same as pc[,1]

#index scatter plot on the first 2 PCs
plot(pc[,1],pc[,2],pch=rep(c(1,3),c(21,28)), col=rep(c("black","red"),c(21,28)),xlab="First PC", ylab="Second PC",cex.lab=1.3) 

#determine the number of PCs
evalue=obj$sdev^2
(pvec=cumsum(evalue)/sum(evalue)) #adding the eigenvalues and perform the proportion
plot(1:length(pvec),pvec,xlab="Number of PC",ylab="Cumulative Proportion of Variance",ylim=c(0,1),cex.lab=1.3,pch=19)
(cor(birddata))

#2 Factor Analysis
#a) Find the initial loading using the principal component method 
#assign the correlation matrix
(mat=matrix(c(1,0.63,0.45,0.63,1,0.35,0.45,0.35,1),3,3))
install.packages("psych") #load the package
library(psych)
#get the initial loading without rotation using principal component method with one common factor
(fitpa=factor.pa(mat, nfactors=1,rotate="none"))

#get the initial loading without rotation using MLE method with one common factor
(fitmle=fa(mat,nfactors=1,rotate="none"))
 

(m1=fa(mat, nfactors=1,rotate="none",fm="pa"))
(m2=fa(mat, nfactors=1,rotate="none",fm="ml"))
cbind(m1$loadings,m2$loadings)

#understand the procedure by principal component method
(eobj=eigen(mat)) #find the eigenvalue and eigenvectors of correlation matrix
(myload=sqrt(eobj$values[1])*eobj$vectors[,1]) #if m=1, loading is a column vector.

(phi0=diag((1-myload^2),3,3)) #use as initial
myphi=phi0
nrun=100
for (i in 1:nrun){
  newmat=mat-myphi #obtain the adjusted correlation matrix
  neobj=eigen(newmat) #find the eigenvalues and vectors for the updated correlation matrix
  newload=sqrt(neobj$values[1])*neobj$vectors[,1] #update the loading
  myphi=diag((1-newload^2),3,3) #obtain the updated phi matrix, repeat until the matrix won't change.
}
myphi
newload #the same as the MLE approach.

#Compare the resulting loading of principal component method and maximum likelihood method.

cbind(fitpa$loading,fitmle$loading) 
#very close for this example, but not necessarily to be true.

#Compare the result before and after factor rotation. 
install.packages("ade4")
library(ade4) #load the package
data(olympic)  #get the data set, it is an object
(dmat=olympic$tab) #get the results of 33 men
round(cor(dmat),2) #show the correlation matrix
(eobj=eigen(cor(dmat))) #find eigenvalues and vectors

#we could try two to three common factors.
#obtain the loading before rotation using principal component method
(fitpa0=fa(dmat, nfactors=2,rotate="none",fm="pa"))
#loading before rotation using MLE method
(fitmle0=fa(dmat, nfactors=2,rotate="none",fm="ml"))

round(cbind(fitpa0$loading,fitmle0$loading),3) #compare the loading before rotation

#obtain the loading after rotation using principal component method
(fitpa1=fa(dmat, nfactors=2,rotate="varimax",fm="pa"))
#loading after rotation using MLE method
(fitmle1=fa(dmat, nfactors=2,rotate="varimax",fm="ml"))

round(cbind(fitpa1$loading,fitmle1$loading),3) #compare the loading after rotation

#you might want to try three common factors
(fitpa3=fa(dmat, nfactors=3,rotate="varimax",fm="pa"))
(fitmle3=fa(dmat, nfactors=3,rotate="varimax",fm="ml"))
round(cbind(fitpa3$loading,fitmle3$loading),3)
fitmle3$loadings
fitmle0$loadings
