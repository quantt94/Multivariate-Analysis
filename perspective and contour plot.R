#Question 5 - import data from Bird data 
z=rep(c(1,2),c(21,28))
bird=cbind(birddata,z)
pairs(bird[1:5], main="Bird Data (blue plus = survivors, red circle = non-survivors)", pch=c(1,3)[unclass(bird$z)], col=c("blue","red")[unclass(bird$z)])
stars(bird[,1:5], main="Stars plot of Bird Data")
library(aplpack)
faces(bird[,1:5], main="Faces plot of Bird Data")

#Question 6 - confirm Q2
#I create 1 more observation in order the standardize the range of height and weight
#Let assume the third observation with not right handed, playing piano, grey hair and 26, 41 for height and weight
x=c("Y","Y","N")
y=c(0,0,1)
color=c("Grey","Brown","Grey")
height=c(14,2,26)
weight=c(23,5,41)
z=data.frame(RightHanded=x,PlayPiano=y,HairColor=color,Height=height,Weight=weight)
library(cluster)
daisy(z,metric="gower", type=list(asymm=2,symm=1))

#Question 7 - draw perspective plot and contour
library(mnormt) 
mu=c(0,2)
sigma=matrix(c(2,(sqrt(2)/2),(sqrt(2)/2),1),2,2)
len=100
x=seq(mu[1]-5*sqrt(sigma[1,1]),mu[1]+5*sqrt(sigma[1,1]),length=len)
y=seq(mu[2]-5*sqrt(sigma[2,2]),mu[2]+5*sqrt(sigma[2,2]),length=len)
temp=as.matrix(expand.grid(x,y)) 
zvec=dmnorm(temp,mu,sigma)
z=matrix(zvec,len,len,byrow=T)

#draw the perspective plot
persp(x,y,z, main="3-D perspective plot of Question 3")

#sketch the 50% of probability contour
csq=qchisq(0.5,2)
lev=1/(2*pi*sqrt(det(sigma)))*exp(-csq/2)
contour(x,y,z, xlab="X1", ylab="X2",levels=lev, main="50% of probability contour") 
#perform the point of theorem onto the contour sketch
obj=eigen(sigma)
c=sqrt(csq)
a1=mu-c*sqrt(obj$values[1])*obj$vectors[,1]
a2=mu+c*sqrt(obj$values[1])*obj$vectors[,1]
b1=mu-c*sqrt(obj$values[2])*obj$vectors[,2]
b2=mu+c*sqrt(obj$values[2])*obj$vectors[,2]
points(rbind(a1,a2,b1,b2))
