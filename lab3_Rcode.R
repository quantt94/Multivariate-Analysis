#Stat 372, Lab 3, Jan 25, 2016

################################################################################
#Topics covered:
#  1. Distances
#  2. More about plots
#  3. Generate random numbers from a given distribution
#  4. Multivariate normal distribution
#  5. Surface plot and contour plot
################################################################################


#1. Distance for quantitative variables (should be in row vectors)
x=c(60,50,70)
y=c(80,60,90)
dist(rbind(x,y),method="euclidean") #the input should be a numeric matrix
?dist

dist(rbind(x,y),method="manhattan")
dist(rbind(x,y),method="minkowski",p=1)#the same as Manhattan
dist(rbind(x,y),method="minkowski",p=2)#the same as Euclidean

#Distance for categorical variables
## example of BINARY and canberra distances
#try to google canberra for the usage
x = c(0, 0, 1, 1, 1, 1)
y =c(1, 0, 1, 1, 0, 1)
dist(rbind(x,y), method= "binary") #they minus the zero-pair ## number of (0,1) and (1,0) pairs divided by # of (1,1) # of (0,1) # of (1,0)

#distance between observations with categorical measurements 
x=c("F","M", "M")
y=c("Brown","Grey","Brown")
z=data.frame(gender=x,haircolor=y)
library(cluster)
daisy(z,metric = "gower")
# N- stand for nominal (categorical)

#distance for mixed data types.
height=c(60,50,70)
weight=c(80,60,90)
x=c("F","M", "M")
y=c("Brown","Grey","Brown")
z=data.frame(gender=x,haircolor=y,height=height,weight=weight)
library(cluster)
daisy(z,metric = "gower")
# I- stand for numerical (intervals)

#add a unbalanced binary variable
asian=c(1,0,0)
z=data.frame(gender=x,haircolor=y,race=asian,height=height,weight=weight)
daisy(z,metric = "gower")

asian=c(1,0,0)
z=data.frame(gender=x,haircolor=y,race=asian,height=height,weight=weight)
daisy(z,metric = "gower", type=list(asymm =3, symm=c(1)))
##the first col is balanced, the 3rd col. is asymmetric (unbal.) - you should let R knows whether they re bal. or not
#specify symmetric and asymmetric binary

#2. More about plot
#2-d plot: two vectors, specify point type, line type, color for each point
plot(iris[,2],iris[,4],col=c("red","black","green")[unclass(iris$Species)],
     pch=c(1,3,4)[unclass(iris$Species)],xlab="Sepal Width",ylab="Petal Width",
     main="2-D Scatter Index Plot")
legend(3.9,2.5,pch=c(1,3,4),col=c("red","black","green"),c("Setosa","Versicolor","Virginical"))

c("red","black","green")[unclass(iris$Species)]
unclass(iris$Species)


#3. Generate simple random sample from a population
#consider the Iris data, randomly pick 12 flowers from each species
set.seed(107) #set the seed to make sure you can reproduce the result
tind=c(sample(1:50,12),sample(51:100,12),sample(101:150,12))#randomly pick 12 flowers from each species
ind=sample(tind,36) #permute the order of the sample
iris[ind,-1]

?rnorm #generate random numbers from normal distributions
mu=0
std=1
n=1000 #when your data is small, use qq-plot to see whether it is normal or not
set.seed(107) #set the random seed
x=rnorm(n,mu,std) #generate 1000 random numbers from N(0,1)
hist(x,prob=T) #draw the histogram of the random number, bell-shape?
yhat=density(x,bw=0.5) #obtained the estimated density of x (impose density curve on the top of histogram)
lines(yhat) #add the density curve on the top of the histogram (get the estimate of the density and put it on the top of histogram)

#calculate the density
x=seq(-4,4,by=0.1) #generate a sequence from -4 to 4, increase by 0.1 each time
y=dnorm(x) #calculate the density of x - density of normal distribution
plot(x,y,type="l") #draw the density curve

#about t-distribution (d:density, q:quantile, r:random number generator, p:cdf)
?rt

#4. for multivariate normal
library(mnormt) 
mu=c(0,0) #set the mean vector
sigma=matrix(c(1,0,0,1),2,2) #covariance matrix  
#sigma=matrix(c(1,0.75,0.75,1),2,2)                            
x=seq(-4,4,0.1) #generate the grids of x, and for each point on the grid, you calculate density function f(vector x)
y=seq(-4,4,0.1) #generate the grids of y
len=length(x) #find the number of observations of x
temp=as.matrix(expand.grid(x,y)) #generate the grids of x and y pairs
zvec=dmnorm(temp,mu,sigma) #calculate the density of f(x, y)
z=matrix(zvec,len,len,byrow=T) #rearrange the value of f(x, y) in grids

#5. Surface plot and contour plot
persp(x,y,z) #draw the 3-D perspective plot
contour(x,y,z, xlab="X1", ylab="X2") #draw the contour

#try another one
library(mnormt) 
mu=c(0,0) #set the mean vector
sigma=matrix(c(2,(sqrt(2)/2),(sqrt(2)/2),1),2,2) #covariance matrix  
len=100
x=seq(mu[1]-5*sqrt(sigma[1,1]),mu[1]+5*sqrt(sigma[1,1]),length=len) #generate the grids of x, 5 standard deviation from the mean
y=seq(mu[2]-5*sqrt(sigma[2,2]),mu[2]+5*sqrt(sigma[2,2]),length=len) #generate the grids of y
temp=as.matrix(expand.grid(x,y)) #generate the grids of x and y pairs
zvec=dmnorm(temp,mu,sigma) #calculate the density of f(x, y)
z=matrix(zvec,len,len,byrow=T)
csq=qchisq(0.5,2) #chi-square with df=2 - c^2 in the function
lev=1/(2*pi*sqrt(det(sigma)))*exp(-csq/2) #density function f(vector x)
contour(x,y,z, xlab="X1", ylab="X2",levels=lev) #the level matches with the density function 

#save the figure as ps file
postscript(file="/Users/wanhuasu/Documents/MacEwan/stat372/plots/bivariate_normal_rho_negative0.75.ps",height=8,width=8,horizontal=F)
persp(x,y,z)
dev.off()

#use the theorem
obj=eigen(sigma)
c=sqrt(csq)
a1=mu-c*sqrt(obj$values[1])*obj$vectors[,1]
a2=mu+c*sqrt(obj$values[1])*obj$vectors[,1]
b1=mu-c*sqrt(obj$values[2])*obj$vectors[,2]
b2=mu+c*sqrt(obj$values[2])*obj$vectors[,2]
points(rbind(a1,a2,b1,b2)) #add the points to the figure
abline(0,1) #add a line with intercept 0 and slope 1 to the current figure
abline(0,-1)  #add a line with intercept 0 and slope -1 to the current figure




