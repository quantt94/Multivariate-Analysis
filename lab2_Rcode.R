#Stat 372, Lab 2, Jan 18, 2016

################################################################################
#Topics covered:
#  1. 2-d, 3-d scatter plots and scatter plot matrix
#  2. Star plot and Chernoff faces plot
#  3. Distances
#  4. Generate random numbers from a given distribution
#  5. Use R Sweave (.Rnw file) to compile your R codes and outputs in a pdf file  
################################################################################

#2d scatter plot on sepal width and petal length
plot(iris[,2],iris[,4],col=c("red","black","green")[unclass(iris$Species)],
     pch=c(1,3,4)[unclass(iris$Species)],xlab="Sepal Width",ylab="Petal Width",
     main="2-D Scatter Index Plot")
legend(3.9,2.5,pch=c(1,3,4),col=c("red","black","green"),c("Setosa","Versicolor","Virginical"))

names(iris)
unclass(iris$Species)

example(points) #about points (check all kinds of points)
?par #control parameters for plots

#3d scatterplot
library(scatterplot3d)
scatterplot3d(iris[,c(1,2,4)],color=c("red","black","green")[unclass(iris$Species)],
              pch=c(1,3,4)[unclass(iris$Species)],ylab="Sepal Width",zlab="Petal Width",
              xlab="Septal Length",main="3-D Scatter Index Plot")

#scatter plot matrix
pairs(iris[1:4],main="Iris Data (red circle=setosa,black plus=versicolor,green cross=virginica)", 
      +       pch=c(1,3,4)[unclass(iris$Species)],     
      +       col=c("red","black","green")[unclass(iris$Species)]()
#stars plot and faces plot
stars(iris[,-5]) #star plot of the Iris data
library(aplpack)
faces(iris[,-5],face.type=0)
faces(iris[,-5],face.type=1)
faces(iris[,-5],face.type=2)

s?faces

#Distance for quantitative variables
x=c(60,50,70)
y=c(80,60,90)
dist(rbind(x,y),method="euclidean") #the input should be a numeric matrix
?dist

dist(rbind(x,y),method="manhattan")
dist(rbind(x,y),method="minkowski",p=1)#the same as Manhattan
dist(rbind(x,y),method="minkowski",p=2)#the same as Euclidean

#Distance for categorical variables
## example of binary and canberra distances.
x = c(0, 0, 1, 1, 1, 1)
y =c(1, 0, 1, 1, 0, 1)
dist(rbind(x,y), method= "binary")

#distance between observations with categorical measurements 
x=c("F","M", "M")
y=c("Brown","Grey","Brown")
z=data.frame(gender=x,haircolor=y)
library(cluster)
daisy(z,metric = "gower")

#distance for mixed data types.
height=c(60,50,70)
weight=c(80,60,90)
x=c("F","M", "M")
y=c("Brown","Grey","Brown")
z=data.frame(gender=x,haircolor=y,height=height,weight=weight)
library(cluster)
daisy(z,metric = "gower")

#add a unbalanced binary variable
asian=c(1,0,0)
z=data.frame(gender=x,haircolor=y,race=asian,height=height,weight=weight)
daisy(z,metric = "gower")

asian=c(1,0,0)
z=data.frame(gender=x,haircolor=y,race=asian,height=height,weight=weight)
daisy(z,metric = "gower", type=list(asymm =3, symm=c(1,2)))

#Generate simple random sample from a population
#consider the Iris data, randomly pick 12 flowers from each species
set.seed(107) #set the seed to make sure you can reproduce the result
tind=c(sample(1:50,12),sample(51:100,12),sample(101:150,12))#randomly pick 12 flowers from each species
ind=sample(tind,36) #permute the order of the sample
iris[ind,-1]

?rnorm #generate random numbers from normal distributions
mu=0
std=1
n=1000
set.seed(107) #set the random seed
x=rnorm(n,mu,std) #generate 1000 random numbers from N(0,1)
hist(x,prob=T) #draw the histogram of the random number, bell-shape?
yhat=density(x,bw=0.5) #obtained the estimated density of x
lines(yhat) #add the density curve on the top of the histogram

#calculate the density
x=seq(-4,4,by=0.1) #generate a sequence from -4 to 4)
y=dnorm(x) #calculate the density of x
plot(x,y,type="l") #draw the density curve

#about t-distribution
?rt

#for multivariate normal
library(mnormt) 
mu=c(0,0) #set the mean vector
sigma=matrix(c(1,0,0,1),2,2) #covariance matrix  
#sigma=matrix(c(1,0.75,0.75,1),2,2)                            
x=seq(-4,4,0.1) #generate the grids of x
y=seq(-4,4,0.1) #generate the grids of y
len=length(x) #find the number of observations of x
temp=as.matrix(expand.grid(x,y)) #generate the grids of x and y pairs
zvec=dmnorm(temp,mu,sigma) #calculate the density of f(x, y)
z=matrix(zvec,len,len,byrow=T) #rearrange the value of f(x, y) in grids
persp(x,y,z) #draw the 3-D perspective plot
contour(x,y,z, xlab="X1", ylab="X2") #draw the contour


#Use Sweave (.Rnw file) to compile R codes and outputs in a pdf file
#Open a new R Sweave file
#click the "chunck" (green botton) on the top right corner of the script window
#choose "insert chunk" to insert a new chunk.
#type your R codes within the chunk
#click "Complile PDF"



