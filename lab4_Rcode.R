#Stat 372, Lab 4, Feb.1, 2016

################################################################################
#Topics covered:
#  1. Conduct t test for univariate cases.
#  2. Hotelling's T-square test for comparing one or two mean vector(s)
#  3. One-way MANOVA for comparing more than two mean vectors
#  4. More about writing your own function
################################################################################

#1. Conduct t test for univariate cases.
#One-sample, paired and two-sample t tests for univariate case.
?t.test
#one-sample t test
x=rnorm(100,0,1) #generate 100 random numbers from a standard normal 
t.test(x, mu=0) #two-tailed test
t.test(x, mu=0,alternative="less") #left-tailed test
t.test(x, mu=0,alternative="greater",conf.level=0.99) #right-tailed test
t.test(x, mu=0,alternative="greater",conf.level=0.95)
#reject the null if the corresponding confidence interval 

#two t-test, use the extra sleep data
sleep #show the data
plot(extra~group, data=sleep) #side-by-side boxplot
#a direct way
ind1=which(sleep$group==1) #get the index of group 1
x1=sleep$extra[ind1] #get the response for group 1
x2=sleep$extra[-ind1] #get the response for group 2
t.test(x1,x2)

#a short cut
t.test(extra~group, data=sleep) 
t.test(extra~group, data=sleep,alternative="less") 

#2. Hotelling's T-square test for comparing one or two mean vector(s)
#Example in course note. For ONE MEAN vector. 
xmat=matrix(c(6,10,8,9,6,3),3,2) #assign the data matrix from the course note
library(ICSNP) #load the package
?HotellingsT2
HotellingsT2(xmat, mu = c(9,5)) #Hotelling's test, it gives you the F score

#do it directly
p=ncol(xmat) #counting the number of column
n=nrow(xmat) #counting the number of row
mvec=matrix(apply(xmat,2,mean),p,1) #apply the mean function on column
mu0=matrix(c(9,5),p,1) 
smat=cov(xmat)
v0=n*(n-p)/(p*(n-1))*t(mvec-mu0)%*%solve(smat)%*%(mvec-mu0)
pvalue=pf(v0,p,n-p,lower.tail=F) #calculate the p-value (give us the upper tail coz lower tail is equal False)


#compare TWO SAMPLES
#read the bird data
bird=read.table("/Users/wanhuasu/Documents/MacEwan/stat372/winter2016/lab/bumpus.txt",header=T)

ind=1:21 #birds 1 to 21 are survivors
xmat1=birddata[ind,] #body measurements for survivors
xmat2=birddata[-ind,] #body measurements for non-survivors
HotellingsT2(xmat1, xmat2)
apply(xmat1,2,mean)
apply(xmat2,2,mean)
#for two mean vectors, we can also use
install.packages("Hotelling")
library(Hotelling)
?hotelling.test
obj=hotelling.test(xmat1,xmat2)

#try the function on the first two species of the Iris data
data0=iris[1:100,]
HotellingsT2(data0[1:50,-5], data0[51:100,-5])

#another way to do that
HotellingsT2(data.matrix(data0[,-5])~data0[,5]) 
##convert the first input from data.frame to a numeric matrix

#  3. One-way MANOVA for comparing more than two mean vectors
#one-way ANOVA, use the iris data on pedtal length
xvec=iris[,3]
group=iris[,5]
obj=lm(xvec~group)
anova(obj)

#example in the course note
#Wilks' Lambda test
(x1=matrix(c(9,3,6,2,9,7),3,2,byrow=T))
(x2=matrix(c(0,4,2,0),2,2,byrow=T))
(x3=matrix(c(3,8,1,9,2,7),3,2,byrow=T))
(x=rbind(x1,x2,x3))
(xbar=t(apply(x,2,mean))) #a row vector
(x1bar=t(apply(x1,2,mean)))
x2bar=t(apply(x2,2,mean))
x3bar=t(apply(x3,2,mean))

s1=cov(x1)
s2=cov(x2)
s3=cov(x3)
n1=3
n2=2
n3=3
B=n1*t(x1bar-xbar)%*%(x1bar-xbar)+n2*t(x2bar-xbar)%*%(x2bar-xbar)+n3*t(x3bar-xbar)%*%(x3bar-xbar)

w=(n1-1)*s1+(n2-1)*s2+(n3-1)*s3
det(w)
det(w+B)
lambda=det(w)/det(w+B)
n=n1+n2+n3
p=2
k=3
f0=(n-k-1)/(k-1)*(1-sqrt(lambda))/sqrt(lambda)
pf(f0,4,8,lower=F)

#use the built-in function
install.packages("rrcov")
library(rrcov)
gr=as.factor(rep(c(1,2,3),c(n1,n2,n3)))
Wilks.test(x,gr)


#Wilk's lambda test for several mean vectors, use the Iris data
library(rrcov)
Wilks.test(iris[,3:4],iris[,5]) #use normal chi-square approximation

?manova
ir=as.matrix(iris[,-5])
irgr=iris[,5]

irisman=manova(ir~irgr)
summary(irisman, test="Wilks")

#one-way ANOVA
m0=lm(iris[,3]~iris[,5]) #lm=linear model
m0
anova(m0)
