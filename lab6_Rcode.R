#Stat 372, Lab 6, Feb.22, 2016

################################################################################
#Topics covered:
#  1. take a simple random sample from the data, divide the data into training and testing set
#  2. loop in R 
#  3. Newton Raphson method
#  4. K-nearest neighbor 
#  5. Logistic regression
################################################################################

#  1. take a simple random sample from the data, divide the data into training and testing set
(?sample)
a=c(2,1,4)
(sample(a,2)) #take an SRS of size 2 from vector a

#consider the iris data, the last two species, 50 flowers from each species. 
#Divide the data into two parts: training and test
set.seed(107)
(ind=sort(c(sample(51:100,25),sample(101:150,25)))) #with 25 flowers from the 2nd species and 25 flowers from the 3rd species
train=iris[ind,]
(test=iris[setdiff(51:150,ind),]) #from 51 to 150, except for the one we took out, the remaining is test set

#  2. loop in R
#for loop
for (i in 1:5){
  cat("i=",i,"\n") #cat means print i=the value of i, "\n": change line, give another line
}

#while loop (loop while the conditions are actually satisfied)
i=0
while (i<=5){
  cat("i=",i,"\n")
  i=i+1
}

#some conditionals
(1==2) #equal
(1>2)
(1<=2)
(1!=2) #not equal

#  3. Newton Raphson method
##############################################################################
# a numerical method to find the roots for f(x)=0
#by the first order Taylor approximation
#f(x)=f(x0)+f'(x0)(x-x0)==>x=x0-f(x0)/f'(x0)==> x_new=x_old-f(x_old)/f'(x_old)
#loop until |x_new-x_old|<error
##############################################################################

#use Newton Raphson method to find the roots of f(x)=x^2+6x+8=0
#f'(x)=2x+6
x0=0
x1=-100000
error=0.00001
i=1
while (abs(x1-x0)>error)
{
  x0=x1
  f0=x0^2+6*x0+8
  f1=2*x0+6
  x1=x0-f0/f1
  cat("i=",i,"x=", x1, "\n")
  i=i+1
}

#  4. K-nearest neighbor 
set.seed(107)
ind=sort(c(sample(51:100,25),sample(101:150,25)))
train=iris[ind,-c(3,4)]
test=iris[setdiff(51:150,ind),-c(3,4)]
train$Species=as.factor(as.matrix(train$Species))
test$Species=as.factor(as.matrix(test$Species))

plot(train[,-3],col=c("red","black")[rep(c(1,2),c(25,25))],pch=c(1,3)[rep(c(1,2),c(25,25))])

#use cross-validation to obtain the best k
library(class) #load the library
kvec=seq(1,49,by=2) #grid of k values
klen=length(kvec)
evec=rep(0,klen) #vector for misclassification rate for each k value
for (i in 1:klen){
  y.cv=knn.cv(train[,-3],train[,3],k=kvec[i]) #built-in R function using leave-one-out cross validation 
  evec[i]=sum(y.cv!=train[,3])
}
plot(kvec,evec)
(kbest=kvec[order(evec)[1]]) #pick the k giving the smallest misclassification rate

#apply the KNN model with the best k on the test set
(mknn=knn(train[,-3],test[,-3],train[,3],k=kbest,prob=T))
(ktab=table(test[,3],mknn)) #misclassification table
(krate=1-sum(diag(ktab)/sum(ktab)))

#  5. Logistic regression
?glm #generalized linear model
m0=glm(Species~.,data=train,family="binomial") 
(summary(m0))

?predict.glm
pvec=predict(m0,test,type="response")
(ltab=table(test[,3],pvec>0.5))
(lrate=1-sum(diag(ltab)/sum(ltab)))

#about the spam data
data0=read.table("/Users/wanhuasu/Documents/MacEwan/stat372/Lab/data/spam.txt",sep="")
data0=spam
data=data0[,-58]

y=data0[,58]
table(y) #0 is email and 1 is spam

len=length(y)
ind1=which(y==1)
ind0=which(y==0)
n1=round(length(ind1)/2)
n0=ceiling(length(ind0)/2)

#divide into training and test sets

set.seed(107)
trind=sort(c(sample(ind1,n1),sample(ind0,n0)))
train=data0[trind,]
test=data0[-trind,]

ytrain=y[trind]
ytest=y[-trind]

dimnames(train)[[2]]=c("make","address","all","3d","our","over","remove","internet","order","mail","receive","will","people","report","addresses","free","business","email","you","credit","your","font","000","money","hp","hpl","george","650","lab","labs","telnet","857","data","514","85","technology","1999","parts","pm","direct","cs","meeting","original","project","re","edu","table","conference","ch;","ch(","ch[","ch!","ch$","ch#","CRL_avg","CRL_longest","CRL_total","y")

dimnames(test)[[2]]=c("make","address","all","3d","our","over","remove","internet","order","mail","receive","will","people","report","addresses","free","business","email","you","credit","your","font","000","money","hp","hpl","george","650","lab","labs","telnet","857","data","514","85","technology","1999","parts","pm","direct","cs","meeting","original","project","re","edu","table","conference","ch;","ch(","ch[","ch!","ch$","ch#","CRL_avg","CRL_longest","CRL_total","y")
library(rpart)
tobj=rpart(y~.,data=data.frame(x=train[,-58],y=train[,58]),method="class",parms=list(split="information"),cp=0) #build a big tree, cp=0