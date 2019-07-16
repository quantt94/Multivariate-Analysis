#Stat 372, Lab 7, Feb.29, 2016

################################################################################
#Topics covered:
#  1. K-nearest neighbor
#  2. Logistic regression
#  3. Classification tree
#  4. Regression tree
################################################################################
#consider the Iris data, the last two classes. 
#divide the data into TRAINING AND TEST SETS.
set.seed(107) #make sure you set the seed every time you take SRS
ind=sort(c(sample(51:100,25),sample(101:150,25))) #take SRS from 51st species to 100th with 25 species in total, and 101st to 150th with 25 species
train=iris[ind,]
test=iris[setdiff(51:150,ind),] #set.different for removing those from ind, the remaining of those we took
train$Species=as.factor(as.matrix(train$Species))
test$Species=as.factor(as.matrix(test$Species))
head(train) #see the data structure for training set, the first 6 rows of the test
levels(train[,5])
#  1. K-nearest neighbor 
#use cross-validation to obtain the best k
library(class) #load the library
kvec=seq(1,49,by=2) #grid of k values, sequence of only odd number, by=2 so that we have majority 
klen=length(kvec)
evec=rep(0,klen) #vector for misclassification rate for each k value
yind=5 #the class label = 5
for (i in 1:klen){
  y.cv=knn.cv(train[,-yind],train[,yind],k=kvec[i]) #give you what are those class label? #built-in R function using leave-one-out cross validation 
  evec[i]=sum(y.cv!=train[,yind]) #calculate the number of error for k=i
}
plot(kvec,evec)
(kbest=kvec[order(evec)[1]]) #pick the k giving the smallest misclassification rate
#postscript(file="/Users/wanhuasu/Documents/MacEwan/stat372/plots/iris_knn_tune.ps",height=8,width=8)	
#plot(kvec,evec,xlab="k",ylab="# of Errors",cex.lab=1.5,pch=19)
#dev.off()

#apply the KNN model with the best k on the test set
kvec[c(1,2,8)]
#k=1,3,15 give the smallest error.
kbest=3
(mknn=knn(train[,-yind],test[,-yind],train[,yind],k=kbest,prob=T))
(ktab=table(test[,yind],mknn)) #misclassification table
(krate=1-sum(diag(ktab)/sum(ktab)))
?knn

#if take k=15
kbest=15
mknn=knn(train[,-yind],test[,-yind],train[,yind],k=kbest,prob=T)
(ktab=table(test[,yind],mknn)) #misclassification table
(krate=1-sum(diag(ktab)/sum(ktab)))

#if k=1
kbest=1
mknn=knn(train[,-yind],test[,-yind],train[,yind],k=kbest,prob=T)
(ktab=table(test[,yind],mknn)) #misclassification table
(krate=1-sum(diag(ktab)/sum(ktab)))


#  2. Logistic regression
?glm
?predict.glm
m0=glm(Species~.,data=train,family="binomial",maxit = 100) #maxit = maximum integration using MLE method
#Warning message:
#glm.fit: fitted probabilities numerically 0 or 1 occurred #maybe 1 variable can separate the class perfectly
#this is due to the fact that there might be one variable 
#sepaate the species perfectly.
(summary(m0))
pvec=predict(m0,test,type="response") #type="response" gives the probabilities
(ltab=table(test[,yind],pvec>0.5))
(lrate=1-sum(diag(ltab)/sum(ltab)))

plot(train[,c(1,2)], col=c("red","black")[unclass(train$Species)],pch=c(1,3)[unclass(train$Species)],cex.lab=1.3)
legend(4.9,3.4, col=c("red","black"),pch=c(1,3),c("Versicolor","Virginical"),cex=1.2)

plot(train[,c(3,4)], col=c("red","black")[unclass(train$Species)],pch=c(1,3)[unclass(train$Species)],cex.lab=1.3)
legend(3.3,2.5, col=c("red","black"),pch=c(1,3),c("Versicolor","Virginical"),cex=1.2)

#3. classification tree
library(rpart)
?rpart
tobj=rpart(Species~.,data=train,method="class",parms=list(split="information"),cp=0,minsplit=2)
#set cp=0, a tree without pruning, a large tree
#minslit=2 mean I want at least 2 

#minimize c(t)+alpha|t|
#c(t) - error, misclassification rate
#alpha - penalize 
#|t| - tree size
#grow a huge tree then we prune it
# if alpha is large then we prefer a small tree

#tobj=rpart(Species~.,data=train,method="class",parms=list(split="information"),cp=0)
mt=predict(tobj,test,type="prob")
temp=as.numeric(apply(-mt,1,order)[1,])
(ttab=table(test[,yind],temp))
(trate=1-sum(diag(ttab)/sum(ttab)))
#or use type="class"
mt=predict(tobj,test,type="class")
(ttab=table(test[,yind],mt))
(trate=1-sum(diag(ttab)/sum(ttab)))

plot(tobj)
text(tobj,minlength=0, digits=6,use.n =TRUE)

#pretty plot?
#par(mfrow=c(1,1),xpd=NA) 
plot(tobj,uniform=T,branch=0)
text(tobj,use.n=T,all=T,fancy=T,cex=1.2,fwidth=0.6,fheight=1.2)

#shall we prune the tree?
#On choosing the optimal # of splits: 
#1. choose the one gives the SMALLEST cross-validated error (xerror)
#2. Pick the SMALLEST TREE WITHIN ONE STANDARD DEVIATION of the best.
names(tobj)
tobj$cptable
#xerror - cross validation error
#xstd - standard deviation

#the trees with 2 splits and 3 splits have the same cross-validated error
#prune the tree
tobj.prune=prune(tobj,cp=0.04)
plot(tobj.prune)
text(tobj.prune,minlength=0, digits=6,use.n =TRUE)
(tm.prune=predict(tobj.prune,test,type="class"))
(tptab=table(test[,yind],tm.prune))
(tprate=1-sum(diag(tptab)/sum(tptab)))

#spam data
#data0=read.table("/Users/wanhuasu/Documents/MacEwan/stat372/Lab/data/spam.txt",sep="")
data0=read.table("M:/stat372/Lab/data/spam.txt", sep="")
data0=spam
head(data0)
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

tobj=rpart(y~.,data=data.frame(x=train[,-58],y=train[,58]),method="class",parms=list(split="information"),cp=0) #build a big tree, cp=0


mt=predict(tobj,data.frame(x=test[,-58],y=test[,58]),type="prob")
(spamtab=table(ytest,mt[,1]<mt[,2]))
(spamrate=1-sum(diag(spamtab))/nrow(test))

#plot the big tree, overfit
plot(tobj,uniform=T,branch=0)
text(tobj,use.n=T,all=T,cex=0.6)

#we need to prune the tree, that means we need to choose a value of cp to cut the tree
#print the cptable

(cptab=tobj$cptable)
#search for the value of cp such that xerror is the smallest. 
#For ties, take the large one, we prefer a simpler model.

plot(log(cptab[,1]),cptab[,4],xlab="Log of CP value", ylab="Error")
mind=order(cptab[,4])[1] #pick the cp with the smallest error
cp1=cptab[mind,1]

#or use the function ``which.min''
(cp1=cptab[which.min(cptab[,4]),1])
tobj.prune=prune(tobj,cp=cp1) #the prune tree has 16 splits and therefore 17 nodes

pmt=predict(tobj.prune,data.frame(x=test[,-58],y=test[,58]),type="prob")
(mat=table(ytest,pmt[,1]<pmt[,2]))
(ptrate=1-sum(diag(mat))/nrow(test))
#Plot the prune tree
plot(tobj.prune,uniform=T,branch=0)
text(tobj.prune,use.n=T,all=T,cex=0.6)


#4. Regression tree
#try the Boston data
library(MASS)
?Boston
#divide the data into training and testing
data=Boston
(n=nrow(data))
set.seed(107)
ind=sample(1:n,round(n/2))
train=data[ind,]
test=data[-ind,]
m1=rpart(medv~.,data=train,cp=0)

#plot the tree without pruning
plot(m1, uniform=TRUE, compress=TRUE, margin=0.1)
text(m1)

(tab=m1$cptable)
(cp1=tab[which.min(tab[,4]),1])
m1.prune=prune(m1,cp=cp1)
plot(m1.prune, uniform=TRUE, compress=TRUE, margin=0.1)
text(m1.prune)

#calculate the fitted value by regression tree
pvec1=predict(m1.prune,data=test) 

#fit a multiple regression
m2=lm(medv~., data=train)
summary(m2)
#fitted value by the multiple regression
pvec2=predict(m2, data=test)

#compare the SSE
c(sum((pvec1-test$medv)^2),sum((pvec2-test$medv)^2))
