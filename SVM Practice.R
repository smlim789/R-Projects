# Support Vector Classifier

x <- matrix(rnorm(40*2), ncol=2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y), pch=19)
dat=data.frame(x=x, y=as.factor(y))


library(e1071)
svmfit_10=svm(y~., data=dat, kernel="linear", cost=10,scale=FALSE)
plot(svmfit_10, dat)
svmfit_10$index # Support vectors
summary(svmfit_10)

svmfit_01=svm(y~., data=dat, kernel="linear", cost=0.1,scale=FALSE)
plot(svmfit_01, dat)
svmfit_01$index # Support vectors
summary(svmfit_01)


tune.out=tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)
plot(bestmod, dat)


xtest=matrix(rnorm(100*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1

# C=0.1
testdat=data.frame(x=xtest, y=as.factor(ytest))
ypred=predict(bestmod,testdat)
table(predict=ypred, truth=testdat$y)

# C=0.01
svmfit=svm(y~., data=dat, kernel="linear", cost=0.001,scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred, truth=testdat$y)


# Linearly Separable
x[y==1,]=x[y==1,]+0.5
#plot(x, col=(y+5)/2, pch=19)
plot(x[,2], x[,1], col=(y+5)/2, pch=19)
dat=data.frame(x=x,y=as.factor(y))

svmfit=svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit, dat)

svmfit=svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit,dat)








# Support Vector Machine
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x[,2], x[,1], col=y+1, pch=19)

train=sample(200,100)
svmfit=svm(y~., data=dat[train,], kernel="radial",  gamma=1, cost=1)
plot(svmfit, dat[train,])
summary(svmfit)
svmfit=svm(y~., data=dat[train,], kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])
summary(svmfit)

tune.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newdata=dat[-train,]))



# ROC Curves

library(ROCR)
rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}
par(mfrow=c(1,1))

svmfit.opt=svm(y~., data=dat[train,], kernel="radial",gamma=2, cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,dat[train,],decision.values=TRUE))$decision.values
rocplot(fitted,dat[train,"y"],main="Training Data", lwd=2)

svmfit.flex=svm(y~., data=dat[train,], kernel="radial",gamma=50, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red", lwd=2)

fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data", lwd=2)

fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="blue", lwd=2)


