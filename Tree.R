
# Fitting Classification Trees
library(tree)
library(ISLR)
data(Carseats)
attach(Carseats)

# Create Class
High <- as.factor(ifelse(Sales<=8,"No","Yes"))
Carseats <- data.frame(Carseats,High)

# Classification Tree
tree.carseats <- tree(High~.-Sales,Carseats)
summary(tree.carseats)

# Tree Plot
plot(tree.carseats)
text(tree.carseats,pretty=0)

# Results of Tree
tree.carseats

table(High)
table(ShelveLoc,High)
           
#---------------------------------------------------------------------------

# Create Training and Validation sets
n             <- dim(Carseats)[1]
trainidx      <- sample(1:nrow(Carseats), n/2)
Carseats.test <- Carseats[-trainidx,]
High.test     <- High[-trainidx]

# Tree model for Training set
tree.carseats_train <- tree(High~.-Sales,Carseats,subset=trainidx)
summary(tree.carseats_train)
tree.carseats_train

# Tree model prediction using Test set
tree.pred <- predict(tree.carseats_train,Carseats.test,type="class")
table(tree.pred,High.test)

# Cross Validation
cv.carseats <- cv.tree(tree.carseats_train, FUN=prune.misclass)
cv.carseats

par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b",col="red", pch=19, lwd=2, xlab="Tree Size", ylab="Deviance")
plot(cv.carseats$k,cv.carseats$dev,type="b",col="blue", pch=19, lwd=2, xlab="K-fold", ylab="Deviance")


prune.carseats <- prune.misclass(tree.carseats_train,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)

tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

prune.carseats=prune.misclass(tree.carseats_train,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)

tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)

#--------------------------------------------------------------------

# Fitting Regression Trees

library(MASS)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)

summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)

cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')

prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)

yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test, col="orange")
abline(0,1, col="red", lwd=2)
mean((yhat-boston.test)^2)

# Bagging and Random Forests

library(randomForest)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)


bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)

importanceMSE  <- importance(rf.boston)
Varname <- rownames(importanceMSE)
data  <- data.frame(importanceMSE[,1], Varname)
data2 <- data[order(data[,1]),]
barplot(data2[,1],names.arg=data2[,2], horiz=TRUE, col="red", border="orange", xlab="%IncMSE")

importanceMSE  <- importance(rf.boston)
Varname <- rownames(importanceMSE)
data  <- data.frame(importanceMSE[,2], Varname)
data2 <- data[order(data[,1]),]
barplot(data2[,1],names.arg=data2[,2], horiz=TRUE, col="blue", border="violet", xlab="IncNodePurity")





