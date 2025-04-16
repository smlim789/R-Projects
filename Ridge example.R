library(tidyverse)
library(dplyr)
library(matlib)

#Reading in Data
df <- read.delim("/Users/ykwon/My Drive/1. UCA Class/5. Machine Learning/3. Codes/ML_Ridge.txt")
names(df)
X <- as.matrix(df %>% select(-c('y')))
Y <- as.matrix(df %>% select(c('y')))

a<-lm(y~0+x1+x2+x3+x4+x5+x6+x7,data=df)
summary(a)

beta_ridge <- solve(t(X)%*%X+0.052*diag(rep(1,7)))%*%t(X)%*%Y
beta_lse   <- solve(t(X)%*%X)%*%t(X)%*%Y; beta_lse
yhat <- X%*%beta_lse

residual <- Y-yhat

plot(yhat, Y, col="red", pch=19, main="yhat vs y")
abline(a=0, b=1, col="blue")
plot(yhat, residual,col="red", pch=19, main="yhat vs residual")

cbind(Y,yhat)


#----------------------------------------------------------------------------------------

true_beta <- 1:7
lambda_grid  <- seq(0.001,0.1,0.001)
lenGrid      <- length(lambda_grid)
MSE_beta     <- rep(0,lenGrid)
LOOCV_rid    <- rep(0,12)
LOOCV_bet    <- rep(0,12)
#b_rid        <- array(0,c(7,lenGrid))
for(j in 1:lenGrid){
  for( i in 1:12){
    valid  <- df[i,]
    train  <- df[-i,]
    trainx <- train[,-8]
    trainy <- train[, 8]
    
    X <- as.matrix(trainx)
    y <- as.matrix(trainy)
    b_rid <- solve(t(X)%*%X+lambda_grid[j]*diag(rep(1,7)))%*%t(X)%*%y
    
    Validx <- valid %>% select(-y) %>% as.matrix()
    y_rid <- Validx%*% b_rid
    
    LOOCV_bet[i] <- sum((true_beta-b_rid)^2)
  }
  MSE_beta[j]<- mean(LOOCV_bet)
}

hatlambda <- lambda_grid[which.min(MSE_beta)]
MSE_LSE   <- rep(0.05467984,length(lambda_grid))

plot(lambda_grid,MSE_beta,col="white", ylab = "MSE_beta", xlab="lambda")
lines(lambda_grid,MSE_beta, col="red", lwd=2, ylab="MSE_beta", xlab="lambda")
abline(v=hatlambda, h=min(MSE_beta), col="blue")

range <- 31:100
plot(lambda_grid[range],MSE_beta[range],col="white", ylab = "MSE_beta", xlab="lambda")
lines(lambda_grid[range],MSE_beta[range], col="red", lwd=2, ylab="MSE_beta", xlab="lambda")
abline(v=hatlambda,h=min(MSE_beta), col="blue")


#---------------------------------------------------------------------------------------

# LOOCV to get LSE & Ridge

LOOCV_lse  <- rep(0,12) 
LOOCV_rid  <- rep(0,12) 
y_lse  <- rep(0,12) 
y_rid  <- rep(0,12) 

for( i in 1:12){
  valid  <- df[i,]
  train  <- df[-i,]
  trainx <- train[,-8]
  trainy <- train[, 8]
  
  X <- as.matrix(trainx)
  y <- as.matrix(trainy)
  
  b_lse <- solve(t(X)%*%X)%*%t(X)%*%y
  b_rid <- solve(t(X)%*%X+hatlambda*diag(rep(1,7)))%*%t(X)%*%y
  
  Validx <- valid %>% select(-y) %>% as.matrix()
  
  y_lse[i] <- Validx%*% b_lse
  y_rid[i] <- Validx%*% b_rid
  
  LOOCV_lse[i] <- (Y[i]-y_lse[i])^2
  LOOCV_rid[i] <- (Y[i]-y_rid[i])^2

}
sum(LOOCV_lse)/12
sum(LOOCV_rid)/12


cbind(y_lse-Y, y_rid-Y)
round(cbind(((y_lse-Y)^2), ((y_rid-Y)^2)),4)
cbind((sum(y_lse-Y)^2), sum((y_rid-Y)^2))

