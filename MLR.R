

#---------------------------------------------------------------------------------------------------------#
# Read "Advertising.csv" from your laptop first.
# File-> Import 
attach (Advertising)
#---------------------------------------------------------------------------------------------------------#

#install.packages("scatterplot3d")
library(scatterplot3d)
library(tidyverse)

#---------------------------------------------------------------------------------------------------------#

y        <- sales
x1       <- TV
x2       <- radio
x3       <- newspaper   

yname    <- "Sales"
x1name   <- "TV adv"
x2name   <- "Radio adv"
x3name   <- "News adv"

adv <- lm(y ~ x1 + x2 + x3)
summary(adv)
summary(aov(adv))

performance::check_model(adv)


#---------------------------------------------------------------------------------------------------------#
# Scatter plot

adv_tv <- lm(sales ~ TV, data=Advertising)

Advertising %>%
  bind_cols(
    pred_sales = predict(adv_tv, data = Advertising)
  ) %>%
  ggplot(aes(x = TV)) +
  geom_linerange(aes(ymin = sales, ymax = pred_sales)) +
  geom_point(aes(y = sales), color = "red") +
  theme_classic() +
  geom_abline(intercept = coef(adv_tv)[1], slope = coef(adv_tv)[2],
              color = "blue", linewidth = 1)


pairs(Advertising[,2:5], pch = 19, col="orange")
plot3d <- scatterplot3d(x1,x2,y, color = "purple", angle =45, scale.y =1, pch = 16, main = "Delivery Time Data")
model  <- lm(y ~ x1+x2)
plot3d$plane3d(model, col="red")

#---------------------------------------------------------------------------------------------------------#


# Model Fitting

n       <- length(y)
const   <- rep(1,n)

X       <- cbind(const,x1,x2,x3)
XpX     <- t(X)%*%X
XpY     <- t(X)%*%y
Inv_XpX <- solve(t(X)%*%X)
betahat <- Inv_XpX%*%XpY
betahat <- c(Inv_XpX%*%XpY)
H       <- X%*%Inv_XpX%*%t(X)
p       <- length(betahat)
k       <- p-1

ybar    <- mean(y)
yhat    <- X%*%betahat
e       <- y-yhat

#---------------------------------------------------------------------------------------------------------#

# Anova Table
I       <- diag(n)
J       <- const%*%t(const)

SST     <- t(y)%*%(I-J/n)%*%y
SSR     <- t(y)%*%(H-J/n)%*%y
SSE     <- t(y)%*%(I-H)%*%y
R2      <- SSR/SST
R2adj   <- 1-SSE/SST*(n-1)/(n-p)

MSR     <- SSR/(k)
MSE     <- SSE/(n-p)
F0      <- MSR/MSE
pvalF0  <- 1-pf(F0,k,n-p)

#---------------------------------------------------------------------------------------------------------#

# Conficence Interval

s2      <- c(MSE)
s       <- sqrt(s2)
Cov     <- s2*Inv_XpX
sterr   <- c(sqrt(diag(Cov)))

tstat   <- c(betahat/sterr)
pvalT   <- 2*(1-pt(abs(tstat),n-p))
LB      <- c(betahat-qt(0.975,n-p)*sterr)
UB      <- c(betahat+qt(0.975,n-p)*sterr)
ParaEst <- cbind(betahat, sterr, tstat, pvalT, LB, UB)


colnames(ParaEst) <-c("Est","se","tval","pval","LB","UB")
row.names(ParaEst)<-c("Int","X1","X2", "X3")
round(ParaEst,3)


#---------------------------------------------------------------------------------------------------------#

# Residuals
hii     <-c(diag(H))
si2     <- ((n-p)*s2-(e^2)/(1-hii))/(n-p-1)
e_std   <- (y-yhat)/s # Standardized
e_stud  <- e_std/sqrt(1-diag(H)) # Studentized (Internal)
e_pres  <- e/(1-diag(H))  # Press
e_Rstu  <- e/sqrt(si2*(1-diag(H)))  # R-Studentized (External)

residual <-round(cbind(e, e_std, e_stud, e_pres, e_Rstu, hii),2)
residual

plot(e_std, e_stud)
plot(e, e_std)

#---------------------------------------------------------------------------------------------------------#

# Leverage and Influence
CookD    <- e_stud^2/p*hii/(1-hii)
DFFITS   <- sqrt(hii/(1-hii))*e_Rstu
COVRATIO <- (si2/s2)^p/(1-hii)

Diagnostics <- round(cbind(hii,CookD, DFFITS, COVRATIO),4)
Diagnostics

#---------------------------------------------------------------------------------------------------------#

# QQ plot
qqnorm(e,     datax=F, pch=19, col='red', cex.main=1, cex.axis=1, cex.lab=1, main="QQ-plot (Resid)")
qqline(e,     datax=F)
qqnorm(e_std, datax=F, pch=19, col='red', cex.main=1, cex.axis=1, cex.lab=1, main="QQ-plot (Std_Resid)")


# Residual plot
plot(yhat, e,      pch=19, col='blue',   cex.main=2,               cex.axis=1, cex.lab=1, main="Residual plot")
plot(yhat, e_std,  pch=19, col='purple', cex.main=2, ylim=c(-2,4), cex.axis=1, cex.lab=1, main="Standardized Residual plot")
plot(yhat, e_stud, pch=19, col='red',    cex.main=2, ylim=c(-2,4), cex.axis=1, cex.lab=1, main="Studentized Residual plot")

#------------------------------------------------------------------------------------------------------#
# Plots using R function
plot(adv, which=1)
plot(adv, which=2)
plot(adv, which=3)
plot(adv, which=4, cook.levels=4/(n-p))
plot(adv, which=5, cook.levels=4/(n-p))


# Distribution of studentized residuals
hist(e_std, freq=FALSE, main="Distribution of Studentized Residuals", breaks=n/5)
xfit<-seq(min(e_std),max(e_std),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit, col='red')

#------------------------------------------------------------------------------------------------------#



