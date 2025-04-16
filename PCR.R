
library(ISLR2)
library(pls)

data(Hitters)
names(Hitters)
head(Hitters)

n <- dim(Hitters)[1]

x1t <- Hitters[,2]
x2t <- Hitters[,3]

x1 <- (x1t-mean(x1t))/(sd(x1t)/sqrt(n-1))
x2 <- (x2t-mean(x2t))/(sd(x2t)/sqrt(n-1))
t(cbind(x1,x2))%*%cbind(x1,x2)

mean(x1)
mean(x2)
sd(x1)
sd(x2)

t(cbind(x1,x2))%*%cbind(x1,x2)/(n-1)
cor(cbind(x1,x2))


# Discard NA's
Hitters <- na.omit(Hitters)

attach(Hitters)
head(Hitters)
dim(Hitters)

# Summary Statistics
summary(Hitters)

# Design Marix
X <- model.matrix(Salary~.,Hitters)[,-1]
y <- Hitters$Salary

# Training and Validation datasets
train <- sample (1: nrow(X), nrow(X)/2)
test  <- (-train)
y.test<- y[test]


# PCR
pcrfit <- pcr(Salary~. , data=Hitters, subset=train, scale = TRUE, validation = "CV")
summary(pcrfit)
Var_explained_ind <- pcrfit$Xvar/pcrfit$Xtotvar*100
Var_explained_sum <- cumsum(pcrfit$Xvar/pcrfit$Xtotvar*100)
Var_explained     <- c(0,Var_explained_sum)

Comp <- names(pcrfit$Xvar)
numComp <- 0:19

# Graphs
barplot(Var_explained_ind,names.arg=Comp,xlab="Component", ylab="Variation Explained",
        col="orange", main="Contribution chart",border="red")

plot(numComp,  Var_explained, xlab="The Numbe of PCs", ylab="Variation explained",
     main="Cumulative Contribution of PCs")
lines(numComp, Var_explained, col='red', pch=19, type='b', lwd=2)
validationplot(pcrfit,val.type="RMSEP")

fitted_values <- fitted(pcrfit, comps = 1:7)
residuals     <- resid(pcrfit, comps = 1:7)
coefficients  <- coef(pcrfit, comps = 1:7)

pcrpred <- predict(pcrfit,X[test,],ncomp=3) 
sqrt(mean((pcrpred-y.test)^2))


#----------------------------------------------------------

library(MPV)
data(table.b1)
nfl <- table.b1
attach(nfl)


# Summary Statistics
summary(nfl)

# pairs.panels(nfl, gap = 0, pch=21, bg = c("red", "yellow", "blue")[training$Species])

# Design Marix
X <- model.matrix(y~., nfl)[,-1]
y <- nfl$y

eigen(t(X)%*%X)
c <- cor(X)
eigen(c)
# Training and Validation datasets
train <- sample (1: nrow(X), nrow(X)/2)
test  <- (-train)
y.test<- y[test]




# PCR
pcrfit <- pcr(y~. , data=nfl, subset=train, scale = TRUE, validation = "CV")
summary(pcrfit)
pcrfit$coefficients
pcrfit$scores
pcrfit$loadings
pcrfit$Yloadings

Var_explained_ind <- pcrfit$Xvar/pcrfit$Xtotvar*100
Var_explained_sum <- cumsum(pcrfit$Xvar/pcrfit$Xtotvar*100)
Var_explained     <- c(0,Var_explained_sum)

Comp <- names(pcrfit$Xvar)
numComp <- 0:dim(X)[2]

# Graphs
barplot(Var_explained_ind,names.arg=Comp,xlab="Component", ylab="Variation Explained",
        col="orange", main="Contribution chart",border="red")

plot(numComp,  Var_explained, xlab="The Numbe of PCs", ylab="Variation explained",
     main="Cumulative Contribution of PCs")
lines(numComp, Var_explained, col='red', pch=19, type='b', lwd=2)
validationplot(pcrfit,val.type="RMSEP")
  
fitted_values <- fitted(pcrfit, comps = 1:7)
residuals     <- resid(pcrfit, comps = 1:7)
coefficients  <- coef(pcrfit, comps = 1:7)

pcrpred <- predict(pcrfit,X[test,],ncomp=1) 
sqrt(mean((pcrpred-y.test)^2))

























lambda_grid <- 10^seq(10, -2, length = 100)
ridge_salary <- glmnet::glmnet(x, y, alpha = 0, lambda = lambda_grid)

coef(ridge_salary)[, 50]; coef(ridge_salary)[, 60]

library(tidyverse)

library(parsnip)

ridge_spec <- linear_reg(penalty = 0, mixture = 0) %>%
  set_engine("glmnet", path_values = lambda_grid)

hitters_ridge_fit <- fit(ridge_spec, Salary ~ ., data = Hitters)

tidy(hitters_ridge_fit, penalty = lambda_grid[50])

library(ggplot2)
map_dfr(lambda_grid,
        ~ tidy(hitters_ridge_fit, penalty = .x)) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = penalty, y = estimate)) +
  geom_point() +
  scale_x_log10(breaks = 10^seq(-2, 10, 4),
                labels = c("1e-2", "1e2", "1e6", "1e10")) +
  facet_wrap(~ term, scales = "free_y", ncol = 5) #+
  #add_facet_borders()









