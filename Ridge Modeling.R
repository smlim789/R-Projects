library(ISLR)
library(glmnet)
library(dplyr)
library(tidyr)


Hitters = na.omit(Hitters)

# Regressors
x <- model.matrix(Salary~., Hitters)[,-1]

# Response variable
y <- Hitters %>%
  select(Salary) %>%
  unlist() %>%
  as.numeric()

grid <- 10^seq(4, -2, length = 50)
ridge_mod <- glmnet(x, y, alpha = 0, lambda = grid) # alpha=0: Ridge, alpha=1; Lasso

dim(coef(ridge_mod))
plot(ridge_mod, lwd=3)    # Plot of coefficients

predict(ridge_mod, s = 50, type = "coefficients")[1:20,]


#---------------------------------------------------------------------------

train <- Hitters %>%
  sample_frac(0.5)

test <- Hitters %>%
  setdiff(train)

x_train <- model.matrix(Salary~., train)[,-1]
y_train <- train %>%
  select(Salary) %>%
  unlist() %>%
  as.numeric()

x_test <- model.matrix(Salary~., test )[,-1]
y_test <- test %>%
  select(Salary) %>%
  unlist() %>%
  as.numeric()

#---------------------------------------------------------------------------

ridge_mod <- glmnet(x_train, y_train, alpha=0, lambda = grid, thresh = 1e-12)

ridge_pred <- predict(ridge_mod, s = 4, newx = x_test)
mean((ridge_pred - y_test)^2)

ridge_pred <- predict.glmnet(ridge_mod, s = 50, newx = x_test)
mean((ridge_pred - y_test)^2)

ridge_pred <- predict(ridge_mod, s = 1e10, newx = x_test)
mean((ridge_pred - y_test)^2)

ridge_pred <- predict(ridge_mod, s = 0, newx = x_test)
mean((ridge_pred - y_test)^2)


#--------------------------------------------------------------------------------------------------------

cv.out <- cv.glmnet(x_train, y_train, alpha = 0) 
bestlamda <- cv.out$lambda.min  # Select lambda that minimizes training MSE
bestlam

plot(cv.out) # Draw plot of training MSE as a function of lambda


ridge_pred <- predict(ridge_mod, s = bestlam, newx = x_test) # Use best lambda to predict test data
mean((ridge_pred - y_test)^2) # Calculate test MSE

out <- glmnet(x, y, alpha = 0) # Fit ridge regression model on full dataset
predict(out, type = "coefficients", s = bestlam)[1:20,] # Display coefficients using lambda chosen by CV







