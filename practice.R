library(tidyverse)
mu <- 10; sigma <- 2; n <- 25; numSim <- 1000

parameter <- log(mu^2) 
estimator <- array(0,numSim)
x<- rnorm(n,mu,sigma)

bdata <- x %>%
  rsample::bootstraps(times = numSim)

for (i in 1:numSim){
  idx   <- sample(1:numSim, replace=TRUE)
  bootd <- x[idx,]
  estimator[i] <- log(mean(x^2))
}

boot_stderr <- sd(balpha)  
boot_stderr