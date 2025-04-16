
par(mfrow=c(1,2))

numSim   <-  50
n        <-  10
p        <-  2
lambda   <-  0.05
rx       <-  1
sigmae   <-  0.2
beta1    <-  5
beta2    <- -1
beta     <- cbind(rep(beta1,numSim), rep(beta2,numSim))

LSE   <- array(0,c(numSim,2))
Rid   <- array(0,c(numSim,2))

# For each iteration..
for( i in 1:numSim ){
  x1 <- rnorm(n,0,1)
  x2 <- rx*x1+rnorm(n,0,sigmae)

  # Design matrix.
  x <- cbind(x1,x2 )
  r <- cor(x1,x2)
  # y = x1 - x2 + epsilon where epsilon ~ N(0,.1)
  y <- beta1*x1 + beta2*x2 + rnorm(n)
  
  # LS regression 
  LSE[i,]<- solve(t(x)%*%x)%*%t(x)%*%y
  
  # Ridge regression 
  Rid[i,]<- solve(t(x)%*%x+lambda*diag(2)) %*%t(x)%*%y
}

# Plot the LS estimates for betas.
plot(LSE[,1], col = 'blue', pch = 16, ylim = c(min(LSE),max(LSE)),
      ylab = 'Coefficients Estimates', xlab = 'Least Square Estimates', bty ='l',)
points(LSE[,2], col = 'red', pch = 16 )

# Add horizontal lines representing the true values of betas
abline(a = beta1, b =0, col = 'blue' ) # Plot the true value of beta_1 = 1.
abline(a = beta2, b =0, col='red' ) # Plot the true value of beta_2 = -1.

# Do the same for the ridge estimates.
plot(Rid[,1], col = 'blue', pch = 16, ylim = c(min(LSE), max(LSE)), 
      ylab = 'Coefficients Estimates', xlab = 'Ridge Estimates', bty ='l',)
points(Rid[,2], col = 'red', pch = 16 ) 

abline( a = beta1, b = 0, col = 'blue' )
abline( a = beta2, b = 0, col = 'red' )

legend('bottomleft', horiz=F, cex = 1, col=c('blue','red'), pch=c(16,19), 
        legend = c(expression(hat(beta)[1]),expression(hat(beta)[2])), bty = 'n' )
mtext(bquote(~r==.(round(r,4))~ ", " ~n==.(n)~ ", "  ~p==.(p)~ ", "  ~Iteration==.(numSim)
              ~ ", " ~beta[1]==.(beta1)~ ", " ~beta[2]==.(beta2)), side = 3, line = -2, outer = TRUE )

# MSE ratio
sum((LSE-beta)^2)/sum((Rid-beta)^2)







