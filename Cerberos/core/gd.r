# Feature Scaling > Gradient Descent > Returns intercept(Theta)
# Takes input and returns a MATRIX of the form gd [theta,J_hist]
#Maybe the theta given is init to 0 check this first if error
#clean
#rm(list=ls())
gd <- function(x,y,alpha,num_iter)
{
  theta <- as.matrix(rep.int(0,ncol(x)))
  m <- nrow(y)
  J_hist <- as.matrix(rep.int(0,num_iter))
  
  for(i in 1:num_iter)
  {
    htheta = x %*% theta
    theta_temp <- as.matrix(rep.int(0,nrow(theta)))
    for(iter in 1:nrow(theta))
    {
      theta_temp[iter,] <- theta[iter,] - ((alpha/m)*sum(t(htheta-y)%*%x[,iter]))
    }
    theta <- theta_temp
      
    J_hist[i,] <- cost(x,y,theta)
  }
  return(list(theta=theta,J_hist=J_hist))
}



