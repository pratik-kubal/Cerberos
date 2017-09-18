#Cost function takes input of x,y and Initial theta and gives output of a cost value which we try to minimize it

cost <- function(x,y,theta)
{
  J = 0
  m <- nrow(y)
  sqrErr <- as.matrix(rep.int(0,nrow(y)))
  #for(i in 1:nrow(x))
  #{
    #temp <- t(theta)%*%x[i,]
    #pred <- sum(temp)
    #sqrErr[i,] <- (pred - y[i,])^2
  #}
  htheta <- x %*% theta
  sqrErr <- htheta - y
  J <- (1/2*m)*sum(sqrErr^2) #Consider NaN values rm.na=false
  return(J)
}
