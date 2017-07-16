# Depends on theta, feature's mean, feature's standard dev and Predicts Y
# NOTE: Give it the original X not a normalized X, since it will through errors if given a scaled/ normalized X data

oracle <- function(theta,features,x)
{
  mu <- as.matrix(rep.int(0,ncol(x)))
  sigma <- as.matrix(rep.int(1,ncol(x)))
  for(i in 2:ncol(x))
  {
    mu[i,] <- mean(x[,i])
    sigma[i,] <- sd(x[,i])
  }
  temp <- ((features - mu)/sigma)*theta
  output <- sum(temp)
 return(output)  
}

#Professor X function
#Takes input in as a test dataframe and outputs a prediction matrix
#Note: uses Oracle function
profX <- function(test_data,theta,x){
  pred <- matrix(data=NA,nrow(test_data),ncol=1)
  for(i in 1:nrow(test_data)){
         pred[i,] <- oracle(theta,test_data[i,],x)
     }
  return(pred)
}
