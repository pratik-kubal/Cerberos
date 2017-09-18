#Equation: How do u want the indiviual features
# takes input with ones matrix and gives a generalized polynomic form used
# edit and add input$var when building an interface

polyMat <- function(x){
  x[,2] <- sqrt(x[,2])
  x4 <- x[,3]^2
  x_temp <- as.matrix(cbind(x,x4))
  x <- as.matrix(x_temp)
  rm(x_temp,x4) 
  return(x)
}
