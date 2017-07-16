#ones ini
ones_ini <- function(train){
  x1 <- as.matrix(feature1)
  x2 <- as.matrix(feature2)
  x_temp <- as.matrix(cbind(x1,x2))
  ones <- as.matrix(rep.int(1,nrow(train)))
  x <- as.matrix(cbind(ones,x_temp))
  rm(x1,x2,x_temp,ones)
  return(x)
}
