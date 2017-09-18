#Imports 260.7 0.935477
#requirements
source("featScaling.r")
source("oracle.R")
source("eq.r")
source("conv.r")
source("gd.r")

#Stats
filename <- "2010.csv"
#creates a quadratic regression model
  detach(datavar)
  datavar <- read.csv(filename)
  rm(filename)
  attach(datavar)
  #feature 1 Sqrt
  #feature 2 square
  feature1 <- datavar$poverty_people_rural
  feature2 <- datavar$poverty_people_urban

#ones function
ones_ini <- function(train){
  x1 <- as.matrix(feature1)
  x2 <- as.matrix(feature2)
  x_temp <- as.matrix(cbind(x1,x2))
  ones <- as.matrix(rep.int(1,nrow(train)))
  x <- as.matrix(cbind(ones,x_temp))
  rm(x1,x2,x_temp,ones)
  return(x)
}
x <- ones_ini(datavar)
y <- as.matrix(Incidences)

x.scaled_l <- FScale(x)
#x.scaled
#Regression

alpha <- 0.3
num_iter <- 700
x_q <- polyMat(x)
#scalling
x.scaled_q <- FScale(x_q)
rm(theta)
theta <- as.matrix(rep.int(0,ncol(x.scaled_q))) #need to take input as x
rm(J_hist)
#regression
conv <- gd(x.scaled_q,y,alpha,num_iter)
#unlist theta and J_hist
theta_temp <- as.matrix(conv[1]) #conv is the object made from the gd function
J_hist_temp <- as.matrix(conv[2]) #conv is the object made from the gd function
theta_q <- unlist(theta_temp)
J_hist_q <- unlist(J_hist_temp)
rm(theta_temp,J_hist_temp,conv,alpha,num_iter)

alpha <- 0.3
num_iter <- 700
x.scaled_l <- FScale(x)
rm(theta)
theta <- as.matrix(rep.int(0,ncol(x.scaled_l))) #need to take input as x
rm(J_hist)
conv <- gd(x.scaled_l,y,alpha,num_iter)
#unlist theta and J_hist
theta_temp <- as.matrix(conv[1]) #conv is the object made from the gd function
J_hist_temp <- as.matrix(conv[2]) #conv is the object made from the gd function
theta_l <- unlist(theta_temp)
J_hist_l <- unlist(J_hist_temp)
rm(theta_temp,J_hist_temp,conv,alpha,num_iter)

#diagnosis
  # theta
  # idealModel <- lm(I(y) ~ I(x.scaled_q[,2]) + I(x.scaled_q[,3]) + I(x.scaled_q[,4]))
  # idealModel
  # summary(idealModel)
  # pred_r_squared(idealModel)
  # linearModel <- lm(y ~ x.scaled_l[,2] + x.scaled_l[,3])
  # linearModel
  # summary(linearModel)
  # pred_r_squared(linearModel)

