#feature scaling
FScale<-function(x)
{
for(i in 2:ncol(x))
	x[,i] <- ((x[,i] - mean(x[,i]))/sd(x[,i]))
return(x)
}

