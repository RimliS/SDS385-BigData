##Part-A
################################################################################
#y: data
#lambda:lambda
#theta: parameter
################################################################################

##Define the function:
f <- function(theta,y,lambda) {
  0.5*((y-theta)^2)+lambda*(abs(theta))
}

##Plot of the function to check that the function indeed obtains the minimum.
y <- 2
lambda <- 1
theta <- seq(-2,2,len=100)
plot(theta,f(theta,y,lambda),ty="l",main="y=2 & lambda=1")


##################################################################################
#The soft thresholding operator
#Function to solve the soft thresholding problem
#y The data value
#lambda The lambda value
#return The solution to the soft thresholding operator.
##################################################################################

S_y <- function(y, lambda) {  # Soft Thresholding Operator
  return((abs(y) - lambda) * sign(y) * (abs(y) > lambda))
}

lambda <- 10
y <- seq(-2,2,len=100)
plot(y,S_y(y,lambda),ty="l",main="lambda=10")



##################################################################################
##Part B Soft Thresholding
##################################################################################

set.seed(4001) ##Fixing seed

##Case-1: sigma^2 are all equal
#################################################################################
theta <- c(rep(2,5),rep(-2,5),rep(0,90)) ##Creating a sparse vector
sigma <- rep(0.2,100) ##Assuming sigma^2s are all equal
#z <- list(mode="vector",length=length(theta)) ##Initialize the data vector
z <- rep(0,length(theta))

##Loop for simulating one data point for each theta:
for (i in 1:length(theta)){
  z[i] <- rnorm(1,theta[i],sigma[i])
}

#z<-unlist(z, use.names=FALSE) ##converting from list to vector

theta_y <- function(y, lambda) {  # Soft Thresholding Operator
  return((abs(y) - lambda) * sign(y) * (abs(y) > lambda))
}


##Plotting predicted value of theta versus theta
plot(1:length(theta),theta_y(y=z, lambda=1.5*(sigma^2)),ty="l",col="red",
     ylab="Predicted theta",xlab="theta",main="Sparsity check with different lambda")
lines(1:length(theta),theta_y(y=z, lambda=2*(sigma^2)),ty="l",col="blue")
lines(1:length(theta),theta_y(y=z, lambda=4*(sigma^2)),ty="l",col="green")
legend('topright',c("lambda=1","lambda=2","lambda=4"),
       col=c("red","blue","green"),lty=1:2,lwd=1,cex = 0.5)


lambda <- 1.5 ##Initialize lambda value

##Predicted value of theta for different lambda values:
theta_yi <- theta_y(y=z, lambda=lambda*(sigma^2))

###Mean-squared error:
MSE_lambda <- (sum((theta_yi-theta)^2))/length(theta)

##MSE for different values of lambda:
vary.lambda <- seq(0,5,len=100)
MSE_lambda <- NULL

for (j in 1:length(vary.lambda)) {
  MSE_lambda[j] <- (sum(((theta_y(y=z, lambda=vary.lambda[j]*(sigma^2))-theta)^2)))/length(theta)
}

##Plotting of MSE for different lambda values:
plot(vary.lambda,MSE_lambda,ty="l",main="MSE vs lambda")
#####################################################################################

##Case-2: sigma^2 are not equal
#################################################################################
set.seed(123) ##Fixing seed

theta <- c(rep(2,5),rep(-2,5),rep(0,90)) ##Creating a sparse vector
sigma <- runif(100,1,10)
z <- rep(0,length(theta)) ##Initialize the data vector


##Loop for simulating one data point for each theta:
for (i in 1:length(theta)){
  z[i] <- rnorm(1,theta[i],sigma[i])
}


theta_y <- function(y, lambda) {  # Soft Thresholding Operator
  return((abs(y) - lambda) * sign(y) * (abs(y) > lambda))
}

lambda <- 0.5 ##Initialize lambda value

##Predicted value of theta for different lambda values:
theta_yi <- theta_y(y=z, lambda=lambda*(sigma^2))

###Mean-squared error:
MSE_lambda <- (sum((theta_yi-theta)^2))/length(theta)


##MSE for different values of lambda:
vary.lambda <- seq(0,5,len=100)
MSE_lambda <- NULL

for (j in 1:length(vary.lambda)) {
  MSE_lambda[j] <- (sum(((theta_y(y=z, lambda=vary.lambda[j]*(sigma^2))-theta)^2)))/length(theta)
}

##Plotting of MSE for different lambda values:
plot(vary.lambda,MSE_lambda,ty="l",main="MSE vs lambda")
#####################################################################################
