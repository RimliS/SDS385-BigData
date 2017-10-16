##Part-A
################################################################################
#y: data
#lambda: tuning parameter
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

set.seed(123) ##Fixing seed

##Case-1: sigma^2 are all equal
#################################################################################
#theta <- c(seq(-2,2,len=50),rep(0,200),seq(1,3,len=50),rep(0,200))
theta <- c(rep(2,5),rep(-2,5),rep(0,90)) ##Creating a sparse vector
sigma <- rep(0.2,100) ##Assuming sigma^2s are all equal
z <- list(mode="vector",length=length(theta)) ##Initialize the data vector


##Loop for simulating one data point for each theta:
for (i in 1:length(theta)){
  z[i] <- rnorm(1,theta[i],sigma[i])
}

z<-unlist(z, use.names=FALSE) ##converting from list to vector

theta_y <- function(y, lambda) {  # Soft Thresholding Operator
  return((abs(y) - lambda) * sign(y) * (abs(y) > lambda))
}

lambda <- 5 ##Initialize lambda value

##Predicted value of theta for different lambda values:
theta_yi <- theta_y(y=z, lambda=lambda*(sigma^2))


##Plotting predicted value of theta versus theta
plot(1:length(theta),theta_yi,ty="h",main="lambda=1")

##Lambda=5
###Mean-squared error:
MSE_lambda <- (sum((theta_yi-theta)^2))/length(theta)
#0.009981807

##Different values of lambda:
vary.lambda <- seq(0,50,len=100)
MSE_lambda <- NULL

for (j in 1:length(vary.lambda)) {
  MSE_lambda[j] <- (sum(((theta_y(y=z, lambda=vary.lambda[j]*(sigma^2))-theta)^2)))/length(theta)
}

##Plotting of MSE for different lambda values:
plot(vary.lambda,MSE_lambda,ty="h")
#####################################################################################

##Case-2: sigma^2 are not equal
#################################################################################
set.seed(123) ##Fixing seed

theta <- c(rep(2,5),rep(-2,5),rep(0,90)) ##Creating a sparse vector
sigma <- runif(100,1,10) ##sigma^2s are not equal
z <- list(mode="vector",length=length(theta)) ##Initialize the data vector


##Loop for simulating one data point for each theta:
for (i in 1:length(theta)){
  z[i] <- rnorm(1,theta[i],sigma[i])
}

z<-unlist(z, use.names=FALSE) ##converting from list to vector

theta_y <- function(y, lambda) {  # Soft Thresholding Operator
  return((abs(y) - lambda) * sign(y) * (abs(y) > lambda))
}

lambda <- 0.5 ##Initialize lambda value

##Predicted value of theta for different lambda values:
theta_yi <- theta_y(y=z, lambda=lambda*(sigma^2))


##Plotting predicted value of theta versus theta
plot(1:length(theta),theta_yi,ty="l",main="lambda=0.5")

##Lambda=0.5
###Mean-squared error:
MSE_lambda <- (sum((theta_yi-theta)^2))/length(theta)


##Different values of lambda:
vary.lambda <- seq(0,5,len=100)
MSE_lambda <- NULL

for (j in 1:length(vary.lambda)) {
  MSE_lambda[j] <- (sum(((theta_y(y=z, lambda=vary.lambda[j]*(sigma^2))-theta)^2)))/length(theta)
}

##Plotting of MSE for different lambda values:
plot(vary.lambda,MSE_lambda,ty="l",main="MSE vs lambda")
#####################################################################################
