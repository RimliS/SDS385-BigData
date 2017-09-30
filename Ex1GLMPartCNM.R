
##Part C Application of Newton Method on Logistic Regression:
#Define sigmoid
g = function (z) {
  return (1 / (1 + exp(-z) ))
} 

#Define hypothesis 
h = function (x,theta) {
  return( g(x %*% theta) )
}

#Define cost
J = function (x,y,theta,m) {
  return( 1/m * sum(-y * log(h(x,theta)) - (1 - y) * log(1 - h(x,theta))) )
}

#Derivative of J (gradient)
grad = function (x,y,theta,m) {
  return( 1/m * t(x) %*% (h(x,theta) - y))
}

#Define Hessian
H = function (x,y,theta,m) {
  return (1/m * t(x) %*% x * diag(h(x,theta)) * diag(1 - h(x,theta)))
}


##Call library:
library("sas7bdat")
data=read.sas7bdat("/Users/rimlisengupta/Dropbox/SDS385/Data/Herring.sas7bdat")

###Data manipulations:
new_data <- data[,c("sga10","mbp_log_adj","mibp_log_adj","mep_log_adj","mbzp_log_adj","mcpp_log_adj","DEHP_log_adj")]
temp1 <- na.omit(new_data) ##Omitting missing values from the dataset


##Design matrix:
xx <- temp1[,c("mbp_log_adj","mibp_log_adj","mep_log_adj","mbzp_log_adj","mcpp_log_adj","DEHP_log_adj")] # original x-values

##Standardize the values of predictors
x1<-scale(xx)

##The number of rows of design matrix
m <- nrow(x1)

##Including 1 into design matrix
x0 <- rep(1,m) # column of 1's
x <- as.matrix(cbind(x0,x1))

##Response
y <- temp1[,1]
y <- matrix(y)

##Define number of iterations
j = array(0,c(10,1))
theta = matrix(0,7)

# iterate 
# It seems that Newton's method converges fast, 10 is enough
for (i in 1:10) {
  j[i] = J(x,y,theta,m) # stores each iteration Cost function
  theta = theta - solve(H(x,y,theta,m)) %*% grad(x,y,theta,m) 
}


plot(j, xlab="iterations", ylab="cost J",type="l",main="Convergence check")

