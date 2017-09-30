
##Call library:
library("sas7bdat")
data=read.sas7bdat("/Users/rimlisengupta/Dropbox/SDS385/Data/Herring.sas7bdat")

###Data manipulations:

new_data <- data[,c("sga10","mbp_log_adj","mibp_log_adj","mep_log_adj","mbzp_log_adj","mcpp_log_adj","DEHP_log_adj")]
temp1 <- na.omit(new_data)


xx <- temp1[,c("mbp_log_adj","mibp_log_adj","mep_log_adj","mbzp_log_adj","mcpp_log_adj","DEHP_log_adj")] # original x-values
x1<-scale(xx)
m <- nrow(x1)
x0 <- rep(1,m) # column of 1's
x <- as.matrix(cbind(x0,x1))
y <- temp1[,1]

y <- matrix(y)

# Gradient descent function
grad <- function(x, y, theta) {
  gradient <- (1 / nrow(y)) * t(x) %*% ((1 / (1 + exp(-x %*% t(theta)))) - y)
  return(t(gradient))
}

#Define gradient descent update algorithm
grad.descent <- function(x, maxit){
  num.features <- ncol(x)
  theta <- matrix(rep(0, num.features), nrow=1) # Initialize the parameters
  
  alpha = 0.1 # set learning rate
  for (i in 1:maxit) {
    theta <- theta - alpha  * grad(x, y, theta)   
  }
  return(theta)
}

#Results with feature scaling
print(grad.descent(x,1000))

#Get results from gradient descent
beta <- grad.descent(x,1000)

#Sigmoid
g <- function (z) {
  return (1 / (1 + exp(-z)))
}


#Define the 'hypothesis function'
h <- function (x,theta) {
  return(g(x %*% t(theta)))
} 

#Convergence check:

cost <- t(mat.or.vec(1,m))
for(i in 1:m) {
  cost[i,1] <-  1/m * sum(-y[i,] * log(h(x[i,],beta)) - (1 - y[i,]) * log(1 - h(x[i,],beta))) 
}


totalCost <- colSums(cost)
print(totalCost)

#Save this as Cost1000
cost1000 <- totalCost

#Change iterations to 1001
beta <- grad.descent(x,1001)

cost <- t(mat.or.vec(1,m))
for(i in 1:m) {
  cost[i,1] <-  1/m * sum(-y[i,] * log(h(x[i,],beta)) - (1 - y[i,]) * log(1 - h(x[i,],beta))) 
}


totalCost <- colSums(cost)
print(totalCost)

#Save this as Cost1001
cost1001 <- totalCost
print(cost1000 - cost1001)##Difference

