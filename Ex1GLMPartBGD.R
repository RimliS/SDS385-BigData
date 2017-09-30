
##Call library:
library("sas7bdat")
data=read.sas7bdat("/Users/rimlisengupta/Dropbox/SDS385/Data/Herring.sas7bdat")

###Data manipulations:
new_data <- data[,c("sga10","mbp_log_adj","mibp_log_adj","mep_log_adj","mbzp_log_adj","mcpp_log_adj","DEHP_log_adj")]
temp1 <- na.omit(new_data)##Omitting missing values from the dataset (complete case analysis)


##Design matrix
x <- temp1[,c("mbp_log_adj","mibp_log_adj","mep_log_adj","mbzp_log_adj","mcpp_log_adj","DEHP_log_adj")]
x.sc <- scale(x) ##Standardize
y <- temp1[,1] ##Response


##GLM Estimates:
fit <- glm(y~x.sc,family=binomial)
theta_glm <- coef(fit)


#x <- as.matrix(x)
x.sc <- as.matrix(x.sc)

##Number of iterations:
num.iterations <- 500

y <- matrix(y)
m <- nrow(x.sc)
x.sc <- cbind(rep(1, m), x.sc) ##Including 1 in design matrix
num.features <- ncol(x.sc)
theta <- matrix(rep(0, num.features), nrow=1)
theta.path <- theta

# Gradient descent function
grad <- function(x, y, theta) {
  gradient <- (1 / nrow(y)) * t(x) %*% ((1 / (1 + exp(-x %*% t(theta)))) - y)
  return(t(gradient))
}

gradient.descent <- function(x,y,alpha=0.9,num.iterations=500,threshold=1e-5,output.path=FALSE) {
  
  for (i in 1:num.iterations) {
    theta <- theta - alpha * grad(x, y, theta)
    if(all(is.na(theta))) break
    theta.path <- rbind(theta.path, theta)
    if(i > 2) if(all(abs(theta - theta.path[i-1,]) < threshold)) break 
  }
  
  if(output.path) return(theta.path) else return(theta.path[nrow(theta.path),])
}

#unscaled.theta <- gradient.descent(x=x, y=y, num.iterations=num.iterations, output.path=TRUE)
scaled.theta <- gradient.descent(x=x.sc, y=y, num.iterations=num.iterations, output.path=TRUE)
theta_hat <- c(mean(scaled.theta[,1]),mean(scaled.theta[,2]),mean(scaled.theta[,3]),mean(scaled.theta[,4]),
               mean(scaled.theta[,5]),mean(scaled.theta[,6]),mean(scaled.theta[,7]))

result <- cbind(theta_glm, theta_hat)


library(ggplot2)
library(gridExtra)

plot1<-qplot(1:(nrow(scaled.theta)), scaled.theta[,1], geom=c("line"), xlab="iteration", ylab="theta_1")
plot2<-qplot(1:(nrow(scaled.theta)), scaled.theta[,2], geom=c("line"), xlab="iteration", ylab="theta_2")
grid.arrange(plot1, plot2, ncol=2)

plot3<-qplot(1:(nrow(scaled.theta)), scaled.theta[,3], geom=c("line"), xlab="iteration", ylab="theta_3")
plot4<-qplot(1:(nrow(scaled.theta)), scaled.theta[,4], geom=c("line"), xlab="iteration", ylab="theta_4")
grid.arrange(plot3, plot4, ncol=2)

plot5<-qplot(1:(nrow(scaled.theta)), scaled.theta[,5], geom=c("line"), xlab="iteration", ylab="theta_5")
plot6<-qplot(1:(nrow(scaled.theta)), scaled.theta[,6], geom=c("line"), xlab="iteration", ylab="theta_6")
plot7<-qplot(1:(nrow(scaled.theta)), scaled.theta[,7], geom=c("line"), xlab="iteration", ylab="theta_7")
grid.arrange(plot5, plot6, plot7, nrow=3)

