##Import dataset containing predictors:
diabetesX <- read.table("/Users/rimlisengupta/Documents/SDS385/Ex-5-Data/DiabetesX.txt",
                        header=T,sep=',')

##Import dataset containing response:
diabetesY <- read.table("/Users/rimlisengupta/Documents/SDS385/Ex-5-Data/DiabetesY.txt")

#####################################################################################
#Combining predictors and response into a single dataset
diabetes <- data.frame(diabetesX)
diabetes$Y <- diabetesY$V1

### Get libraries:
library(glmnet)
library(MASS)

###############################################################################################
##Part B: lasso Model fit by cv.glmnet:
set.seed(1)

##Split the dataset into training and test dataset
ind = sample(1:2, size = nrow(diabetes), replace = TRUE)

test_index =  which(ind == 1)
train      =  diabetes[ -test_index, ] 
test       =  diabetes[  test_index, ]

fit.lasso <- cv.glmnet(x = as.matrix(train[,-65]), y = train$Y, type.measure="mse",
                       nfolds=5, alpha=1)


#################################################################################################
##CP Mallows:
lasso_cv = cv.glmnet( x = as.matrix(diabetes[,-65]), y = diabetes$Y, alpha = 1)


lambda <- lasso_cv$glmnet.fit$lambda
s_lambda <- lasso_cv$glmnet.fit$df
x.matrix <- cbind(rep(1,length(diabetes$Y)), as.matrix(diabetes[,-65]))

lasso_pred_y <- rep(0,length(diabetes$Y))
MSE_beta.lambda <- rep(0,length(lambda))
beta_hat <- rep(0,length(lambda))
sigma_lambda.sq <- rep(0,length(lambda))
Cp_lambda <- rep(0,length(lambda))

for (i in 1:length(lambda)) {
  lasso_pred_y = predict(lasso_cv, newx =as.matrix(diabetes[,-65]), s = lambda[i])
  MSE_beta.lambda[i] <- mean((diabetes$Y-lasso_pred_y)^2)
  beta_hat <- coef(lasso_cv, s=lambda[i])
  beta_hat <- as.matrix(beta_hat)
  beta_hat <- beta_hat[,1]
  sigma_lambda.sq[i] <- sum((diabetes$Y-(x.matrix%*%beta_hat))^2)/
    (length(diabetes$Y)-s_lambda[i])
  Cp_lambda[i] <- MSE_beta.lambda[i] + ((2*s_lambda[i]*sigma_lambda.sq[i])/length(diabetes$Y))
}

plot(log(lambda),Cp_lambda,ty='l',main="Mallow's CP")

#################################################################################################

##Part A: lasso Model fit by glmnet:
lasso_fit = glmnet( x = as.matrix(diabetes[,-65]), y = diabetes$Y, alpha = 1)


##Plot of MSE for different values of lambda
lambda <- lasso_fit$lambda
lasso_pred_y <- rep(0,length(diabetes$Y))
meansq <- rep(0,length(lambda))

for (i in 1:length(lambda)) {
  lasso_pred_y = predict(lasso_fit, newx =as.matrix(diabetes[,-65]), s = lambda[i])
  meansq[i] <- mean((diabetes$Y-lasso_pred_y)^2)
}

plot(fit.lasso,lwd=2, lty=1,ylim=c(2500,6000),ylab="MSE,CV error and CP",xlim=c(-4,4))
lines(log(lambda),meansq,ty='l',lwd=1, lty=2,col="blue")
lines(log(lambda),Cp_lambda,ty='l',col="purple")
legend('topleft',c("5-fold CV Error","in-sample MSE","CP"),col=c("red","blue","purple"),lty=1:2,lwd=1,cex = 0.5)
mtext("Comparison between MSE,CV and CP", side=3, line=2, cex.lab=1,las=1, col="blue")
#################################################################################################
