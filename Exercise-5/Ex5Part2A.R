#####################################################################################
##Part LASSO

##Import dataset containing predictors:
diabetesX <- read.table("/Users/Documents/SDS385/Ex5-Data/DiabetesX.txt",header=T,sep=',')

##Import dataset containing response:
diabetesY <- read.table("/Users/Documents/SDS385/Ex5-Data/DiabetesY.txt")

#####################################################################################
#Combining predictors and response into a single dataset
diabetes <- data.frame(diabetesX)
diabetes$Y <- diabetesY$V1

### Get libraries:
library(glmnet)
library(MASS)

#set.seed(101)

##lasso Model fit:
lasso_fit = glmnet(x = as.matrix(diabetes[,-65]), y = diabetes$Y, alpha = 1)

##Predicted values of beta:
beta_predicted <- coef(lasso_fit,s=0.1) ##s=Shrinkage Factor: t/sum(abs(beta_hat))

#beta_hat <- as.matrix(beta_predicted)
#beta_hat <- beta_hat[,1]

##Pot
plot(lasso_fit, xvar ="lambda")
mtext("Beta_hat vs log(lambda)", side=3, line=2, cex.lab=1,las=1, col="blue")

##Predicted value of y:
lasso_pred_y = predict(lasso_fit, newx =as.matrix(diabetes[,-65]), s=c(0.01,0.1))
tab <- cbind(diabetes$Y,lasso_pred_y)

##MSE
mse = function(a,b) (mean((a-b)^2)) #function defined for MSE

##MSE for s=0.01
meansq1 <- mse(tab[,1], tab[,2])

##MSE for s=0.1
meansq2 <- mse(tab[,1], tab[,3])
