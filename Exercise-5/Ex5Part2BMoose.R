##PartB MOOSE

##Import dataset containing predictors:
diabetesX <- read.table("/Users/rimlisengupta/Documents/SDS385/Ex-5-Data/DiabetesX.txt",
                        header=T,sep=',')

##Import dataset containing response:
diabetesY <- read.table("/Users/rimlisengupta/Documents/SDS385/Ex-5-Data/DiabetesY.txt")

diabetes <- data.frame(diabetesX)
diabetes$Y <- diabetesY$V1

### Get libraries:
library(glmnet)
library(MASS)

set.seed(3)

##Split the dataset into training and test dataset
ind = sample(1:2, size = nrow(diabetes), replace = TRUE)

test_index =  which(ind == 1)
train  =  diabetes[ -test_index, ] 
test   =  diabetes[  test_index, ] 

##lasso Model fit:
lasso_cv = cv.glmnet( x = as.matrix(train[,-65]), y = train$Y, alpha = 1)
lasso_pred_y=predict(lasso_cv, newx =as.matrix(train[,-65]), s = lasso_cv$lambda.min)

##MSE:
mse = function(a,b) (mean((a-b)^2))
meansq_train <- mse(train$Y, lasso_pred_y)

### 5 fold cross validation:
###
no_of_folds = 5

index_values = sample(1:no_of_folds, size = dim(train)[1], replace = TRUE)

tmp_mse_lasso   = rep(0, no_of_folds)

for (j in 1:no_of_folds)
{
  index_out = which(index_values == j)                             
  left_out_data = train[  index_out, ]                                
  left_in_data  = train[ -index_out, ]   
  
  
  tmp_lasso = glmnet(x = as.matrix(left_in_data[,-65]), y = left_in_data$Y, alpha = 1)     
  tmp_lasso_y = predict(tmp_lasso, newx = as.matrix(left_out_data[,-65]),s = mean(tmp_lasso$lambda))
  tmp_mse_lasso[j] = mse(left_out_data$Y, tmp_lasso_y) 
  
}


#Plot test error
lambda <- tmp_lasso$lambda
lasso_yhat <- rep(0,length(test$Y))
moose_error <- rep(0,length(lambda))

for (i in 1:length(lambda)) {
  lasso_yhat = predict(tmp_lasso, newx =as.matrix(test[,-65]), s = lambda[i])
  moose_error[i] <- mean((test$Y-lasso_yhat)^2)
}

plot(log(lambda),moose_error,ty='h',xlim=c(-2,4),ylim=c(3200,6500),ylab="Test Error", main="Test Error - 5-fold Cross-Validation")

