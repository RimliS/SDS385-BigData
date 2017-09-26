library('Matrix')
library('foreach')
library('glmnet')

##This package fits lasso and elastic-net model paths for regression, logistic and 
#multinomial regression using coordinate descent. The algorithm is extremely fast, and 
#exploits sparsity in the input x matrix where it exists.

set.seed(123)

N <- 1000
P <- 500

s <- trunc(P / 10)

X <- matrix(rnorm(N*P), N, P)
#X2 = Matrix(X, sparse = FALSE)

r <- sample(1:(N * P), size = N * P * 0.85, replace = FALSE)
X[r] <- 0

X2 = Matrix(X, sparse = TRUE)
beta <- rnorm(s)
Y <- X[, seq(s)] %*% beta + rnorm(N)

glmnet.fit1 <- glmnet(X2, Y)

full.times <- system.time(fit2 <- glmnet(X, Y))
sparse.times <- system.time(fit1 <- glmnet(X2, Y))
