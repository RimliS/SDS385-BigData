
##Part-C
##Call library
library(base)
library(Matrix)

##Initialize the values of N and P:
N=NULL
P=NULL

##Simulating design matrix, response variable and Weight matrix:
data<-function(N,P) {
  Y <- rnorm(N)
  Xmat <- matrix(rnorm(N*P),N,P)
  #W <- diag(runif(N),N,N)
  W <- diag(N)
  data.frame(Y=Y,Xmat=Xmat,W=W)
}

##Use the equation (X'WX)beta=X'WY:
   rdata <- data(1000,500)
    Y <- rdata[,1]
    X <- as.matrix(rdata[,2:501])
    W <- as.matrix(rdata[,502:1501])

    system.time({qr(X)})
    system.time({svd(X)})
    #It seems that QR decomposition is faster than SVD decompsition.
    
##Express in quadratic form:
  A <- t(X)%*%W%*%X
  b <- t(X)%*%W%*%Y
  
  
##QR Decomposition:
  result1 <- qr.solve(A, b)

##eigen(A)
##all(A >= 1)
##any(A < 0 )

##Application of SVD Decomposition:
  C<-svd(A)
  D <- diag(C$d)
  U <- C$u
  V <- C$v

  A <- U%*%D%*%t(V) ##Gives the original matrix
  InvA <- U%*%solve(D)%*%t(V) ##Inverse of A matrix

  result2 <- InvA%*%b
