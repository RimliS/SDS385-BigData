#Reading data
text <- read.csv("/Users/rimlisengupta/Dropbox/Bigdata final project/text3.csv")

#Random Forest:
#Converting to factor
y1.stars <- as.factor(text$stars)
topic <- text$topic

data=data.frame(text$doc_id, y1.stars,topic)

#Splitting into train and test dataset:
set.seed(2)
smp_size <- floor(0.5 * nrow(data))
ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[ind, ]
test <- data[-ind, ]

stars.train=train$y1.stars
stars.test=test$y1.stars

##Model: stars~topic:
library(ranger)
rf <- ranger(y1.stars ~ topic, data = train, num.trees = 20, write.forest=TRUE) 
yhat <- predict(rf, data=train)$predictions
tab1 <- table(yhat, stars.train)
sum(yhat!= stars.train)/length(yhat)
#train.error = 0.545
             
yhat <- predict(rf, data=test)$predictions
tab2 <- table(yhat, stars.test)
sum(yhat!= stars.test)/length(yhat)
#test.error = 0.56

#Converting stars to dummy:
text$stars.new[text$stars <= 3] <- 0
text$stars.new[text$stars > 3] <- 1


#SGD package:
library(sgd)
sgd.fit <- sgd(stars.new ~ topic, model="glm",data=df,
               model.control=list(family = binomial(link="logit")),
               lr.control=c(scale=1, gamma=1, alpha=0.01, c=10))
sgd.fit$coefficients

#SGD
#Sigmoid function
wi <- function(z){1/(1+exp(-z))}

#Define the 'hypothesis function'
h <- function (x,theta) {
  return(wi(x %*% t(theta)))
} 

W <- rep(1,n) #weight
X=as.matrix(df$topic) #design matrix
y=df$stars.new #response

p <- ncol(X)+1
n <- nrow(X)

#SGD
step.size <- function(C,t,t0,a){1/(C*(t+t0)^(-a))}
alpha <- 0.01

betas.all <- NULL
ls        <- NULL
for (h in 1:length(alpha)){
  betas <- NULL
  l <- NULL
  beta0 <- as.matrix(rep(0,p),p,1)
  for (i in 1:20000){
    for (k in 1:10){
      j <- sample(1:n,1)
     beta0 <- beta0 -
       step.size(10,(i-1)*10+k,1,alpha[h])*W[j]*(wi(sum(X[j,]*beta0))-y[j])*X[j,]
    }
    betas <- rbind(betas,t(beta0))
    l <- c(l)
  }
  betas.all[[h]] <- betas
}


#Convergence check:
x=as.matrix(df$topic)
y=df$stars.new
m=nrow(x)
theta=c(betas[1],betas[2])

cost <- t(mat.or.vec(1,m))
for(i in 1:m) {
  cost[i,1] <-  1/m * sum(-y[i] * log(h(x[i,],theta)) - 
                            (1 - y[i]) * log(1 - h(x[i,],theta))) 
}

#iteration: 20000
totalCost <- colSums(cost)
cost20000=3.56
#3.96

#iteration: 20001
totalCost <- colSums(cost)
cost20001=4.01
#4.01

print(cost20000 - cost20001)##Difference
#-0.05
