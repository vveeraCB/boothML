################
###Question 1###
################

####Part 1######
set.seed(1)
x_train<-rnorm(1:100)
set.seed(2)
epsilon_train <- rnorm(1:100)

y_train <- 1.8*x_train+2+epsilon_train  # train set
y_train_no_error <- 1.8*x_train+2

set.seed(3)
x_test <- rnorm(1:10000)
set.seed(4)
epsilon_test <- rnorm(1:10000)

y_test <- 1.8*x_test+2+epsilon_test     # test set

# setting up jaxtaposed graphs of 2 by 2.
par(mfrow=c(2,2))

####Part 2######
plot(x_train,y_train, main="Training Set: y vs. x", ylab = "Y Data Set", xlab = "X Data Set", col="orange",pch=19) 
abline(a=2,b=1.8,col="black",lwd=3)

####Part 3######
linear_model = lm(y_train~x_train)
abline(linear_model,col="blue",lwd=3, lty=2)

####Part 4######
  ## instructor comments below ##
  ## 1.4 -- Plot training data and three fits obtained using training data (linear and kNN with k=2 and k=12)
  ## 1.5 -- Here you are using the test set to evaluate performance of models fitted on the training set in 1.4

  ## make sure package is downloaded. 
library(kknn)
train_data <- data.frame(x=x_train,y=y_train)
test_data <- data.frame(x=x_test,y=y_test)
train_data_sorted <- train_data[order(train_data$x),]
kf2 = kknn(y~x,train_data,train_data_sorted,k=2,kernel = "rectangular")
kf3 = kknn(y~x,train_data,train_data_sorted,k=3,kernel = "rectangular")
kf12 = kknn(y~x,train_data,train_data_sorted,k=12,kernel = "rectangular")

## plot changes based on instructor's comments on Piazza. 
plot(x_train,y_train, main="Training Set: true Vs knn(k=2)", ylab = "Y Training", xlab = "X Training", col="orange",pch=19) 
abline(a=2,b=1.8,col="black",lwd=3) # true relationship has slope 1.8 and intercept 2
lines(train_data_sorted$x,kf2$fitted,col="pink",lwd=2)

plot(x_train,y_train, main="Training Set: true Vs knn(k=12)", ylab = "Y Training", xlab = "X Training", col="orange",pch=19) 
abline(a=2,b=1.8,col="black",lwd=3) # true relationship has slope 1.8 and intercept 2
lines(train_data_sorted$x,kf12$fitted,col="magenta",lwd=2)

####Part 5######
## data frames already build above. train_data <- data.frame(x_train,y_train), test_data <- data.frame(x_test,y_test) ##


kvec=2:15
nk=length(kvec)
outMSE = rep(0,nk) #will put the out-of-sample MSE here for each k value

for(i in 1:nk) {
  near = kknn(y~x,train_data,test_data,k=kvec[i],kernel = "rectangular")
  MSE = mean((test_data$y-near$fitted)^2)
  outMSE[i] = MSE
}

linear_reg = lm(y~x, data=train_data)
testPredictors <- data.frame(x = test_data$x)
linear_prediction = predict(linear_reg,newdata = testPredictors)
MSE_linear = mean((test_data$y-linear_prediction)^2)


imin = which.min(outMSE)
plot(log(1/kvec),outMSE,ylim = c(1,2))
abline(MSE_linear,0,col='black',lwd=3,lty=2)
cat("best k is ",kvec[imin],"\n") 
