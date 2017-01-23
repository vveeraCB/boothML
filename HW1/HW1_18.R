################
###Question 1.8### sin(2x) + 2
################

####set up train and test data set######
nTrain = 100                                   ## define train size
nTest  = 10000                                 ## define test size
set.seed(1)  
x_train = matrix( rnorm(nTrain*20), nTrain )    ## define train X
set.seed(2)  
x_test = matrix( rnorm(nTest*20), nTest )       ## define test X
set.seed(3)
y_train = sin(2*x_train[,1]) + 2 + rnorm(nTrain) ## define train Y
set.seed(4)
y_test = sin(2*x_test[,1]) + 2 + rnorm(nTest)    ## define test Y

train_data = data.frame(y_train,x_train)            ## define train dataframe
test_data = data.frame(y_test,x_test)               ## define test dataframe
names(test_data) = c("Y","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20")
names(train_data) = names(test_data)

# setting up jaxtaposed graphs of 2 by 2.
par(mfrow = c(4, 5), oma = c(2, 1, 0, 0),vmar = c(1, 2, 2, 1) )

####Part 5######
## data frames already built above. train_data <- data.frame(x_train,y_train), test_data <- data.frame(x_test,y_test) ##


kvec=2:15
nk=length(kvec)
outMSE = matrix(0,nk,20) #will put the out-of-sample MSE here for each k value

for(p in 1:20) {
    xnam <- paste0("X",1:p)
    for(i in 1:nk) {
      near = kknn(paste("Y ~ ", paste(xnam, collapse= "+")),train_data,test_data,k=kvec[i],kernel = "rectangular")
      MSE = mean((y_test - near$fitted)^2)
      outMSE[i,p] = MSE
    }
}

for(p in 1:20) {
    plot(log(1/kvec),outMSE[,p],pch=19, main=paste("P = ", p, sep=" "), col="orange", ylim = c(1,2))
    dftrain = data.frame(x_train[,1:p]); dftest = data.frame(x_test[,1:p])
    names(dftest) = names(dftrain)
    linear_reg <- lm(y_train~.,data=dftrain)                                 ## Calculate linear regression
    predictions = predict(linear_reg,dftest)
    MSE_linear = mean((y_test-predictions)^2)
    abline(MSE_linear,0,lty=2,lwd=2)                     
}

