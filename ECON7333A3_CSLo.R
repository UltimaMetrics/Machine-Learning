
#ECON7333 Assignment 3 
setwd("E:/UQ/2019 S2/ECON7333 Big Data and ML/Assignment 3") #Set path
Assign3=read.csv("Assign3.csv") #Read Assign2.csv file
attach(Assign3) #The database Assign3 is attached to the R search path


# (Q1)
#Fit a linear regression model 
linfit=lm(y~x) #Run the linear regression
coef(summary(linfit)) #Regression table of linear regression

pred.linfit=predict(linfit) #Make prediction based on the regression
plot(x,y,pch=13) #Scatter plot
points(x, pred.linfit, col="deeppink2",  pch=25)#Add a connecting points from predicted value

#(Q2)
#Fit a quadratic regression model
quadfit=lm(y~x+I(x^2)) #Run the quadratic regression
coef(summary(quadfit))#Regression table of quadratic regression

pred.quadfit=predict(quadfit) #Make prediction based on the regression
plot(x,y, pch=5) #Scatter plot
points(x, pred.quadfit, col="mediumspringgreen", pch=17) #Add a connecting points from predicted value

#(Q3)
#(Q3-A)
#Use two different method
#First method
#Regression with step functions
min.x=min(x) #Find the mini x
max.x=max(x) #Find the max x
n=1 #Let n=1
U=runif(n, min.x, max.x) #draw random number with uniform distribution
breaks=c(min.x, U, max.x) #Set the breaks into three parts: min, random number, and max
cut.x=cut(x, breaks, include.lowest=TRUE, dig.lab=7) #divides the range of x into intervals and codes the values in x
step.fit=lm(y~cut.x-1 , data=Assign3) #Run the regression 
coef(summary(step.fit)) #Output the regression coefficient
predict.step.fit=predict(step.fit) #Predict from the regression
predict.step.fit
plot(Assign3$x, Assign3$y, pch=4) #Scatter plot
points(Assign3$x, predict(step.fit), col="dodgerblue2") #Add a connecting points from predicted value

#Second method 
Assign3$alpha1 <- ifelse(U <= Assign3$x, 1, 0) # ifelse function returns 1 for alpha 1 if U<=X; otherwise zero
Assign3$alpha2 <- ifelse(U >  Assign3$x, 1, 0) # ifelse function returns 1 for alpha 2 if U>X; otherwise zero
step.fit2 <- lm(y ~ alpha1 + alpha2 -1, data = Assign3) #Run the regression with alpha1 and alpha2 as regressor, 
#and minus 1 so then intercept is removed
coef(summary(step.fit2)) #Present the regression coefficients
predict.step.fit2=predict(step.fit2) #Estimate the predicted values from the regression
predict.step.fit2
plot(Assign3$x, Assign3$y, pch=4) #Scatter plot
points(Assign3$x, predict(step.fit2), col="maroon1") #Add a connecting points from predicted value

#(Q3-B)
#Create three different plots
#3 runs with set seed
#Below is the function which takes as input x and the data and give as f hat x
Q3B=function(X,Y){ #Start a function
  z1=min(X) #Name z1 as the min of x
  z2=max(X) #Name z2 as the max of x
  U3=runif(1, min=z1, max= z2) #Run a random number with uniform distribution within min and max
  breaks=c(min=z1, U3, max= z2) #A generic function that contains cutpoints
  cut.x1=cut(X, breaks, include.lowest=TRUE, dig.lab=7) #Use the cut function to return ordered categorical variable
  Q3B_fit=lm(y~cut.x1-1 , data=Assign3) #Run the regression
  plot(X,Y, pch=13, col='lightpink2') #Scatter plot
  lines(sort(X), fitted(Q3B_fit)[order(X)],lty=6, pch=11, col='khaki', lwd=5) #Add the line from the regression
  
}
Q3B(X=Assign3$x,Y=Assign3$y) #Output the function of Q3B

#(Q3-C)
#Three runs-plot
#par(mfrow = c(3, 1)) #Plot into a 3 x 1 array
#First Run
set.seed(39)
func3.1=function(X,Y){ #Start a function
  z1=min(X) #Name z1 as the min of x
  z2=max(X) #Name z2 as the max of x
  U3=runif(1, min=z1, max= z2) #Run a random number with uniform distribution within min and max
  breaks=c(min=z1, U3, max= z2) #A generic function that contains cutpoints
  cut.x1=cut(X, breaks, include.lowest=TRUE, dig.lab=7) #Use the cut function to return ordered categorical variable
  fit.3=lm(y~cut.x1-1 , data=Assign3)  #Run the regression
  plot(X,Y, pch=13, col='lightpink2')  #Scatter plot
  lines(sort(X), fitted(fit.3)[order(X)],lty=6, pch=11, col='khaki', lwd=5) #Add the line from the regression
  
}
func3.1(X=Assign3$x,Y=Assign3$y) #Output the function of func3.1

#Second Run
set.seed(702)
func3.2=function(X,Y){ #Start a function
  z1=min(X) #Name z1 as the min of x
  z2=max(X) #Name z2 as the max of x
  U3=runif(1, min=z1, max= z2) #Run a random number with uniform distribution within min and max
  breaks=c(min=z1, U3, max= z2) #A generic function that contains cutpoints
  cut.x1=cut(X, breaks, include.lowest=TRUE, dig.lab=7) #Use the cut function to return ordered categorical variable
  fit.3=lm(y~cut.x1-1 , data=Assign3) #Run the regression
  plot(X,Y, pch=13, col='lightpink2') #Scatter plot
  lines(sort(X), fitted(fit.3)[order(X)], lty=6, pch=11,col='lightslateblue', lwd=5)#Add the line from the regression
  
}
func3.2(X=Assign3$x,Y=Assign3$y) #Output the function of func3.2


#Third Run
set.seed(1037)
func3.3=function(X,Y){ #Start a function
  z1=min(X) #Name z1 as the min of x
  z2=max(X) #Name z2 as the max of x
  U3=runif(1, min=z1, max= z2) #Run a random number with uniform distribution within min and max
  breaks=c(min=z1, U, max= z2) #A generic function that contains cutpoints
  cut.x1=cut(X, breaks, include.lowest=TRUE, dig.lab=7) #Use the cut function to return ordered categorical variable
  fit.3=lm(y~cut.x1-1 , data=Assign3) #Run the regression
  plot(X,Y, pch=13,col='lightpink2') #Scatter plot
  lines(sort(X), fitted(fit.3)[order(X)], lty=6, pch=11,col='navyblue', lwd=5) #Add the line from the regression
  
}
func3.3(X=Assign3$x,Y=Assign3$y)#Output the function of func3.3

#(Q4) 
#Q4 A and B combined codes
X=Assign3$x #Rename x to X
Y=Assign3$y #Rename y to Y
boosting = function(X,Y,lambda,B){ #Starts a function
  library(gbm)
  #load GBM package designed for Generalized Boosted Regression Models
  MSE=rep(0,B) #Create a repeating vector for storing MSE
   #Set the breaks into three parts: min, random number, and max
  #Use the cut function to return ordered categorical variable
  for (b in 1:B){  #Sets a for loop and let the index variable within 1 to B
    boosting.fit=gbm(Y~cut.x-1, distribution="gaussian", n.trees=b, shrinkage=lambda)
    #Run boosted regression with normal distribution
    predict.Y=predict( boosting.fit, n.trees=b)      #Predict the value of boosted regression
    MSE[b]=mean((Y-predict.Y)^2) #Estimate the Mean Square Error
  }
  par(mfrow=c(1,2)) #set the plots into a 1 x 2 array
  Z=predict( boosting.fit, n.trees=B) #Name this variable as prediction from boosted regression
  plot(X,Y, col='darkslateblue',  pch=10) #Scatter plot
  lines(sort(X), Z[order(X)], col='firebrick1', type="l", lty=4, lwd=3, pch=16)
  #Line showing the prediction from boosting
  plot(MSE, xlab="B", col='azure4',pch=12, lwd=0.72 )
  #Plotting MSE agianst the number of iterations
}

boosting(X,Y, lambda=0.01, B=100) #Output the boosting with learning rate and B as inputs where the learning rate sets at 0.01 and iteration sets at 100
boosting(X,Y, lambda=0.01, B=1000) #Output the boosting with learning rate and B as inputs where the learning rate sets at 0.01 and iteration sets at 1000
boosting(X,Y, lambda=0.01, B=10000) #Output the boosting with learning rate and B as inputs where the learning rate sets at 0.01 and iteration sets at 10000

#(Q4-B)
#Plot training MSE vs the number of iterations
#Same as above

#(Q4-C) 
train1=sample(1:nrow(Assign3), 50) #draw 50 random number from assign3 using sample function
X.train=X[train1] #Set training data for X
Y.train=Y[train1] #Set training data for Y
train=data.frame(X.train, Y.train) #Group training data with data.frame function
test4c=Assign3[-train1] #Set testing data 
test.4c=Assign3[-train1,"y"] #Set testing data 
boosttest=gbm(Y~cut.x-1, train, distribution="gaussian", n.trees=2500, shrinkage=0.01) #Run GBM regression
predict.c=predict(boosttest, test4c, n.trees=2500) #Make predicted value
MSE.4c=mean((predict.c-test.4c)^2) #Calculate MSE
MSE.4c

train1=sample(1:nrow(Assign3), 50) #draw 50 random number from assign3 using sample function
X.train=X[train1] #Set training data for X
Y.train=Y[train1] #Set training data for Y
train=data.frame(X.train, Y.train) #Group training data with data.frame function
test4c=Assign3[-train1] #Set testing data 
test.4c=Assign3[-train1,"y"] #Set testing data 
boosttest=gbm(Y~cut.x-1, train, distribution="gaussian", n.trees=5000, shrinkage=0.01) #Run GBM regression
predict.c=predict(boosttest, test4c, n.trees=5000) #Make predicted value       
MSE.4c=mean((predict.c-test.4c)^2) #Calculate MSE
MSE.4c  

train1=sample(1:nrow(Assign3), 50) #draw 50 random number from assign3 using sample function
X.train=X[train1] #Set training data for X
Y.train=Y[train1] #Set training data for Y
train=data.frame(X.train, Y.train) #Group training data with data.frame function
test4c=Assign3[-train1] #Set testing data 
test.4c=Assign3[-train1,"y"] #Set testing data 
boosttest=gbm(Y~cut.x-1, train, distribution="gaussian", n.trees=7500, shrinkage=0.01) #Run GBM regression
predict.c=predict(boosttest, test4c, n.trees=7500) #Make predicted value       
MSE.4c=mean((predict.c-test.4c)^2) #Calculate MSE
MSE.4c  

train1=sample(1:nrow(Assign3), 50) #draw 50 random number from assign3 using sample function
X.train=X[train1] #Set training data for X
Y.train=Y[train1] #Set training data for Y
train=data.frame(X.train, Y.train) #Group training data with data.frame function
test4c=Assign3[-train1] #Set testing data 
test.4c=Assign3[-train1,"y"] #Set testing data 
boosttest=gbm(Y~cut.x-1, train, distribution="gaussian", n.trees=10000, shrinkage=0.01) #Run GBM regression
predict.c=predict(boosttest, test4c, n.trees=10000) #Make predicted value
MSE.4c=mean((predict.c-test.4c)^2) #Calculate MSE
MSE.4c

tmse=read.csv("tmse.csv")
attach(tmse)
plot(B, Test.MSE, pch=11, cex=2, col='darkorchid2', lwd=3.77)


