

#**********Exercise 1**********#
#(1)
n=1000 #This generates random size n=1000
b=0.6 #Sets beta =0.6
x=rnorm(n, mean=0,sd=1)  #sets x draws randomly that 
#follows standard normal distribution
e=rnorm(n, mean=0,sd=1) #sets error term draws randomly that 
#follows standard normal distribution
y=exp(b)*x+ e #get the model for y
cor.test(y, x, method=c("pearson", "kendall", "spearman")) #Correlation between y and x
var(x) #Variance of x
var(e) #Variance of error term
var(y) #Variance of y
summary(x) #summary statistics about the distribution of x
summary(e) #summary statistics about the distribution of e
summary(y) #summary statistics about the distribution of y

#(2)
#Below includes codes for sections from (2-A to 2-C)
#Linear regression
linearfit=lm(y~x) #runs linear regression
linearfit
summary(linearfit) #estimation from the linear regression
#plot(linearfit)
#plot(linearfit,which=1, col=c("blue")) #residuals plot for quadratic regression

layout(matrix(1:4, 2, 2)) #Makes four small graphs in one screen
plot(linearfit,col=c("blue"), main="Linear regression") #Residuals plots

library(car) #package for conducting several statistical tests
ncvTest(linearfit) #for conducting BP test
#outlierTest(linearfit) #outlier test
#leveragePlots(linearfit)  #outlier test

#Quadratic regression
quafit=lm(y~x+I(x^2)) #runs quadratic regression
summary(quafit) #estimation from the quadratic regression

layout(matrix(1:4, 2, 2)) #Makes four small graphs in one screen
plot(quafit,col=c("blue"), main="Quadratic regression") #Residuals plots

#plot(quafit)
#plot(quafit,which=1, col=c("blue")) #residuals plot for quadratic regression
ncvTest(quafit) #for BP test
#outlierTest(quafit) #outlier test
#leveragePlots(quafit) #leverage plot

#Cubic regression
cubicfit=lm(y~x+I(x^2)+I(x^3)) #runs cubic regression
summary(cubicfit) #estimation from the cubic regression
#plot(cubicfit)
#plot(cubicfit,which=1, col=c("blue")) #residuals plot for cubic regression

layout(matrix(1:4, 2, 2)) #Makes four small graphs in one screen
plot(cubicfit,col=c("blue"), main="Cubic regression") #Residuals plots
ncvTest(cubicfit) #for conducting BP test
#outlierTest(cubicfit) #outlier test
#leveragePlots(cubicfit) #leverage plot

#(2-D) plot on a single fiture of in-sample predicted values
data=data.frame(x=seq(from=min(x), to=max(x), length.out=n)) #set the x within range with n
#Below set up the prediction for three regressions models
pred.linearfit=predict(linearfit, newdata=data)
pred.quafit=predict(quafit,newdata=data)
pred.cubicfit=predict(cubicfit,newdata=data)
#Plot of in-sample predicted values
plot(x,y, col=c("gray28"))
lines(pred.linearfit~data$x, col="pink", lw=2)
lines(pred.quafit~data$x, col="blueviolet", lw=2)
lines(pred.cubicfit~data$x, col="cornflowerblue", lw=2)

#3
#(3-A)
#Plot Q for b taking values on a grid 
#of point of size 100 in the interval [-1.5,1.5]
p=100 #change to size 100
beta_range= seq(-1.5,1.5, length.out = p) #Limits the interval from -1.5 to 1.5 given size of p

#Below shows the function for beta by
#minimizing the sum of squares of the residuals
Q_function=function(beta_range){ #Sets up function for b and name it as Q_function
  Qb = 0 #Let Qb=0 to start with the loop
  
  for (i in 1:length(x)){ #Sets an index variable for loop within x
    Qb = (y[i]-exp(beta_range)*x[i])*(y[i]-exp(beta_range)*x[i]) + Qb #Sets up equation 
  }
  return(Qb) #Return back the result 
}
Qb = c() #This function returns a vector for Qb
for(i in 1:p){ #Sets an index variable for loop within p
  Qb[i] = Q_function(beta_range[i]) #Defines the Q_function that includes the grid which has interval
}

plot(beta_range, Qb, col=c("blueviolet")) #Plot of Q for b

#(3-B): Deduce estimate for B hat
#This betahat is achieved by 
#minimizing the Q(b) or sum of squares of the residuals
betahat=beta_range[match(min(Qb),Qb)] #This match function helps finding the min
betahat

#alternative ways
newbetahat=nls(y~exp(b)*x,start=list(b=0.6)) #alternative way by using nonlinear least square function
newbetahat

#(3-C) plot predicted values vs. the actual data
yhat=exp(betahat)*x #Sets the new model with betahat
plot(x,y, main="In-sample value vs the actual data")
points(x,yhat,col=c("mediumvioletred"))  #plot of  predicted values vs actual data

#Predict value for yhat and conduct regression with x
pred.yhat=predict(yhat) #Predict value for yhat
actualfit=lm(yhat~x) #Sets regression model of yhat with x

#(3-D) Possible to estimate model with lm function in R?
#It is possible, the steps are shown below:
lm(y~x) #Step I: Conducts linear regressions
log(1.809) #Step II: Takes the log on coefficient of 

#(4-A)
#(4-a)Compute the training mean squared error
training.linearfit_mse=sum(residuals(linearfit)^2)/n # training mean squared error for linear model
training.quafit_mse=sum(residuals(quafit)^2)/n # training mean squared error for quadtratic model
training.cubicfit_mse=sum(residuals(cubicfit)^2)/n # training mean squared error for cubic model
training.actualfit_mse<-sum((yhat-y)^2)/n # training mean squared error for actual model

#Outputs from above
training.linearfit_mse
training.quafit_mse
training.cubicfit_mse
training.actualfit_mse

#(4-B)
m=data.frame(x=rnorm(100)) #Sets new sample size to m=100 with random 
nm=100 #new sample with m=100 renamed to nm
#for x that follows standard normal
k=exp(b)*m +data.frame(e=rnorm(100)) #Sets true model with size m 
#with error terms that follows standard normal

#Sets model with predicted model as test models with new data m
test.linearfit=predict(linearfit,newdata=m)
test.quafit=predict(quafit,newdata=m)
test.cubicfit=predict(cubicfit,newdata=m)
test.actualfit=predict(newbetahat,newdata=m)

kt=as.matrix(k) #Sets up 1 x 100 matrix for true model 

#Below are the equations and outputs for test mse
test.linearfit_mse=sum((kt-test.linearfit)*(kt-test.linearfit))/(nm)
test.quafit_mse=sum((kt-test.quafit)*(kt-test.quafit))/(nm)
test.cubicfit_mse=sum((kt-test.cubicfit)*(kt-test.cubicfit))/(nm)
test.actualfit_mse=sum((kt-test.actualfit)*(kt-test.actualfit))/(nm)

test.linearfit_mse
test.quafit_mse
test.cubicfit_mse
test.actualfit_mse

#(4-C)
#See my comments on word or pdf file


#**********Exercise 2**********#
#(1)
X1 = c(1,1,-3,2,3,4,4) #data input for X1
X2 = c(2,3,1,2,2,1,3) #data input for X2
Y = c(0,0,0,1,1,1,1) #data input for Y
observation = data.frame(X1,X2,Y) #This puts together the inputs and forms 7 x 3 matrix
observation
View(observation)

#(2)
predicty=function(a,b){ #this function allows for output for the prediction of P{Y=1|X_1=a, X_2=b} 
  euclid.distance = c() #This function returns a vector for euclidean distance
  for (i in 1:length(X1)) { #Sets a for loop #Sets an index variable for loop within X1
    euclid.distance[i]=sqrt((a-X1[i])*(a-X1[i])+(b-X2[i])*(b-X2[i])) #Sets up equations for Euclidean distances 
  }
  SHPD = Y[which(!is.na(match(euclid.distance,min(euclid.distance))))] #The !is.na(match() function helps finding 
  #among which of euclidean distances is the shortest or the shortest path distance (SHPD)
  threshold=(1+0)/2 #The threshold probability should be the mean of 0 and 1
  if(length(SHPD)>1){ #Sets an if loop
    #Where length function  sets the length of a vector of SHPD and must set it to >1
    #to complete the entire loops
    PSHPD=(sum(SHPD)*(1/length(SHPD))) 
    #Sets PSHPD to set the condition to predict classification
    #Where sum function returns the sum of SHPD
    
    
    if (PSHPD>=(threshold)) #Output equals 1 if PSHPD>=0.5
      print("1")
    else if(PSHPD<(threshold)) #Output equals 0 if PSHPD<0.5
      print("0")
    
  }
  else 
    paste("Output:",SHPD) #Sets message with output
  
}

#Tests the above function on three random points
predicty(-413,279)
predicty(3,2)
predicty(-3,1)

