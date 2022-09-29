
#ECON7333 Assignment 2
setwd("D:/UQ/2019 S2/ECON7333 Big Data and ML/Assignment 2/Assignment official file FINAL") #Set path

#(1) plot the classification 
Assign2=read.csv("Assign2.csv") #Read Assign2.csv file
attach(Assign2) #attach Assign2
plot(Assign2$x1,Assign2$x2,xlab='x1', ylab='x2') #Plotting the data of Assign2
y1 <- Assign2[Assign2$y==1,] #Set y1 if y is 1
y0 <- Assign2[Assign2$y==0,] #Set y0 if y is 0
points(y1$x1,y1$x2,col="blueviolet") #Set color of y1 based on x1 and x2
points(y0$x1,y0$x2,col="hotpink") #Set color of y0 based on x1 and x2

#library(ggplot2)
#View(Assign2)
#qplot(x1, x2, data=Assign2, col=y+8)
#plot(x1, x2, col=y+15)

#(2) 
#Fit linear model
linfit=glm(y~x1+x2) #Run a linear regression model
linfit
linfit.probs=predict(linfit, type ="response") #Output probabilities of the form P(y=1|regressor)
linfit.pred=rep(0,1000)##Creates a vector of 1000 "0" elements
linfit.pred[linfit.probs>0.5]=1 #Transform to "1" elements if the predicted probability>0.5
table(linfit.pred,y) #Produce a confusion matrix
(11+506)/1000 #accuracy rate
mean(linfit.pred==y) #alternative way of getting accuracy rate with mean function

#(3) Repeat the same for logistic regression, LDA, and QDA
#(3-A)logistic
logfit=glm(y~x1+x2, family=binomial) #Run a logistic regression
logfit
#Output prob. of the form P(y=1|regressor)
logfit.probs=predict(logfit,type="response") #Output probabilities of the form P(y=1|regressor)
logfit.pred=rep(0,1000) #Creates a vector of 1000 "0" elements
logfit.pred[logfit.probs>0.5]=1 #Transform to "1" elelements 
#if the predicted probability>0.5
table(logfit.pred,y) #Produce the confusion matrix for logistic "regression"
mean(logfit.pred==y) #Output the accuracy rate

#(3-B)LDA
library(MASS) #Load the MASS package
lda.fit=lda(y~x1+x2) #Run linear discriminate analysis
lda.fit
lda.pred=predict(lda.fit) #Predict the LDA
names(lda.pred) #The predict function returns: class, posterior, and x
#where class contains LDA's prediction about the movement of y
#Posterior is a matrix whose kth column contains the posterior prob that the 
#observation belongs to kth class
#x contains the linear discriminants
lda.class=lda.pred$class #Movement of y is stored in class
table(lda.class,y) #Produce the confusion matrix for LDA
mean(lda.class==y) #Output the accuracy rate

#(3-C)QDA
qda.fit=qda(y~x1+x2) #Run quadratic discriminate analysis
qda.fit
qda.pred=predict(qda.fit) #Predict the QDA
names(qda.pred) ##The predict function returns: class, posterior
qda.class=qda.pred$class  #Movement of y is stored in class
table(qda.class,y) #Produce the confusion matrix for LDA
mean(qda.class==y)#Output the accuracy rate

#(4)KNN
library(class) #load the class package
newAssign2=cbind(x1,x2) #Use cbind function to combine x1 and x2 into a single dataframe
#use for LOOPS
for (i in 1:20){ #Create an index variable for running loop from k=1 to 20
  knn.pred.pb = knn(newAssign2, newAssign2,  Assign2$y, k = i) #Predict the posterior probability
  #for knn
  print (paste('k=',i)) #output which k
  print (table(knn.pred.pb,  Assign2$y)) #Set Confusion matrix with table function
  print (mean(knn.pred.pb == Assign2$y)) #Accuracy rate with mean function
}

#(5)24 methods using 10 fold cross validation
library(boot) #load the boot package
library(class) #load the class package
#Create a fold 
ns1=1000 #Set up sample size n=1000
kf1=10 #Set up 10 folds
top1 = ceiling(ns1/kf1) #returns the smallest integers larger than the parameter.
fold= sample(rep(1:1000, top1), 1000) #Randomized the data with 1000 obs

#CV error for linear model
linear.cv.error = rep(0,10) #Replicate a 1x10 matrix for linear model cv error
for (lin in 1:10){ #Set up for loop for the 10-folds
  index = fold[(lin-1)*100+1:100]                # Set 100 observations to extract the testing fold
  fit.lin = lm(y ~ x1 + x2, Assign2[-index,])    #Fit the validation set
  pr.lin= predict(fit.lin, Assign2[index,], type = 'response') #Predict the model with k-1 fold
  pred.lin=rep(0,100)         #Replicate a 1x10 matrix for classification of pred.lin
  pred.lin[pr.lin > 0.5] = '1'  #Transform to "1" elements if the predicted probability>0.5  
  linear.cv.error[lin] = mean(Assign2$y[index] != pred.lin) #Linear CV error
}
linear.cv.error
mean(linear.cv.error) #Average of linear CV error                                         

#CV error for logistic model
logistic.cv.error = rep(0,10) #Replicate a 1x10 matrix for logistic model cv error
for (f.logit in 1:10){ #Set up for loop for the 10-folds
  index_logit <- fold[(f.logit-1)*100+1:100]  # Set 100 observations to extract 
  #the testing fold           
  fit.logit = glm (y ~ x1 + x2, Assign2[-index_logit,], family = binomial) #Fit the validation set
  pr.logit = predict(fit.logit, Assign2[index_logit,], type = 'response')  #Predict the model with k-1 fold
  #Output probabilities 
  pred.logit = rep(0,100)   #Replicate a 1x10 matrix for classification of pred.logit
  pred.logit[pr.logit > 0.5] = '1' #Transform to "1" elements if the predicted probability>0.5 
  logistic.cv.error [f.logit] = mean(Assign2$y[index_logit] != pred.logit) #Logisitic cv error
  #the error rate of prediction for logistic model
}
logistic.cv.error 
mean(logistic.cv.error ) #Average of logistic cv error          

#CV errror for LDA
lda = rep(0,10) #Replicate a 1x10 matrix for LDA cv error
for (f.lda in 1:10){ #Set up for loop for the 10-folds
  index.lda = fold[(f.lda-1)*100+1:100]  # choose 100 observations from the kth fold for LDA
  fit.lda = lda (y ~ x1 + x2, Assign2[-index.lda,]) #Fit the validation set for LDA
  pred.lda = predict(fit.lda, Assign2[index.lda,]) #Predict the model with k-1 fold for LDA
  lda[f.lda] = mean(Assign2$y[index.lda] != pred.lda$class) #LDA cv error
}
lda
mean(lda) #Average of LDA cv errors

#CV error for QDA
qda = rep(0,10)  #Replicate a 1x10 matrix for QDA cv error
for (f.qda in 1:10){#Set up for loop for the 10-folds
  index.qda = fold[(f.qda-1)*100+1:100] # choose 100 observations from the kth fold for QDA
  fit.qda = qda(y ~ x1 + x2, Assign2[-index.qda,])  #Fit the validation set for QDA
  pred.qda = predict(fit.qda, Assign2[index.qda,])#Predict the model with k-1 fold for QDA
  qda[f.qda] = mean(Assign2$y[index.qda] != pred.qda$class)#QDA CV error
}
qda
mean(qda) #Average of QDA CV errors

#CV error for KNN 

for (ol in 1:20) {     #Create the outer for loop for knn with k=1:20
  err.knn = function(ol) {  #Create a function
    error.rate = rep(0,10)# store 10 error.rate from 10 folds
    for (il in 1:10) {# Create an inner loop for 10 cv folds
      index.knn = fold[(il-1)*100+1:100] #Get 100 observations as 
      #testing fold from obs 1 to 100
      train_set = cbind(x1,x2)[-index.knn,] #Create train set
      test_set = cbind(x1,x2)[index.knn,] #Create test set
      pred.knn = knn(train_set, test_set, y[-index.knn], k = ol) #Predict the knn in which the k inside knn function 
      #is the number of nearest neighbor used by the classifier
      error.rate[il] = mean(pred.knn != y[index.knn])   #Get the KNN cv error
    }
    return(mean(error.rate)) #output average of knn cv error                      
  }
  error.rate = err.knn(ol)  # cv error for 20 knn prediction
  print(paste('CV error = ', error.rate, ',if k=',ol)) #Output the cv error rate
}

#CV error for KNN 
ns2=1000 #Set up sample size n=1000
kf2=10 #Set up 10 folds
top2 = ceiling(ns2/kf2) #returns the smallest integers larger than the parameter.
fold2= sample(rep(1:1000, top2), 1000) #Randomized the data with 1000 obs

for (ol in 1:20) {     #Create the outer for loop for knn with k=1:20
  err.knn = function(ol) {  #Create a function
    error.rate = rep(0,10)# store 10 error.rate from 10 folds
    for (il in 1:10) {# Create an inner loop for 10 cv folds
      index.knn = fold2[(il-1)*100+1:100] #Get 100 observations as 
      #testing fold from obs 1 to 100
      train_set = cbind(x1,x2)[-index.knn,] #Create train set
      test_set = cbind(x1,x2)[index.knn,] #Create test set
      pred.knn = knn(train_set, test_set, y[-index.knn], k = ol) #Predict the knn
      error.rate[il] = mean(pred.knn != y[index.knn])   #Get the KNN cv error
    }
    return(mean(error.rate)) #output average of knn cv error                      
  }
  error.rate = err.knn(ol)  # cv error for 20 knn prediction
  print(paste('CV error = ', error.rate, ',if k=',ol)) #Output the cv error rate
}

#Alternative CV error for KNN with permute package
library(permute)
foldz = shuffle(1000)
foldz
#CV error for KNN
for (ol in 1:20) {     #Create the outer for loop for knn with k=1:20
  err.knn = function(ol) {  #Create a function
    foldz = shuffle(1000)  # Randomized the data with 1000 obs
    error.rate = rep(0,10)# store 10 error.rate from 10 folds
    for (il in 1:10) {# Create an inner loop for 10 cv folds
      index.knn = foldz[(il-1)*100+1:100] #draw 100 observations as 
      #testing fold from obs 1 to 100
      train_set = cbind(x1,x2)[-index.knn,] #Create train
      test_set = cbind(x1,x2)[index.knn,] #Create test
      pred.knn = knn(train_set, test_set, y[-index.knn], k = il) #Predict the knn
      error.rate[il] = mean(pred.knn != y[index.knn])   #Get the KNN cv error
    }
    return(mean(error.rate)) #output average of knn cv error                      
  }
  error.rate = err.knn(ol)  # cv error for 20 knn prediction
  print(paste('k@',ol,',CV error = ', error.rate)) #Output the cv error rate
}


