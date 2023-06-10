library(kernlab)
library(corrplot)

#Easy to reproduce results in model
set.seed(1)

data <- read.table("credit_card_data-headers.txt", header = TRUE, sep = "", dec = ".")
head(data)

corr_data = cor(data)
corrplot(corr_data)

n = nrow(data) # number of data points
numCol = ncol(data)

# ntrain <- round(n*0.8) # get x% of data points 
# tindex <- sample(n,ntrain) 
# xtrain <- as.matrix(data[tindex,-c(numCol)]) # training data, not include the class label
# xtest  <- as.matrix(data[-tindex,-c(numCol)]) # test data, not include the class label
# ytrain <- as.factor(data[tindex,c(numCol)]) # class label for training data
# ytest  <- as.factor(data[-tindex,c(numCol)]) # class label for testing data

model = ksvm(as.matrix(data[,1:10]), as.factor(data[,11]),type= 'C-svc',kernel= 'vanilladot', C=100, scaled=TRUE)
pred <- predict(model,data[,1:10])
print(sum(pred == data[,11]) / nrow(data))

# Get basic details about the model
model

# # Attributes that you can access
# attributes(model)
# 
# alpha(model)
# xmatrix(model)
# coef(model)
# b(model)

# calculate a1.am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])

# calculate a0
a0 <- b(model)

# Explore for best C
Possible_C = c(0.00001,0.0001,0.0005,0.001,0.01,0.1,1,10,100,200,500)
result_C = rep(0,length(Possible_C))

for (i in 1:length(Possible_C)){
  
  model = ksvm(as.matrix(data[,1:10]), as.factor(data[,11]),type= 'C-svc',kernel= 'vanilladot', C=Possible_C[i], scaled=TRUE)

  # see what the model predicts
  pred <- predict(model,data[,1:10])
  
  # see what fraction of the model's predictions match the actual classification
  result_C[i] = (sum(pred == data[,11]) / nrow(data))
}

plot(log(Possible_C),result_C)

#try without A11 and A12 at C = 100
data2 = subset(data, select = -c(A11) )

model = ksvm(as.matrix(data2[,1:9]), as.factor(data2[,10]),type= 'C-svc',kernel= 'vanilladot', C=10, scaled=TRUE)
pred <- predict(model,data2[,1:9])
print(sum(pred == data2[,10]) / nrow(data2))

data2 = subset(data, select = -c(A12) )

model = ksvm(as.matrix(data2[,1:9]), as.factor(data2[,10]),type= 'C-svc',kernel= 'vanilladot', C=10, scaled=TRUE)
pred <- predict(model,data2[,1:9])
print(sum(pred == data2[,10]) / nrow(data2))

data2 = subset(data, select = -c(A11,A12) )

model = ksvm(as.matrix(data2[,1:8]), as.factor(data2[,9]),type= 'C-svc',kernel= 'vanilladot', C=10, scaled=TRUE)
pred <- predict(model,data2[,1:8])
print(sum(pred == data2[,9]) / nrow(data2))


# All values of C less than 500 give same prediction accuracy of around 86.6%. 

###################################### Trying other Kernels #######################################

model = ksvm(as.matrix(data[,1:10]), as.factor(data[,11]),type= 'C-svc',kernel= 'rbfdot', C=100, scaled=TRUE)
model
pred <- predict(model,data[,1:10])
print(sum(pred == data[,11]) / nrow(data))

model = ksvm(as.matrix(data[,1:10]), as.factor(data[,11]),type= 'C-svc',kernel= 'anovadot', C=100, scaled=TRUE)
model
pred <- predict(model,data[,1:10])
print(sum(pred == data[,11]) / nrow(data))

model = ksvm(as.matrix(data[,1:10]), as.factor(data[,11]),type= 'C-svc',kernel= 'polydot', C=100, scaled=TRUE)
model
pred <- predict(model,data[,1:10])
print(sum(pred == data[,11]) / nrow(data))

####################################### KNN Bit ###################################################

library(kknn)

data <- read.table("credit_card_data-headers.txt", header = TRUE, sep = "", dec = ".")
head(data)

Calc_KNN_Accuracy = function(KNNdata,K)
{
  pred <- rep(0,(nrow(KNNdata))) 
  
  for (i in 1:nrow(KNNdata)){
    
    #R1~ given to exclude R1 in the dataset
    model=kknn(R1~.,KNNdata[-i,],KNNdata[i,],k=K, scale = TRUE)
    #fitted(model)
    pred[i] <- as.integer(fitted(model) + 0.5) # round off to 0 or 1
  }
  
  # calculate fraction of correct predictions
  
  acc = sum(pred == KNNdata[,11]) / nrow(KNNdata)
  return(acc)
}

#Check Run 
Calc_KNN_Accuracy (data,12)

# Iterating by changing number of neighbours from 1 to 30 
#
Neighbours=rep(0,30) # set up a vector with zeros

for (K in 1:length(Neighbours)){
  Neighbours[K] = Calc_KNN_Accuracy(data, K) # test knn with k neighbors
}

plot(Neighbours)
title("KNN Accuracy varying with K")
cat("Highest Accuracy is with K = ", which.max(Neighbours))

Calc_KNN_Accuracy (data,652)
