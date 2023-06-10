# Installing and calling packages
library(kknn)
# Load package required for plotting
library(ggplot2)
library(gridExtra)

#Easy to reproduce results in model
set.seed(1)
setwd("D:/OMS/Intro to Analytical Modelling/FA_SP_hw2/data 3/")
# Reading the data
cc_data <- read.table("credit_card_data.txt", header = FALSE, sep = "", dec = ".")
head(cc_data)

# V11 is response, other variables are predictors

############# Using train.kknn #############
# set maximum value of k (number of neighbors) to test
# Kernel options include : "rectangular", "triangular", "epanechnikov",	"gaussian", "rank", "optimal"

model_optimal <- train.kknn(V11~.,cc_data,kmax=50,scale=TRUE, kernel = "optimal")
model_optimal

model_rect <- train.kknn(V11~.,cc_data,kmax=50,scale=TRUE, kernel = "rectangular")
model_rect

# create array of prediction qualities
CV_Acc_1 <- data.frame()

# calculate prediction qualities

for (i in 1:50) {
  predicted <- as.integer(fitted(model_optimal)[[i]][1:nrow(cc_data)] + 0.5) # round off to 0 or 1
  t = data.frame(k=i, accuracy= sum(predicted == cc_data[,11])/nrow(cc_data), kernel="optimal")
  CV_Acc_1 = rbind(CV_Acc_1,t)

  predicted <- as.integer(fitted(model_rect)[[i]][1:nrow(cc_data)] + 0.5) # round off to 0 or 1
  t = data.frame(k=i, accuracy= sum(predicted == cc_data[,11])/nrow(cc_data), kernel="rectangular")
  CV_Acc_1 = rbind(CV_Acc_1,t)
}

CV_Acc_1

plt <- qplot(x = CV_Acc_1$k, y = CV_Acc_1$accuracy, color = CV_Acc_1$kernel, ylim = c(0.75,0.9), xlab = 'k', ylab = 'Accuracy') + ggtitle('KNN')
grid.arrange(plt)

CV_Acc_1[which.max(CV_Acc_1$accuracy),]
############# cv.kknn from kknn package #############

set.seed(1)

# create array of prediction qualities
CV_Acc_2 <- data.frame()

#Run loop for each k

for (KCV in c(2,5,10,20)){
  for (i in 1:50){
    
    # run cross-validation for all k in KCV-fold cross-validation number of neighbors
    model <- cv.kknn(V11~.,cc_data, kcv=KCV, k=i, scale=TRUE) 
    
    predicted <- as.integer(model[[1]][,2] + 0.5) # round off to 0 or 1
    t = data.frame(k=i, accuracy= sum(predicted == cc_data[,11])/nrow(cc_data), kcv=KCV)
    CV_Acc_2 = rbind(CV_Acc_2,t)
  }
}

CV_Acc_2

# plot
t1 <- CV_Acc_2[CV_Acc_2$kcv==2,]
p1 <- qplot(x = t1$k, y = t1$accuracy, ylim = c(0.75,0.9),xlab = 'k', ylab = 'Accuracy') + ggtitle('2-Fold CV')

t2 <- CV_Acc_2[CV_Acc_2$kcv==5,]
p2 <- qplot(x = t2$k, y = t2$accuracy, ylim = c(0.75,0.9),xlab = 'k', ylab = 'Accuracy') + ggtitle('5-Fold CV')

t3 <- CV_Acc_2[CV_Acc_2$kcv==10,]
p3 <- qplot(x = t3$k, y = t3$accuracy, ylim = c(0.75,0.9),xlab = 'k', ylab = 'Accuracy') + ggtitle('10-Fold CV')

t4 <- CV_Acc_2[CV_Acc_2$kcv==20,]
p4 <- qplot(x = t4$k, y = t4$accuracy, ylim = c(0.75,0.9),xlab = 'k', ylab = 'Accuracy') + ggtitle('20-Fold CV')

t5 <- CV_Acc_2
p5 <- qplot(x = t5$k, y = t5$accuracy, color = t5$kcv, ylim = c(0.75,0.9),xlab = 'k', ylab = 'Accuracy') + ggtitle('K-Fold CV')

grid.arrange(p1,p2,p3,p4, nrow = 2)
grid.arrange(p5, nrow = 2)

# ------------------------ Code for Question 3b -------------------------------------
# Clear environment
rm(list = ls())

library(kernlab)
library(kknn)

# Reading the data
cc_data <- read.table("credit_card_data.txt", header = FALSE, sep = "", dec = ".")
head(cc_data)


# Setting the random number generator seed so that our results are reproducible
set.seed(1)

############### Split data into training, validation, and test sets ###############

# 60% for training

train_mask = sample(nrow(cc_data), size = floor(nrow(cc_data) * 0.6))
cc_data_train = cc_data[train_mask,] # training data set

temp = cc_data[-train_mask, ]  # all rows except that used in training

# Half for validation, half for test

val_mask = sample(nrow(temp), size = floor(nrow(temp)/2))

cc_data_val = temp[val_mask,]  # validation data set
cc_data_test = temp[-val_mask, ] # test data set

check_accuracy <- data.frame()

# --------------- Train SVM models -------------------

# values of C to test
possible_C <- c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000) 

for (i in seq(1,length(possible_C),1)) {
  
  # fit model on training set
  model <- ksvm(as.matrix(cc_data_train[,1:10]),as.factor(cc_data_train[,11]),type = "C-svc", kernel = "vanilladot", C = possible_C[i],scaled=TRUE)
  
  #  check accuracy of models using validation set
  pred <- predict(model,cc_data_val[,1:10])

  t = data.frame(C=possible_C[i], accuracy= sum(pred == cc_data_val$V11) / nrow(cc_data_val), model = "KSVM",k=0)
  check_accuracy = rbind(check_accuracy,t)
}

check_accuracy[which.max(check_accuracy$accuracy),]

model_best_ksvm <- ksvm(as.matrix(cc_data_train[,1:10]),as.factor(cc_data_train[,11]),type = "C-svc",kernel = "vanilladot", C = possible_C[which.max(check_accuracy$accuracy)],
                     scaled=TRUE)

cat("Performance on test data for KVSM = ",sum(predict(model_best_ksvm,cc_data_test[,1:10]) == cc_data_test$V11) / nrow(cc_data_test),"\n")

# --------------- Train KNN models -------------------
#

for (k in 1:25) {
  
  # fit model on training set and check accuracy on validation set
  model <- kknn(V11~.,cc_data_train,cc_data_val,k=k,scale=TRUE)
  
  t = data.frame(C=0, accuracy= sum((as.integer(fitted(model)+0.5)==cc_data_val$V11)/nrow(cc_data_val)), model = "KNN",k=k)
  check_accuracy = rbind(check_accuracy,t)
}

t <- check_accuracy[(check_accuracy$model == "KNN"),]
t[which.max(t$accuracy),]

model_best_knn <- kknn(V11~.,cc_data_train,cc_data_test,k=t[which.max(t$accuracy),"k"],scale=TRUE)
cat("Performance on test data for KNN= ",accuracy= sum((as.integer(fitted(model_best_knn)+0.5)==cc_data_test$V11)/nrow(cc_data_test)),"\n")

#Check for overall best model basis performance on test data
check_accuracy[which.max(check_accuracy$accuracy),]

