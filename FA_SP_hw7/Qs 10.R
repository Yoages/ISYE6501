
# Clear environment
rm(list = ls())

# Load necessary packages
library(randomForest)
library(tree)
library(caret)

#Easy to reproduce results in model
set.seed(1)
setwd("D:/OMS/Intro to Analytical Modelling/FA_SP_hw7/data 10.1/")

data<- read.table("uscrime.txt", header = TRUE, sep = "", dec = ".")
head(data)

calcR2 <- function(predict,actual){
  RSS <- sum((predict - actual)^2)
  TSS <- sum((actual - mean(actual))^2)
  R2 <- 1 - RSS/TSS
  return(R2)
} 

crimeTreeMod <- tree(Crime ~ ., data = data)
summary(crimeTreeMod)
crimeTreeMod$frame

plot(crimeTreeMod)
text(crimeTreeMod)
title("USCRIME Training Set's Classification Tree")

R2RegTree = calcR2(predict(crimeTreeMod, data=data[,-16]),data[,16])

# Prune the tree
termnodes <-4 
prune.crimeTreeMod <- prune.tree(crimeTreeMod, best = termnodes)
plot(prune.crimeTreeMod)
text(prune.crimeTreeMod)
title("Pruned Tree")
summary(prune.crimeTreeMod)

prune.crimeTreeMod$frame

plot(prune.tree(crimeTreeMod)$size, prune.tree(crimeTreeMod)$dev, type = "b")
title("Pruned Tree Deviation")

R2RegTreePruned = calcR2(predict(prune.crimeTreeMod, data=data[,-16]),data[,16])

crime.rf <-randomForest(Crime ~ ., data=data, importance = TRUE)
summary(crime.rf)
R2RF = calcR2(predict(crime.rf, data=data[,-16]),data[,16])

crime.rf2 <- randomForest(Crime ~ ., data=data, importance = FALSE)
summary(crime.rf2)
R2RF2 = calcR2(predict(crime.rf2, data=data[,-16]),data[,16])

# Let's make a loop to plug in different values of mtry and nodesize to try and find the model with the best R2
result.rf <- data.frame(matrix(nrow=5, ncol=3))
colnames(result.rf) <- c("NodeSize", "mtry", "R2")

i = 1
suppressWarnings(for (nodesize in 2:10) {
  for (m in 2:15) {
    model <- randomForest(Crime ~ ., data=data, importance = TRUE, nodesize = nodesize, mtry = m)
    result.rf[i,1] <- nodesize
    result.rf[i,2] <- m
    result.rf[i,3] <- calcR2(predict(model, data=data[,-16]),data[,16])
    i = i + 1
  }
})

result.rf[which.max(result.rf[,3]),]

crime.rf.final <- randomForest(Crime ~ ., data=data, importance = TRUE, nodesize = 4, mtry = 3)
importance(crime.rf.final)
