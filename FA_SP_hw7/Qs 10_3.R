# Clear environment
rm(list = ls())

#Easy to reproduce results in model
set.seed(1)
setwd("D:/OMS/Intro to Analytical Modelling/FA_SP_hw7/data 10.3/")

data<- read.table("germancredit.txt", header = FALSE, sep = "", dec = ".")
str(data)
  
table(data$V21)
data$V21 = data$V21-1

# We should measure model quality in terms of accuracy (truePositive + trueNegative /(total predictions + actual values))

# Split data into train and validation set
datacreditPart <- createDataPartition(data$V21, times = 1, p = 0.6, list=FALSE)
head(datacreditPart)

dataTrain <- data[datacreditPart,] 
dataValid <- data[-datacreditPart,]

table(dataTrain$V21)
table(dataValid$V21)

creditLogModel <- glm(V21 ~ ., data = dataTrain, family=binomial(link="logit"))
# Look at importance of predictors
summary(creditLogModel)

#Let's do a baseline prediction.
creditPredict <- predict(creditLogModel, newdata=dataValid[,-21], type="response")
table(dataValid$V21, round(creditPredict))

V1A14
V8
V4A43
V1A12
V14A143
V6A65
V4A410
V20A202
V2
V1A13
V3A34

# Since there are multiple categorical variables within a single column, we need to manually remove the non-significant variables.
dataTrain$V1A14[dataTrain$V1 == "A14"] <- 1
dataTrain$V1A14[dataTrain$V1 != "A14"] <- 0

dataTrain$V1A13[dataTrain$V1 == "A13"] <- 1
dataTrain$V1A13[dataTrain$V1 != "A13"] <- 0

dataTrain$V4A43[dataTrain$V4 == "A43"] <- 1
dataTrain$V4A43[dataTrain$V4 != "A43"] <- 0

dataTrain$V1A12[dataTrain$V1 == "A12"] <- 1
dataTrain$V1A12[dataTrain$V1 != "A12"] <- 0

dataTrain$V14A143[dataTrain$V14 == "A143"] <- 1
dataTrain$V14A143[dataTrain$V14 != "A143"] <- 0

dataTrain$V6A65[dataTrain$V6 == "A65"] <- 1
dataTrain$V6A65[dataTrain$V6 != "A65"] <- 0

dataTrain$V4A410[dataTrain$V4 == "A410"] <- 1
dataTrain$V4A410[dataTrain$V4 != "A410"] <- 0

dataTrain$V20A202[dataTrain$V20 == "A202"] <- 1
dataTrain$V20A202[dataTrain$V20 != "A202"] <- 0

dataTrain$V3A34[dataTrain$V3 == "A34"] <- 1
dataTrain$V3A34[dataTrain$V3 != "A34"] <- 0

creditLogModel2 <- glm(V21 ~ V1A14+V8+V4A43+V1A12+V14A143+V6A65+V4A410+V20A202+V2+V1A13+V3A34, data = dataTrain, family=binomial(link="logit"))
summary(creditLogModel2)

dataValid$V1A14[dataValid$V1 == "A14"] <- 1
dataValid$V1A14[dataValid$V1 != "A14"] <- 0

dataValid$V1A13[dataValid$V1 == "A13"] <- 1
dataValid$V1A13[dataValid$V1 != "A13"] <- 0

dataValid$V4A43[dataValid$V4 == "A43"] <- 1
dataValid$V4A43[dataValid$V4 != "A43"] <- 0

dataValid$V1A12[dataValid$V1 == "A12"] <- 1
dataValid$V1A12[dataValid$V1 != "A12"] <- 0

dataValid$V14A143[dataValid$V14 == "A143"] <- 1
dataValid$V14A143[dataValid$V14 != "A143"] <- 0

dataValid$V6A65[dataValid$V6 == "A65"] <- 1
dataValid$V6A65[dataValid$V6 != "A65"] <- 0

dataValid$V4A410[dataValid$V4 == "A410"] <- 1
dataValid$V4A410[dataValid$V4 != "A410"] <- 0

dataValid$V20A202[dataValid$V20 == "A202"] <- 1
dataValid$V20A202[dataValid$V20 != "A202"] <- 0

dataValid$V3A34[dataValid$V3 == "A34"] <- 1
dataValid$V3A34[dataValid$V3 != "A34"] <- 0

# create confusion matrix of predicted vs. observed values on validation set
creditPredict2 <- predict(creditLogModel2, newdata=dataValid[,-21], type="response")

t <- as.matrix(table(round(creditPredict2), dataValid$V21))
names(dimnames(t)) <- c("Predicted", "Observed")
t

# Calculate accuracy and specificity, aiming to maximize specificity given the costs of a false positive
for (i in seq(50, 95, by=5)) {
  threshold <- i/100
  print(threshold)
  t2 <- as.matrix(table(round(creditPredict2 > threshold), dataValid$V21))
  print(t2[2,1]*5 + t2[1,2])
}

accuracy <- (t2[1,1]+t2[2,2])/(t2[1,1]+t2[1,2]+t2[2,1]+t2[2,2])
accuracy

specificity <- (t2[1,1])/(t2[1,1]+t2[2,1])
specificity
