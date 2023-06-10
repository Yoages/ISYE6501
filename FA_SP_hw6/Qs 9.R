# Clear environment
rm(list = ls())

#Easy to reproduce results in model
set.seed(1)
setwd("D:/OMS/Intro to Analytical Modelling/FA_SP_hw6/data 9.1/")

data<- read.table("uscrime.txt", header = TRUE, sep = "", dec = ".")
head(data)

check<-data.frame(M = 14.0,So = 0,Ed = 10.0, Po1 = 12.0,Po2 = 15.5, LF = 0.640, M.F = 94.0,Pop = 150,NW = 1.1,U1 = 0.120,
                  U2 = 3.6, Wealth = 3200,Ineq = 20.1,Prob = 0.04, Time = 39.0)

# apply the PCA model 
pca <- prcomp(crime_data[,1:15], scale = FALSE)
summary(pca)
screeplot(pca, type="lines",col="blue")

results <- data.frame()

for (i in seq(2, 15, by=1)) {

  PCs <- pca$x[,1:i]
  PCcrime <- as.data.frame(cbind(PCs, crime_data[,16])) #Create new dataframe with first i PCs and crime
  colnames(PCcrime)[i+1] <- "Crime"
  
  model <- lm(Crime~., data = PCcrime) #Create regression model on new data
  #summary(model)
  
  beta0 <- model$coefficients[1]
  beta <- model$coefficients[1:i+1]
  
  alpha <- pca$rotation[,1:i] %*% beta
  
  orgAlpha <- alpha/sapply(crime_data[,1:15],sd)
  orgBeta0 <- beta0 - sum(alpha*sapply(crime_data[,1:15],mean)/sapply(crime_data[,1:15],sd))
  
  estimates <- as.matrix(crime_data[,1:15]) %*% orgAlpha + orgBeta0
  
  SSE = sum((estimates - crime_data[,16])^2)
  SStot = sum((crime_data[,16] - mean(crime_data[,16]))^2)
  
  R2 <- 1 - SSE/SStot
  Pred <- as.matrix(check[,1:15]) %*% orgAlpha + orgBeta0

  #Residual Standard error (Like Standard Deviation)
  n=length(model$residuals)
  k=length(model$coefficients)-1 #Subtract one to ignore intercept
  SSyy=sum((crime_data[,16]-mean(crime_data[,16]))**2)
  SSE=sum(model$residuals**2)
  
  Resi = sqrt(SSE/(n-(1+k))) #Residual Standard Error
  R2 = 1-SSE/SSyy
  R2_Adj = 1-(SSE/SSyy)*(n-1)/(n-(k+1))
  F_Stat = ((SSyy-SSE)/k) / (SSE/(n-(k+1)))
  
  AIC = AIC(model)
  
  t = data.frame(PCs = i, Resi = Resi, R2 = R2, R2_Adj = R2_Adj, F_Stat = F_Stat, AIC = AIC,Pred = Pred)
  results = rbind(results,t)
}

temp = data.frame(results['PCs'],results['Resi'])
plot(temp, xlab="No. of Principal Components", ylab="residuals")

temp = data.frame(results['PCs'],results['R2'])
plot(temp, xlab="No. of Principal Components", ylab="R2")

temp = data.frame(results['PCs'],results['R2_Adj'])
plot(temp, xlab="No. of Principal Components", ylab="R2_Adj")

temp = data.frame(results['PCs'],results['F_Stat'])
plot(temp, xlab="No. of Principal Components", ylab="F_Stat")

temp = data.frame(results['PCs'],results['AIC'])
plot(temp, xlab="No. of Principal Components", ylab="AIC")

temp = data.frame(results['PCs'],results['Pred'])
plot(temp, xlab="No. of Principal Components", ylab="Prediction")

install.packages("clipr")
library("clipr")
write_clip(results)

crime_data2 = crime_data[,c("M","Ed","Po1","U2","Ineq","Prob","Crime")]
pca2 <- prcomp(crime_data2[,1:6], scale = TRUE)
summary(pca2)

results2 <- data.frame()
check2  = check[,c("M","Ed","Po1","U2","Ineq","Prob")]

for (i in seq(2, 6, by=1)) {
  
  PCs <- pca2$x[,1:i]
  PCcrime <- as.data.frame(cbind(PCs, crime_data2[,7])) #Create new dataframe with first i PCs and crime
  colnames(PCcrime)[i+1] <- "Crime"
  
  model <- lm(Crime~., data = PCcrime) #Create regression model on new data
  #summary(model)
  
  beta0 <- model$coefficients[1]
  beta <- model$coefficients[1:i+1]
  
  alpha <- pca2$rotation[,1:i] %*% beta
  
  orgAlpha <- alpha/sapply(crime_data2[,1:6],sd)
  orgBeta0 <- beta0 - sum(alpha*sapply(crime_data2[,1:6],mean)/sapply(crime_data2[,1:6],sd))
  
  estimates <- as.matrix(crime_data2[,1:6]) %*% orgAlpha + orgBeta0
  
  SSE = sum((estimates - crime_data2[,7])^2)
  SStot = sum((crime_data2[,7] - mean(crime_data2[,7]))^2)
  
  R2 <- 1 - SSE/SStot
  Pred <- as.matrix(check2) %*% orgAlpha + orgBeta0
  
  #Residual Standard error (Like Standard Deviation)
  n=length(model$residuals)
  k=length(model$coefficients)-1 #Subtract one to ignore intercept
  SSyy=sum((crime_data2[,7]-mean(crime_data2[,7]))**2)
  SSE=sum(model$residuals**2)
  
  Resi = sqrt(SSE/(n-(1+k))) #Residual Standard Error
  R2 = 1-SSE/SSyy
  R2_Adj = 1-(SSE/SSyy)*(n-1)/(n-(k+1))
  F_Stat = ((SSyy-SSE)/k) / (SSE/(n-(k+1)))
  
  AIC = AIC(model)
  
  t = data.frame(PCs = i, Resi = Resi, R2 = R2, R2_Adj = R2_Adj, F_Stat = F_Stat, AIC = AIC,Pred = Pred)
  results2 = rbind(results2,t)
}

write_clip(results2)
