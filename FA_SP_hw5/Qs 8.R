# Clear environment
rm(list = ls())

#Easy to reproduce results in model
set.seed(1)
setwd("D:/OMS/Intro to Analytical Modelling/FA_SP_hw5/data 8.2/")

crime_data<- read.table("uscrime.txt", header = TRUE, sep = "", dec = ".")
head(crime_data)

GLM_Model<-lm(Crime~.,data=crime_data)
summary(GLM_Model)
round(summary(GLM_Model)$coef, 3)
attributes(GLM_Model)

check<-data.frame(M = 14.0,So = 0,Ed = 10.0, Po1 = 12.0,Po2 = 15.5,
                 LF = 0.640, M.F = 94.0,Pop = 150,NW = 1.1,U1 = 0.120,
                 U2 = 3.6, Wealth = 3200,Ineq = 20.1,Prob = 0.04, Time = 39.0)

predict(GLM_Model,check)
range(crime_data$Crime)

plot(GLM_Model)

#filtering variables with significant impact on response
data.frame(summary(GLM_Model)$coef[summary(GLM_Model)$coef[,4] <= .1, 4])

GLM_Model_2<-lm(Crime~M+Ed+Po1+U2+Ineq+Prob,data=crime_data,x=TRUE,y=TRUE)
summary(GLM_Model_2)
round(summary(GLM_Model_2)$coef, 3)
plot(GLM_Model_2)

AIC(GLM_Model_2)

predict(GLM_Model_2,check)
