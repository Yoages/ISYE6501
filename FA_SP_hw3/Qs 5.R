# Clear environment
rm(list = ls())

#Easy to reproduce results in model
set.seed(1)
setwd("D:/OMS/Intro to Analytical Modelling/FA_SP_hw3/data 5/")

library(outliers)
library(ggplot2)

crime_data<- read.table("uscrime.txt", header = TRUE, sep = "", dec = ".")
head(crime_data)

#Using Normal Assumption since there are more than 30 datapoints
hist(crime_data[,16])

# visualize data
plot(crime_data$Crime)

mean_crime <- mean(crime_data[,16]) #Average
sd_crime <- sd(crime_data[,16]) #Average
Mean_minus_2sd = mean_crime - 1.96*sd_crime
Mean_plus_2sd = mean_crime + 1.96*sd_crime
subset(crime_data, Crime > Mean_plus_2sd)

#Using Grubbs Test
results <-grubbs.test(crime_data[,16], type = 11)
results

#Check Quantiles and make a box plot
Min <- quantile(crime_data[,16],0) #min
Q1 <- quantile(crime_data[,16],0.25) #Q1
Q2 <- quantile(crime_data[,16],0.5) #Median
Q3 <- quantile(crime_data[,16],0.75) #Q3
Max <- quantile(crime_data[,16],1) #max

boxplot(crime_data[,16],ylab="Crime Value")
title(main="Crime Outliers")

