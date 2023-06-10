# ------------------------ Code for Question 3 -------------------------------------
# Clear environment
rm(list = ls())

#Easy to reproduce results in model
set.seed(1)
setwd("D:/OMS/Intro to Analytical Modelling/FA_SP_hw3/data 6/")

library(outliers)
library(data.table)

temp_data<- read.table("temps.txt", header = TRUE, sep = "", dec = ".")
head(temp_data)

# average the temperature for each day across the years
date_avgs <- rowMeans(temp_data[c(2:length(temp_data))], dims=1, na.rm=T)

# compute the mean of the (now averaged) time series
da_mu <- mean(date_avgs)

# compute the difference between the mean of the time series and each "day"
da_minus_mu <- date_avgs - da_mu

# set C
C <- 4

# subtract C from the difference score
damimu_minus_C <- da_minus_mu - C

# create an empty vector for looping
# include an additional zero to help with indexing
precusum <- 0 * damimu_minus_C
cusum <- append(precusum, 0)

# loop through each day, check the cumulative sum, update the 
# index of our accumulator with the appropriate value
for (i in 1:length(damimu_minus_C)) 
{
  checker <- cusum[i] + damimu_minus_C[i]
  ifelse(checker > 0, cusum[i+1] <- checker, cusum[i+1] <- 0) 
}
plot(cusum)

# get the index of the max cusum
which(cusum >= 85)

# return the date
temps[56, 1]

