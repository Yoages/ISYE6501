# Clear environment
rm(list = ls())

#Easy to reproduce results in model
set.seed(1)
setwd("D:/OMS/Intro to Analytical Modelling/FA_SP_hw8/data 11.1/")

data<- read.table("uscrime.txt", header = TRUE, sep = "", dec = ".")
head(data)

library(MASS)
# Fit the full model 
full.model <- lm(Crime ~., data = data)
null.model <- lm(Crime ~ 1, data = data)

# Stepwise regression models
step.modelBoth <- step(full.model, direction = "both",trace = F)
step.modelbackward <- step(full.model, direction = "backward",trace = F)
step.modelforward <- step(null.model, direction = "forward",trace = F,scope = list(lower = null.model, upper = full.model), steps = 10)

library(DAAG)
# do 5-fold cross-validation on both and full model
c.full.model = cv.lm(data,full.model,m=5) 
c.step.both = cv.lm(data,step.modelBoth,m=5) 
c.step.fwd = cv.lm(data,step.modelforward,m=5) 

# total sum of squared differences between data and its mean
SStot = sum((data$Crime - mean(data$Crime))^2)

SSres_model.full.model = sum(full.model$residuals^2)
SSres_model.step.modelBoth = sum(step.modelBoth$residuals^2)
SSres_model.step.modelbackward = sum(step.modelbackward$residuals^2)
SSres_model.step.modelforward = sum(step.modelforward$residuals^2)

SSres_c.c.full.model = attr(c.full.model,"ms")*nrow(data)
SSres_c.c.step.both = attr(c.step.both,"ms")*nrow(data)
SSres_c.c.step.fwd = attr(c.step.fwd,"ms")*nrow(data)

# Calculate R-squareds for models and cross-validation
r2 = c()
r2[1] = 1 - SSres_model.full.model/SStot
r2[2] = 1 - SSres_model.step.modelBoth/SStot
r2[3] = 1 - SSres_model.step.modelbackward/SStot
r2[4] = 1 - SSres_model.step.modelforward/SStot
r2[5] = 1 - SSres_c.c.full.model/SStot 
r2[6] = 1 - SSres_c.c.step.both/SStot
r2[7] = 1 - SSres_c.c.step.fwd/SStot
r2

AIC = c()
AIC[1] = AIC(full.model)
AIC[2] = AIC(step.modelBoth)
AIC[3] = AIC(step.modelbackward)
AIC[4] = AIC(step.modelforward)
AIC


# Full Model summary
print(summary(full.model))

# Stepwise regression model summary
print(summary(step.modelBoth))
print(summary(step.modelbackward))
print(summary(step.modelforward))

# Lasso and Elastic Net
rm(list = ls())
set.seed(123)
setwd("D:/OMS/Intro to Analytical Modelling/FA_SP_hw8/data 11.1/")

data <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)
head(data)

#scale the data; does not scaled col #2 as it is binary and col #16 as it is the response
#fix the column names
s_data = cbind(as.data.frame(scale(data[,1])), as.data.frame(data[,2]), as.data.frame(scale(data[,c(3,4,5,6,7,8,9,10,11,12,13,14,15)])), as.data.frame(data[,16]))
colnames(s_data) = colnames(data)
#display scaled data
head(s_data)

x <- as.matrix(s_data[,1:15])
y <- as.matrix(s_data[,16])

resultsmin = matrix(, 41, 2)
colnames(resultsmin) = c('alpha', 'lambda')

i = 1
for (a in seq(0,1.00,.025)) {
  enet = cv.glmnet(x,y,family = "gaussian",alpha = a,nfolds = 5,type.measure = "mse")

  devmin = enet$glmnet.fit$dev.ratio[which(enet$glmnet.fit$lambda == enet$lambda.min)]

  resultsmin[i,1] = a
  resultsmin[i,2] = devmin

  i= i+1
}

best_alpha = resultsmin[which.max(resultsmin[,2]),]
best_alpha

#Using the lambda.min model
enetmin = cv.glmnet(x,  y,  family = "gaussian",  alpha = 1, nfolds = 5,  type.measure = "mse")
coef(enetmin, s = enetmin$lambda.min)
model_lasso = lm(Crime~M+So+Ed+Po1+M.F+NW+U2+Ineq+Prob, data = s_data)
summary(model_lasso)
AIC(model_lasso)

enetmin = cv.glmnet(x,  y,  family = "gaussian",  alpha = 0, nfolds = 5,  type.measure = "mse")
coef(enetmin, s = enetmin$lambda.min)
model_ridge = lm(Crime~., data = s_data)
summary(model_ridge)
AIC(model_ridge)

p = cv.glmnet(x,  y,  family = "gaussian",  alpha = 0.95, nfolds = 5,  type.measure = "mse")
coef(enetmin, s = enetmin$lambda.min)
model_elasnet = lm(Crime~M+So+Ed+Po1+Po2+M.F+NW+U1+U2+Wealth+ Ineq+Prob, data = s_data)
summary(model_elasnet)
AIC(model_elasnet)

