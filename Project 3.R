rm(list = ls())
set.seed(1245)

library(bestglm)


getwd()
setwd('D:/TU Dortmund/Semesters/Summer Semester 2022/ICS/Project 3')
data <- read.csv('vw_data.csv')

head(data)

#missing value check
any(is.null(data))

#Data description check
str(data)

#Summary of the total dataset
summary(data)

#Total number of observations
nrow(data)

#Version of R------
R.version.string

################################################################################
#let's look at the independent variables

# Models of the cars and total number of cars in each category
table(data$model)

df_model <- data.frame()
for(i in unique(data$model)){
  df_model <- rbind(df_model, summary(data$price[data$model == i]))
}
colnames(df_model) <- c('Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max')

rownames(df_model) <- c('Passat', 'T-Roc', 'Up')


#Transmission of the cars and total number of cars in each category
table(data$transmission)


df_trans <- data.frame()
for(i in unique(data$transmission)){
  df_trans <- rbind(df_trans, summary(data$price[data$transmission == i]))
}

colnames(df_trans) <- c('Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max')
rownames(df_trans) <- c('Automatic', 'Manual', 'Semi-Auto')

#FuelType of the cars and total number of cars in each category
table(data$fuelType)

df_fuel <- data.frame()
for(i in unique(data$fuel)){
  df_fuel<- rbind(df_fuel, summary(data$price[data$fuel == i]))
}
colnames(df_fuel) <- c('Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max')
rownames(df_fuel) <- c('Diesel', 'Hybrid', 'Other', 'Petrol')

#Tax of the cars and total number of cars in each category
table(data$tax)


#Enginesize of the cars and total number of cars in each category
table(data$engineSize)

table(data$year)

################################################################################
#Task 1 
#In this project, the unit for measuring fuel consumption shall be l/(100km)
#(litres per 100 km). Convert the variable mpg to this new unit.

data$lp100km <- 282.48 / data$mpg

################################################################################
#Task 2
#Use the variable year to calculate the cars’ age, and use the obtained values to
#define the new variable age. Replace the variable year with age in the data set.

data$age <- 2022 - data$year

################################################################################

#Summary of the numeric variables
data_num <- data[c('price', 'mileage', 'tax', 'engineSize','lp100km', 'age')]
summary(data_num)

################################################################################
#Looking at the scatter plots

plot(data$lp100km, data$price, xlab = 'lp100km', ylab = 'Price')


plot(data$age, data$price, xlab = 'Age', ylab = 'Price')

plot(data$tax, data$price, xlab = 'tax', ylab = 'Price')

#plot(data$age, data$price, xlab = 'engineSize', ylab = 'Price')

plot(data$age, data$mileage, xlab = 'Age', ylab = 'Mileage')


#plot(data$model, data$price, xlab = 'Model', ylab = 'Price')
cor(data_num)

#Varince inflation factor
vifx(data_num)

################################################################################
#Converting the categorical variables as factors

data$model <- as.factor(data$model)
data$transmission <- as.factor(data$transmission)
data$fuelType <- as.factor(data$fuelType)

#Task 3
#Build a linear regression model to predict price from the other covariates. 
################################################################################
#Decide whether it is better to use price or log(price) as the response variable.

model1 <- lm(price ~ model + transmission + mileage + fuelType + tax +
               engineSize + lp100km + age, data = data)

residuals_mod1 <- rstandard(model1)
plot(model1$fitted.values, residuals_mod1, ylab = 'Standardardized residuals',
     xlab = 'Fitted values of price')
abline(0,0, col = 'blue')

model2 <- lm(log(price) ~ model + transmission + mileage + fuelType + tax +
               engineSize + lp100km + age, data = data)
residuals_mod2 <- rstandard(model2)
plot(model1$fitted.values, residuals_mod2, ylab = 'Standardardized residuals',
     xlab = 'Fitted values of log(price)')
abline(0,0, col = 'blue')

data$logprice <- log(data$price)

################################################################################
#Dropping the variables

data$X <- NULL
data$year <- NULL
data$mpg <- NULL
data$price <- NULL

str(data)
################################################################################
#Apply appropriate model selection techniques to choose a good set of explanatory 
#variables, e.g. best subset selection based on Akaikes Information Criterion 
#(AIC) or Mallow’s Cp statistic.

predictors <- as.data.frame(colnames(data[-9]))
colnames(predictors) <- "regressor"
SubSetResult <- vector()

for (predictorsCounter in 1:nrow(predictors)) {
  
  allMCombn <- combn(x = predictors$regressor, m = predictorsCounter)
  
  for (mCombnCounter in 1:ncol(allMCombn)) {
    
    modelPredictors <- allMCombn[,mCombnCounter]
    
    betaFormula <- character()
    
    for (subPredictorsCounter in 1:predictorsCounter) {
      
      betaFormula <- paste(betaFormula, modelPredictors[subPredictorsCounter], sep = "+")
      
    }
    
    formula <- paste("logprice ~",sub(".","",betaFormula))
    
    reg.lm <- lm(as.formula(formula), data)
    
    aic <- AIC(reg.lm)
    
    bic <- BIC(reg.lm)
    
    SubSetResult <- rbind(SubSetResult,c(formula,round(aic, digits = 2),round(bic, digits = 2), predictorsCounter))
    
  }
  
}

SubSetResult <- as.data.frame(SubSetResult)

colnames(SubSetResult) <- c("model","aic","bic","CountOfPredictors")

aicmin <- which.min(SubSetResult$aic)
bicmin <- which.min(SubSetResult$bic)


SubSetResult$aic[aicmin]

plot(SubSetResult$CountOfPredictors, SubSetResult$aic, 
     xlab = 'Number of covariates', ylab = 'AIC')
points(SubSetResult$CountOfPredictors[aicmin], SubSetResult$aic[aicmin], 
       col = 'red', cex = 2, pch = 20)

################################################################################
#Task 4
#Interpret the coefficients of the model and their statistical significance, 
#provide confidence intervals for the regression parameters, and evaluate the 
#goodness of fit.

#Fitting linear regression to the best subset of the covariates
modellm <- lm(logprice ~ model + transmission + mileage + fuelType + tax + 
                engineSize + lp100km + age,
              data = data)

#Checking the summary of the fitted model
summary(modellm)

#Confidence intervals for the parameters of the linear regression model
confint(modellm, level = 0.95)


#Model assumptions check for the fitted model
#Residual plot for the fitted values
residuals <- rstandard(modellm)
plot(modellm$fitted.values, residuals, ylab = 'Standardardized Residuals',
     xlab = 'Fitted values of log(price)')
abline(0,0, col = 'blue')

#QQplot fitted to the residuals
qqnorm(residuals)
qqline(residuals, col = 'red')

################################################################################





